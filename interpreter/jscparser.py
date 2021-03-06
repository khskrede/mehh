
from pypy.rlib.parsing.ebnfparse import parse_ebnf, make_parse_function
from pypy.rlib.parsing.tree import RPythonVisitor, Nonterminal, Node, Symbol, VisitError

import sys
import copy

import haskell as h
import module as m
import prim as p

# Load prim module
m.coremods["ghc-prim:GHC.Prim"] = p.prim

# EBNF for JSON parser
ebnf = """
    STRING: "\\"[^\\\\"]*\\"";
    NUMBER: "\-?(0|[1-9][0-9]*)(\.[0-9]+)?([eE][\+\-]?[0-9]+)?";
    IGNORE: " |\n";
    value: <STRING> | <NUMBER> | <object> | <array> | <"null"> | <"true"> | <"false">;
    object: ["{"] (pair [","])* pair ["}"] | ["{}"];
    array: ["["] (value [","])* value ["]"] | ["[]"];
    pair: STRING [":"] value;
    """

# Function takes a Core identifier, and returns the 
# package name, module name and a reference to either 
# a qvar, qdcon or qtycon 
def rewrite_id(ident):

    str_ = ident.replace("\"", "")

    if not (":" in str_ or "." in str_):
        return "", "", str_

    [package,l] = str_.split(":")
    package = package.replace("zm","-")

    mod = l.split("zi")

    module = ""
    for i in range(len(mod)-1):
        module += mod[i] + "."

    ident_ = mod[len(mod)-1].split(".")
    ref = ""
    if len(ident_) > 1 :
        ref = ident_[1]
        for i in range(1, len(mod)-1):
            ref += "." + ident_[i]

    module += ident_[0]

    return package, module, ref

class ForwardReference(object):
    def become(self, x):
        self.__class__ = x.__class__
        self.__dict__.update(x.__dict__)
    def replace_vars(self, subst, _):
        return self

# Kinds
lifted_kind = h.make_constructor(h.Symbol("*"),[])
unlifted_kind = h.make_constructor(h.Symbol("#"),[])
open_kind = h.make_constructor(h.Symbol("?"),[])

# Class for traversing the JSON datastructure and
# generate the Core' AST
class AST(RPythonVisitor):

    def __init__(self):
        self.mident=""

    def get_ast(self, tree):
        tree.visit(self)
        return m.coremods[self.mident]

    def visit_object(self, node):

        if len(node.children) > 0:
            left = node.children[0]
            type_ = left.children[0].additional_info.replace("\"","")

            for i in node.children:
                print i.children[0].additional_info,
            print ""

            # Module
            if type_ == "%module":

                # Create new module object
                info = left.children[1].additional_info.replace("\"","")
                package, mod, _ = rewrite_id(info)
                self.mident = package + ":" + mod
                m.coremods[self.mident] = m.CoreMod(self.mident)

                print "MODULE: ", self.mident

                # Get constructor definitions
                for tdef in node.children[1].children[1].children:
                    tdef.visit(self)

                # Get value definitions
                for vdef in node.children[2].children[1].children:
                    vdef.visit(self)

            # Data constructor
            elif type_ == "%data":

                # An algebraic type definition is turned into a
                # a function returning a Constructor

                mident = self.mident

                info = node.children[0].children[1].additional_info.replace("\"","")
                package, mod, name = rewrite_id(info)

                qtycon_id = mod + "." + name

                print "%data qtycon: ", qtycon_id

                tbinds = []
                for tbind in node.children[1].children[1].children :
                    tbinds.append(tbind.visit(self))

                cdefs = []
                for cdef in node.children[2].children[1].children :
                    cdefs.append(cdef.visit(self))

                constr = h.make_constructor(h.Symbol(qtycon_id), cdefs)

                add_qtycon(package, mod, qtycon_id, constr)

                #m.coremods[mident].qtycons[qtycon_id].become(constr)

                return constr

            # Newtype
            elif type_ == "%newtype":

                new_info = node.children[0].children[1].additional_info.replace("\"", "")
                new_package, new_mod, new_name = rewrite_id(new_info)
                new_mident = new_package + ":" + new_mod
                newtype_id = new_mod + "." + new_name

                qty_info = node.children[1].children[1].additional_info.replace("\"", "")
                qty_package, qty_mod, qty_name = rewrite_id(qty_info)
                qty_mident = qty_package + ":" + qty_mod
                qtycon_id = qty_mod + "." + qty_name

                tbinds = []
                for tbind in node.children[2].children[1].children :
                    tbinds.append(tbind.visit(self))

                ty = node.children[3].children[1].visit(self)

                const = h.make_constructor(h.Symbol(qtycon_id), [ty])

                m.coremods[new_mident].qtycons[newtype_id] = const
                m.coremods[new_mident].qtycons[qtycon_id] = const # WHYYYY ???

                return const

            # Constructor definition
            elif type_ == "qdcon" and len(node.children) == 3:

                # A constructor definition is turned into a
                # a function returning a constructor

                info = node.children[0].children[1].additional_info.replace("\"","")
                package, mod, name = rewrite_id(info)

                mident = package + ":" + mod
                qdcon_id = mod + "." + name

                tbinds = []
                for tbind in node.children[1].children[1].children :
                    tbinds.append(tbind.visit(self))

                atys = []
                for aty in node.children[2].children[1].children :
                    atys.append(aty.visit(self))

                constr = h.make_constructor(h.Symbol(qdcon_id), tbinds)

                m.coremods[self.mident].qdcons[qdcon_id] = constr
                #m.coremods[self.mident].qtycons[qdcon_id] = constr

                return constr

            # Recursive value definition
            elif type_ == "%rec":

                funcs = node.children[0].children[1].visit(self)
                recs = []
                for func in funcs:
                    assert isinstance(func, Function)
                    func.recursive=True
                    recs.append(func)

                return recs # TODO ! FIX ????

            # Value definition
            elif type_ == "qvar" and len(node.children) == 3:
                ident = node.children[0].children[1].additional_info
                package, mod, name = rewrite_id(ident)

                if mod == "":
                    package, mod = self.mident.split(":")

                mident = package + ":" + mod
                qvar_id = mod + "." + name

                print mident, qvar_id
                
                if mod == "":

                    # Currently using ForwardReference due to 
                    # recursive functions

                    m.coremods[self.mident].qvars[qvar_id] = ForwardReference()
                    exp = node.children[2].children[1].visit(self)
                    m.coremods[self.mident].qvars[qvar_id].become(exp)

                else:#if m.coremods.has_key(mident):
                    #qvar = get_external_qvar(package, mod, name)

                    m.coremods[mident].qvars[qvar_id] = ForwardReference()
                    exp = node.children[2].children[1].visit(self)
                    m.coremods[mident].qvars[qvar_id].become(exp)

                return exp

            # Variable
            elif type_ == "qvar" and len(node.children) == 1:
                info = node.children[0].children[1].additional_info
                ident = info.replace("\"","")
                package, mod, name = rewrite_id(ident)

                if mod == "":
                    package, mod = self.mident.split(":")

                    mident = package + ":" + mod
                    qvar_id = mod+"."+name

                    qvar = m.coremods[self.mident].qvars[qvar_id]
                else:
                    qvar_id = mod+"."+name

                    qvar = get_external_qvar(package, mod, qvar_id)

                return qvar

            # Type constructor
            elif type_ == "qtycon":

                info = node.children[0].children[1].additional_info.replace("\"","")
                package, mod, name = rewrite_id(info)

                if mod == "":
                    package, mod = self.mident.split(":")

                mident = package + ":" + mod
                qtycon_id = mod + "." + name

                if mod == "":
                    qtycon = m.coremods[self.mident].qtycons[qtycon_id]
                else:
                    qtycon = get_external_qtycon(package, mod, qtycon_id)

                return qtycon

            # Nested expression
            elif type_ == "exp":
                raise NotImplementedError


            # Application
            elif type_ == "aexp" and len(node.children) == 2:

                exp = node.children[0].children[1].visit(self)
                args = node.children[1].children[1].visit(self)

                aty = node.children[1].children[1].children[0].children[0].additional_info

                if aty == "aty":
                    # Type arguments have no semantic effect?
                    return exp
                else:
                    app = h.make_partial_app(exp, [args])
                    return app


            # Atomic expression
            elif type_ == "aexp" and len(node.children) == 1:
                aexp = node.children[0].children[1].visit(self)
                return aexp

            # Type argument
            elif type_ == "aty" and len(node.children) == 1:
                aty = node.children[0].children[1].visit(self)
                return aty

            # Symmetric coercion
            elif type_ == "%sym":
                aty = node.children[0].children[1].visit(self)
                return aty

            # Lambda abstraction
            elif type_ == "lambda":
                vbind = node.children[0].children[1].visit(self)
                exp = node.children[1].children[1].visit(self)

                func = h.make_partial_app(exp, [vbind])

                return func

            # Local definition
            elif type_ == "%let":
                raise NotImplementedError

            # Case expression
            elif type_ == "%case":

                # A case expression can be reduced to the application
                # of an expression to a function

                case = node.children[0].children[1].visit(self)
                exp = node.children[1].children[1].visit(self)
                of = node.children[2].children[1].visit(self)

                alts_ = node.children[3].children[1].children
                alts = []

                for alt in alts_:
                    alts.append(alt.visit(self))

                f = h.function("alts", alts)
                app = h.make_partial_app(f, [exp]) 

                return app

            # Default case-alternative
            elif type_ == "%_":

                f = node.children[0].children[1].visit(self)

                arg = h.Var("_")
                func = h.make_partial_app(f, arg)

                print "default"
                return ([h.Var("_")], func)

            # Literal case-alternative
            elif type_ == "lit" and len(node.children) == 2:

                pat = node.children[0].children[1].visit(self)
                exp = node.children[1].children[1].visit(self)

                func = h.make_partial_app(exp, pat)

                case = ([pat], func)
                return case

            # Constructor case-alternative
            elif type_ == "qdcon" and len(node.children) == 4:

                info = node.children[0].children[1].additional_info.replace("\"","")
                package, mod, name = rewrite_id(info)

                if mod == "":
                    package, mod = self.mident.split(":")

                mident = package + ":" + mod
                qdcon_id = mod + "." +name

                if mod == "":
                    qdcon = m.coremods[self.mident].qdcons[qdcon_id]
                else:
                    qdcon = get_external_qdcon(package, mod, qdcon_id)

                tbinds_ = node.children[1].children[1].children
                tbinds = []
                for tbind in tbinds_:
                    tbinds.append(tbind.visit(self))
                
                vbinds_ = node.children[2].children[1].children
                vbinds = []
                for vbind in vbinds_:
                    vbinds.append(vbind.visit(self))
                
                exp = node.children[3].children[1].visit(self)
                func = h.make_partial_app(exp, vbinds)

                return ([qdcon], func)

            # Type coercion
            elif type_ == "%cast":
                exp = node.children[0].children[1].visit(self)
                aty = node.children[1].children[1].visit(self)
                return exp

            # Expression note
            elif type_ == "%note":
                raise NotImplementedError

            # External reference
            elif type_ == "%external ccal":
                raise NotImplementedError

            # External reference (dynamic)
            elif type_ == "%dynexternal ccal":
                raise NotImplementedError

            # External label
            elif type_ == "%label":
                raise NotImplementedError

            # Literal
            elif type_ == "lit" and len(node.children) == 1:
                return node.children[0].children[1].visit(self)

            # Type application
            elif type_ == "bty" and node.children[1].children[0].additional_info == "\"aty\"":
                bty = node.children[0].children[1].visit(self)
                aty = node.children[1].children[1].visit(self)

                t_const = h.make_partial_app(bty, [aty])
                return t_const

            # Value binder (TODO: can probably be removed from JSCore language, see below)
            elif type_ == "vbind":
                return node.children[0].children[1].visit(self)

            # Value binder
            elif type_ == "var" and node.children[1].children[0].additional_info == "\"ty\"":
                var_name = node.children[0].children[1].additional_info
                info = var_name.replace("\"","")
                package, mod, name = rewrite_id(info)

                if mod == "":
                    package, mod = self.mident.split(":")

                mident = package + ":" + mod
                qvar_id = mod + "." + name

                if m.coremods[mident].qvars.has_key(qvar_id):
                    ty = m.coremods[mident].qvars[qvar_id]
                else:
                    ty = node.children[1].children[1].visit(self)

                var = h.Var(qvar_id)
                tyvar = h.make_partial_app( ty, [var] )
                m.coremods[mident].qvars[qvar_id] = tyvar

                return tyvar


            # Data constructor
            elif type_ == "qdcon" and len(node.children) == 1:
                info = node.children[0].children[1].additional_info.replace("\"","")
                package, mod, name = rewrite_id(info)

                if mod == "":
                    package, mod = self.mident.split(":")

                mident = package + ":" + mod
                qdcon_id = mod + "." + name

                if mod == "":
                    qdcon = m.coremods[self.mident].qdcons[qdcon_id]
                else:
                    qdcon = get_external_qdcon(package, mod, qdcon_id)

                return qdcon

            # Type binder (TODO: can probably be removed from JSCore language, see below)
            elif type_ == "tbind":
                return node.children[0].children[1].visit(self)

            # Type binder (implicitly of kind *)
            elif type_ == "tyvar" and len(node.children) == 1:
                info = node.children[0].children[1].additional_info.replace("\"","")
                package, mod, name = rewrite_id(info)

                if mod == "":
                    package, mod = self.mident.split(":")

                mident = package + ":" + mod
                ty_id = mod + "." + name

                kind = lifted_kind
                m.coremods[mident].qvars[ty_id] = kind

                return kind

            # Type binder (explicitly kinded)
            elif type_ == "tyvar" and len(node.children) == 2:

                info = node.children[0].children[1].additional_info.replace("\"","")
                package, mod, name = rewrite_id(info)

                if mod == "":
                    package, mod = self.mident.split(":")

                mident = package + ":" + mod
                ty_id = mod + "." + name

                kind = node.children[1].children[1].visit(self)
                m.coremods[mident].qvars[ty_id] = kind

                return kind


            # Atomic kind
            elif type_ == "akind" and len(node.children) == 1:

                akind = node.children[0].children[1]

                print "Atomic, kind: ", repr(akind)

                if akind.additional_info == "*":
                    kind = lifted_kind 

                elif akind.additional_info == "#":
                    kind = unlifted_kind

                elif akind.additional_info == "?":
                    kind = open_kind

                else:
                    kind = node.children[1].children[1].visit(self)

                return kind

            # Arrow kind
            elif type_ == "akind" and len(node.children) == 2:

                akind = node.children[0].children[1]

                print "Atomic, kind: ", repr(akind)

                if akind.additional_info == "*":
                    kind = lifted_kind 

                elif akind.additional_info == "#":
                    kind = unlifted_kind

                elif akind.additional_info == "?":
                    kind = open_kind

                else:
                    kind = node.children[1].children[1].visit(self)

                return kind

            # Type abstraction
            elif type_ == "%forall":
                
                info = node.children[0].children[1].children[0].children[1].additional_info
                forall_id = info.replace("\"","")

                ty = node.children[1].children[1].visit(self)
                forall = node.children[0].children[1].visit(self)

                abst = h.make_partial_app(ty, [forall])
                m.coremods[self.mident].qtycons[forall_id] = abst

                return abst

            else:
                raise NotImplementedError

    # There is really no reason to visit a pair
    def visit_pair(self, node):
        left = node.children[0].visit(self)
        right = node.children[1].visit(self)
        return (left, right)

    # Should really be no reason to visit an array,
    # their parents handle them
    def visit_array(self, node):
        l = []
        for child in node.children:
            l.append(child.visit(self))
        return l

    # Just return the string as a CString,
    # CStrings are unpacked by Haskell functions
    def visit_STRING(self, node):
        # TODO: fix this, we can't simply replace the \"
        str_ = node.additional_info.replace("\"","")
        print str_
        return h.CString(str_)

    # Figure out how to do this correctly
    def visit_NUMBER(self, node):
        number = node.additional_info.replace("\"","")
        num = m.coremods["ghc-prim:GHC.Prim"].qdcons["GHC.Prim.Intzh"]
        return h.make_partial_app(num, [number])


# Helper functions to fetch and add external references
def get_external_qvar(package, mod, name):
    mident = package + ":" + mod
    module = get_mod(package, mod) 

    if not module.qvars.has_key(name):
        module.qvars[name] = ForwardReference()

    return module.qvars[name]

def add_qvar(package, mod, name, qvar):
    mident = package + ":" + mod
    module = get_mod(package, mod) 

    if module.qvars.has_key(name):
        mod = module.qvars[name]
        if isinstance(mod, ForwardReference):
            mod.become(qvar)
        # Else, do nothing?
    else:
        module.qvars[name] = qvar

def get_external_qtycon(package, mod, name):
    mident = package + ":" + mod
    module = get_mod(package, mod)

    if not module.qtycons.has_key(name):
        module.qtycons[name] = ForwardReference()

    return module.qtycons[name]

def add_qtycon(package, mod, name, qtycon):
    mident = package + ":" + mod
    module = get_mod(package, mod) 

    if module.qtycons.has_key(name):
        mod = module.qtycons[name]
        if isinstance(mod, ForwardReference):
            mod.become(qtycon)
        # Else, do nothing?
    else:
        module.qtycons[name] = qtycon

def get_external_qdcon(package, mod, name):
    mident = package + ":" + mod
    module = get_mod(package, mod)

    if not module.qdcons.has_key(name):
        module.qdcons[name] = ForwardReference()

    return module.qdcons[name]

def add_qdcon(package, mod, name, qdcon):
    mident = package + ":" + mod
    module = get_mod(package, mod) 

    if module.qdcons.has_key(name):
        mod = module.qdcons[name]
        if isinstance(mod, ForwardReference):
            mod.become(qdcon)
        # Else, do nothing?
    else:
        module.qdcons[name] = qdcon

# Loads external JSCore file if necessary
def get_mod(package, mod):
    mident = package + ":" + mod

    if m.coremods.has_key(mident):
        module = m.coremods[mident]
        assert isinstance(module, m.CoreMod)
    else:
        path = "./ghc/libraries/" \
                + package + "/" \
                + mod.replace(".","/") + ".hcj"

        js = parse_js( path )
        ast = AST()
        newmod = ast.get_ast( js )
        module = newmod
        assert isinstance(module, m.CoreMod)

    assert isinstance(module, m.CoreMod)
    return module

# Parse JSCore file
def parse_js( path ):
    regexs, rules, ToAST = parse_ebnf(ebnf)
    parse = make_parse_function(regexs, rules, eof=True)

    doc = open(path, 'r').read()
    t = parse(doc)
    t = ToAST().transform(t)
    return t


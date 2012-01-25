
from pypy.rlib.parsing.ebnfparse import parse_ebnf, make_parse_function
from pypy.rlib.parsing.tree import RPythonVisitor, Nonterminal, Node, Symbol, VisitError
import sys
import haskell.haskell as hh
import ghc.prim
import module

import copy

import pdb

ebnf = """
    STRING: "\\"[^\\\\"]*\\"";
    NUMBER: "\-?(0|[1-9][0-9]*)(\.[0-9]+)?([eE][\+\-]?[0-9]+)?";
    IGNORE: " |\n";
    value: <STRING> | <NUMBER> | <object> | <array> | <"null"> | <"true"> | <"false">;
    object: ["{"] (pair [","])* pair ["}"] | ["{}"];
    array: ["["] (value [","])* value ["]"] | ["[]"];
    pair: STRING [":"] value;
    """


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


class AST(RPythonVisitor):

    modules = {}
    mident=""

    def __init__(self, mods=None):
        self.modules = mods

    def get_ast(self, tree):
        node = tree.visit(self)
        return node

    def visit_object(self, node):


        if len(node.children) > 0:
            left = node.children[0]
            type_ = left.children[0].additional_info

            if type_ == "\"%module\"":

                # Create new module object
                p, m, _ = rewrite_id(left.children[1].additional_info.replace("\"",""))
                self.mident = p+":"+m
                self.modules[self.mident] = module.module(self.mident)

                # Get constructor definitions
                for tdef in node.children[1].children[1].children:
                    p, m, f = rewrite_id(tdef.children[0].children[1].additional_info)
                    self.modules[self.mident].qtycons[ m+"."+f ] = ForwardReference()
                    ty = tdef.visit(self)

                # Get value definitions
                for vdef in node.children[2].children[1].children:
                    p = ""
                    m = ""
                    f = ""
                    if not vdef.children[0].children[0].additional_info == "\"%rec\"":
                        p, m, f = rewrite_id(vdef.children[0].children[1].additional_info)
                    else:
                        p, m, f = rewrite_id(vdef.children[0].children[1].\
                                  children[0].children[0].children[1].additional_info)
                    
                    # get vdefs
                    self.modules[self.mident].qvars[ m+"."+f ] = ForwardReference()
                    exp = vdef.visit(self)

                return self.modules[self.mident]

            elif type_ == "\"%data\"":

                package, mod, qtycon = rewrite_id(node.children[0].children[1].additional_info)
                self.modules[self.mident].qtycons[mod+"."+qtycon] = ForwardReference()

                tbinds = []
                for tbind in node.children[1].children[1].children :
                    tbinds.append(tbind.visit(self))

                cdefs = []
                for cdef in node.children[2].children[1].children :
                    cdefs.append(cdef.visit(self))

                const = hh.make_constructor(hh.Symbol(qtycon), tbinds)
                self.modules[self.mident].qtycons[mod+"."+qtycon].become(const)

                return const

            elif type_ == "\"%newtype\"":
                newtype = node.children[0].children[1].additional_info

                qtycon = node.children[1].children[1].additional_info


                tbinds = []
                for tbind in node.children[2].children[1].children :
                    tbinds.append(tbind.visit(self))

                ty = node.children[3].children[1].visit(self)

                return hh.make_constructor(hh.Symbol(qtycon), [ty])


            elif type_ == "\"qdcon\"" and len(node.children) == 3:
                package, mod, qdcon = rewrite_id(node.children[0].children[1].additional_info)
                mident = package + ":" + mod

                self.modules[self.mident].qdcons[qdcon] = ForwardReference()

                tbinds = []
                for tbind in node.children[1].children[1].children :
                    tbinds.append(tbind.visit(self))

                atys = []
                for aty in node.children[1].children[1].children :
                    atys.append(aty.visit(self))

                const = hh.make_constructor(hh.Symbol(qdcon), atys)

                self.modules[self.mident].qdcons[qdcon].become(const)

                return const



            elif type_ == "\"%rec\"":

                funcs = node.children[0].children[1].visit(self)
                recs = []
                for func in funcs:
                    assert isinstance(func, Function)
                    func.recursive=True
                    recs.append(func)

                return recs # TODO ! FIX ????

            elif type_ == "\"qvar\"" and len(node.children) == 3:
                ident = node.children[0].children[1].additional_info

                package, mod, func_name = rewrite_id(ident)
                mident = package + ":" + mod
                
                if mod == "":
                    self.modules[self.mident].qvars[func_name] = ForwardReference()
                    exp = node.children[2].children[1].visit(self)
                    self.modules[self.mident].qvars[func_name].become(exp)
                elif self.modules.has_key(mident):
                    self.modules[mident].qvars[mod+"."+func_name] = ForwardReference()
                    exp = node.children[2].children[1].visit(self)
                    self.modules[mident].qvars[mod+"."+func_name].become(exp)

                return exp

            elif type_ == "\"qvar\"" and len(node.children) == 1:
                c = node.children[0].children[1].additional_info
                ident = c.replace("\"","")
                package, mod, func_name = rewrite_id(ident)
                mident = package + ":" + mod
                func = 0

                if mod == "":
                    func = self.modules[self.mident].qvars[func_name]
                elif self.modules.has_key(mident):
                    func = self.modules[mident].qvars[mod+"."+func_name]
                else:
                    self.get_external(package, mod, func_name)
                    func = self.modules[mident].qvars[mod+"."+func_name]

                return func

            elif type_ == "\"qvar\"" and len(node.children) == 2:
                raise NotImplementedError


            elif type_ == "\"qtycon\"":

                c = node.children[0].children[1].additional_info

                ident = c.replace("\"","")
                package, mod, type_name = rewrite_id(ident)
                mident = package + ":" + mod
                type_ = 0

                if mod == "":
                    type_ = self.modules[self.mident].qtycons[mod+"."+type_name]
                elif self.modules.has_key(mident):
                    type_ = self.modules[mident].qtycons[mod+"."+type_name]
                else:
                    if package == "ghc-prim" and mod == "GHC.Prim":
                        modref = sys.modules["ghc.prim"]
                        type_ = getattr(modref, type_name)
                    else:
                        self.get_external(package, mod, type_name)
                        type_ = self.modules[mident].qtycons[mod+"."+type_name]

                return type_

            elif type_ == "\"exp\"":
                raise NotImplementedError





            elif type_ == "\"aexp\"" and len(node.children) == 2:

                exp = node.children[0].children[1].visit(self)
                args = node.children[1].children[1].visit(self)

                aty = node.children[1].children[1].children[0].children[0].additional_info

                if aty == "\"aty\"":
                    return exp
                else:
                    app = hh.make_application(exp, [args])
                   
                    if isinstance(exp, hh.Thunk):
                        return app
 
                    return app




            elif type_ == "\"aexp\"" and len(node.children) == 1:
                aexp = node.children[0].children[1].visit(self)
                return aexp

            elif type_ == "\"aty\"" and len(node.children) == 1:
                aty = node.children[0].children[1].visit(self)

                return aty

            elif type_ == "\"lambda\"":
                vbind = node.children[0].children[1].visit(self)
                exp = node.children[1].children[1].visit(self)

                #func = hh.function("lambda", [
                #            ([vbind],hh.make_application(exp,[vbind]))
                #            ])

                func = hh.make_application(exp, [vbind])

                return func

            elif type_ == "\"%let\"":
                raise NotImplementedError

            elif type_ == "\"%case\"":
                case = node.children[0].children[1].visit(self)
                exp = node.children[1].children[1].visit(self)
                of = node.children[2].children[1].visit(self)

                alts_ = node.children[3].children[1].children
                alts = []

                for alt in alts_:
                    alts.append(alt.visit(self))

                alts.reverse()

                pdb.set_trace()
                f = hh.function("alts", alts)
                app = hh.make_application(f, exp) 

                return app

            elif type_ == "\"%_\"":
                f = node.children[0].children[1].visit(self)
                return ([hh.Var("_")], f)

            elif type_ == "\"%cast\"":
                raise NotImplementedError

            elif type_ == "\"%note\"":
                raise NotImplementedError

            elif type_ == "\"%external ccal\"":
                raise NotImplementedError

            elif type_ == "\"%dynexternal ccal\"":
                raise NotImplementedError

            elif type_ == "\"%label\"":
                raise NotImplementedError

            elif type_ == "\"lit\"" and len(node.children) == 1:
                return node.children[0].children[1].visit(self)

            elif type_ == "\"lit\"" and len(node.children) == 2:
                pat = node.children[0].children[1].visit(self)
                exp = node.children[1].children[1].visit(self)

                case = ([pat], exp)
                return case

            elif type_ == "\"bty\"" and node.children[1].children[0].additional_info == "\"aty\"":
                bty = node.children[0].children[1].visit(self)
                aty = node.children[1].children[1].visit(self)

                t_app = hh.make_application(bty, [aty])
                #t_app = bty.apply(aty)
                return t_app

            elif type_ == "\"vbind\"":
                return node.children[0].children[1].visit(self)

            elif type_ == "\"var\"" and node.children[1].children[0].additional_info == "\"ty\"":
                var_name = node.children[0].children[1].additional_info
                var_name = var_name.replace("\"","")
                ty = node.children[1].children[1].visit(self) # TODO, use this?
                self.modules[self.mident].qvars[var_name] = ty
                return ty


            elif type_ == "\"qdcon\"" and len(node.children) == 4:
                package, mod, name = rewrite_id(node.children[0].children[1].additional_info)
                mident = package + ":" + mod

                if mod == "":
                    qdcon = self.modules[self.mident].qdcons[name]
                elif self.modules.has_key(mident):
                    qdcon = self.modules[mident].qdcons[name]
                else:
                    self.get_external(package, mod, name)
                    qdcon = self.modules[mident].qdcons[name]


                tbinds_ = node.children[1].children[1].children
                tbinds = []
                for tbind in tbinds_:
                    tbinds.append(tbind.visit(self))
                
                vbinds_ = node.children[2].children[1].children
                vbinds = []
                for vbind in vbinds_:
                    vbinds.append(vbind.visit(self))
                
                print "qdcon: ", repr(qdcon)
                print "vbinds: ", repr(vbinds) 

                exp = node.children[3].children[1].visit(self)
                const_alt = hh.make_application(qdcon, vbinds)

                # TODO ?
                return ([const_alt], exp)

            elif type_ == "\"qdcon\"" and len(node.children) == 1:
                package, mod, name = rewrite_id(node.children[0].children[1].additional_info)
                mident = package + ":" + mod

                if mod == "":
                    return self.modules[self.mident].qdcons[name]
                elif self.modules.has_key(mident):
                    return self.modules[mident].qdcons[name]
                else:
                    self.get_external(package, mod, name)
                    return self.modules[mident].qdcons[name]

            elif type_ == "\"tbind\"":
                return node.children[0].children[1].visit(self)

            elif type_ == "\"tyvar\"" and len(node.children) == 1:
                return hh.Var(node.children[0].children[1].additional_info)

            elif type_ == "\"tyvar\"" and len(node.children) == 2:
                return hh.Var(node.children[0].children[1].additional_info)

            else:
                raise NotImplementedError

    def visit_pair(self, node):
        left = node.children[0].visit(self)
        right = node.children[1].visit(self)
        return (left, right)

    def visit_array(self, node):
        l = []
        for child in node.children:
            l.append(child.visit(self))
        return l

    def visit_STRING(self, node):
        # TODO: fix this, we can't simply replace
        str_ = node.additional_info.replace("\"","")
        return hh.CString(node.additional_info)

    def visit_NUMBER(self, node):
        number = node.additional_info
        test = importlib.import_module("ghc.libraries.ghc-prim.GHC.Prim")
        num = test.Intzh
        return num((node.additional_info.replace("\"","")))

    def get_external(self, package, mod, name):

        mident = package + ":" + mod
        primitives = ["ghc-prim:GHC.Prim"]

        print "loading: ", mident

        if mident in primitives:
            modref = sys.modules["ghc.prim"]
            return getattr(modref, name)
        else:
            path = "./ghc/libraries/" \
                    + package + "/" \
                    + mod.replace(".","/") + ".hcj"

            self.modules[mident] = module.module(mident)
            js = parse_js( path )
            ast = AST(self.modules)
            newmod = ast.get_ast( js )
            self.modules[mident].loadmod(newmod)

def parse_js( path ):
    regexs, rules, ToAST = parse_ebnf(ebnf)
    parse = make_parse_function(regexs, rules, eof=True)

    doc = open(path, 'r').read()
    t = parse(doc)
    t = ToAST().transform(t)
    return t


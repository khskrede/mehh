
from pypy.rlib.parsing.ebnfparse import parse_ebnf, make_parse_function
from pypy.rlib.parsing.tree import RPythonVisitor, Nonterminal, Node, Symbol, VisitError
import sys
import haskell.haskell as hh
import module

import copy

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

#    str_ = ident.replace("\"","").replace("zi",".")
#    l = str_.split(":")
#    l2 = l[1].split(".")
#    package=""
#    mod=""
#    func=""
#    if len(l2) == 2:
#        package = ""
#        mod = l[0]+":"+l2[0]
#        func = l2[0]+"."+l2[1]

#    elif len(l2) == 3:
#        print "l2: ", repr(l2)
#        package = l2[0]
#        mod = l2[1]
#        func = l2[2].replace("zh", "")

#    return package, mod, func

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
    ref = ident_[1]
    for i in range(1, len(mod)-1):
        ref += "." + ident_[i]

    module += ident_[0]

    print "---------"
    print package
    print module
    print ref

    return package, module, ref


class ForwardReference(object):
    def become(self, x):
        self.__class__ = x.__class__
        self.__dict__.update(x.__dict__)
    def replace_vars(self, subst, _):
        return self


def get_external(package, mod, name):
    package_name = "ghc.libraries." + package + "." + mod
    __import__(package_name)
    mod = sys.modules[package_name]
    ext = getattr(mod, name)
    return ext


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
                self.mident = left.children[1].additional_info.replace("\"","")
                
                mod = module.module()
                mod.name = self.mident
                self.modules[self.mident] = mod

                print self.mident
                print "***********"

                for vdef in node.children[2].children[1].children:
                    p = ""
                    m = ""
                    f = ""
                    if not vdef.children[0].children[0].additional_info == "\"%rec\"":
                        p, m, f = rewrite_id(vdef.children[0].children[1].additional_info)
                    else:
                        p, m, f = rewrite_id(vdef.children[0].children[1].\
                                  children[0].children[0].children[1].additional_info)
                        
                    mod.qvars[ m+"."+f ] = ForwardReference()
                    func = vdef.visit(self)
                    mod.qvars[ m+"."+f ].become(func)
                    print m+"."+f
                    print "++++++++++++++"
                return mod

            elif type_ == "\"%data\"":
                raise NotImplementedError

            elif type_ == "\"%newtype\"":
                raise NotImplementedError

            elif type_ == "\"qdcon\"" and len(node.children) == 3:
                raise NotImplementedError

            elif type_ == "\"%rec\"":
                func = node.children[0].children[1].visit(self)
                func.recursive=True
                return func

            elif type_ == "\"qvar\"" and len(node.children) == 3:
                name = node.children[0].children[1].additional_info

                # Skip result/return type ?
                t_args = node.children[1].children[1].children[0].children[1].visit(self)
                #t_args = node.children[1].children[1].visit(self)
                args = []

                while type(t_args) == tuple:
                    args.append(t_args[1])
                    t_args = t_args[0]

                print len(args)
                print repr(args)

                #args = hh.constr(t_args[0],t_args[1])

                exp = node.children[2].children[1].visit(self)
                #func = hh.make_application( exp, [args] )
                func = hh.function( name, [(args, exp)], recursive=False )

                return func

            elif type_ == "\"qvar\"" and len(node.children) == 1:
                c = node.children[0].children[1].additional_info
                print c
                print "ååååååååå"
                ident = c.replace("\"","")
                package, mod, func_name = rewrite_id(ident)
                func = 0
                if not (package == "main" or package == ""):
                    func = get_external(package, mod, func_name)
                else:
                    if mod == "":
                        func = self.modules[self.mident].qvars[func_name]
                    else:
                        mident = package + ":" + mod
                        func = self.modules[mident].qvars[mod+"."+func_name]

                print "Wakka"
                print func_name

                print repr(func)

                return func

            elif type_ == "\"qvar\"" and len(node.children) == 2:
                raise NotImplementedError




            elif type_ == "\"qtycon\"":

                c = node.children[0].children[1].additional_info

                print "-----"

                print c

                ident = c.replace("\"","")
                package, mod, type_name = rewrite_id(ident)
                type_ = 0

                print package
                print mod
                print type_name

                if not package == "main":
                    type_ = get_external(package, mod, type_name)
                else:
                    mident = package + ":" + mod
                    type_ = self.modules[mident].tdefg[mod+"."+type_name]

                #print "package name: \"", package_name, "\""
                #print "type name: \"", type_name, "\""

                return type_

            elif type_ == "\"exp\"":
                raise NotImplementedError





            elif type_ == "\"aexp\"" and len(node.children) == 2:

                app = 0
                func = node.children[0].children[1].visit(self)
                args = node.children[1].children[1].visit(self)

#                type1_ = node.children[0].children[1].children[0].children[0].additional_info

                type2_ = node.children[1].children[1].children[0].children[0].additional_info
                if type2_ == "\"aty\"":
                    # Type arguments have no operational effect?
                    return func
#                elif type1_ == "\"qdcon\"":
#                    return hh.make_constructor( func, [args])
                else:
                    return hh.make_application( func, [args] )





            elif type_ == "\"aexp\"" and len(node.children) == 1:
                return node.children[0].children[1].visit(self)

            elif type_ == "\"aty\"" and len(node.children) == 1:
                return node.children[0].children[1].visit(self)

            elif type_ == "\"lambda\"":
                var = node.children[0].children[1].visit(self)
                exp = node.children[1].children[1].visit(self)

                return hh.function( name, [(var, exp)], recursive=False )

            elif type_ == "\"%let\"":
                raise NotImplementedError

            elif type_ == "\"%case\"":

                exp = node.children[1].children[1].visit(self)
                of = node.children[2].children[1].visit(self)

                alts_ = node.children[3].children[1].children
                alts = []
                print "MMMMMMMMMMMMMMMMMMMMMMM"
                for alt in alts_:
                    alts.append(alt.visit(self))
                    print alts
                print "MMMMMMMMMMMMMMMMMMMMMMM"
                f = hh.function("case", alts)

                return hh.make_application(f, exp)

            elif type_ == "\"%_\"":
                # Default case expression
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
                return hh.CString(node.children[0].children[1].additional_info)

            elif type_ == "\"lit\"" and len(node.children) == 2:
                pat = node.children[0].children[1].visit(self)
                body = node.children[1].children[1].visit(self)

                case = ([pat], body)

                #print "mmmmmmmmmmmmmmmmmmmmmmmm"
                #print case
                #print "mmmmmmmmmmmmmmmmmmmmmmmm"
                return case

            elif type_ == "\"bty\"" and node.children[1].children[0].additional_info == "\"aty\"":
                return (node.children[0].children[1].visit(self), 
                       node.children[1].children[1].visit(self))

            elif type_ == "\"vbind\"":
                return node.children[0].children[1].visit(self)

            elif type_ == "\"var\"":
                var_name = node.children[0].children[1].additional_info
                var_name = var_name.replace("\"","")
                ty = node.children[1].children[1].visit(self) # TODO, use this?
                var = hh.Var(var_name)
                self.modules[self.mident].qvars[var_name] = var
                print var_name
                print "øøøøøøøøøøøø"
                return var

            elif type_ == "\"qdcon\"" and len(node.children) == 4:
                qdcon = node.children[0].children[1].visit(self)

                tbinds_ = node.children[1].children[1].children
                tbinds = []
                for tbind in tbinds_:
                    tbinds.append(tbind.visit(self))
                
                vbinds_ = node.children[2].children[1].children
                vbinds = []
                for vbind in vbinds_:
                    vbinds.append(vbind.visit(self))
 
                exp = node.children[3].children[1].visit(self)

                # TODO ?
                return (vbinds, exp)
                #return hh.function("", (vbinds, exp))

            elif type_ == "\"qdcon\"" and len(node.children) == 1:
                package, mod, name = rewrite_id(node.children[0].children[1].additional_info)
                if not (package == "main" or package == ""):
                    return get_external(package, mod, name)
                else:
                    return self.modules[self.mident].qvars[name]
            else:
                print type_
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
        # all: "
        str_ = node.additional_info.replace("\"","")
        return hh.CString(node.additional_info)

    def visit_NUMBER(self, node):
        return hh.Integer((node.additional_info.replace("\"","")))


def parse_js( path ):
    regexs, rules, ToAST = parse_ebnf(ebnf)
    parse = make_parse_function(regexs, rules, eof=True)

    doc = open(path, 'r').read()
    t = parse(doc)
    t = ToAST().transform(t)
    return t

def print_dot( path ):
    regexs, rules, ToAST = parse_ebnf(ebnf)
    parse = make_parse_function(regexs, rules, eof=True)


    f = open(path, 'r')
    doc = f.read()
    t = parse(doc)

    t = ToAST().transform(t)

    print "digraph parsed {"
    print "\n".join(list(t.dot()))
    print "}"


# If run directly, generate dot file
if __name__ == "__main__":
    if len(sys.argv) > 1:

        path = sys.argv[1]
        regexs, rules, ToAST = parse_ebnf(ebnf)
        parse = make_parse_function(regexs, rules, eof=True)


        f = open(path, 'r')
        doc = f.read()
        t = parse(doc)

        t = ToAST().transform(t)

        print "digraph parsed {"
        print "\n".join(list(t.dot()))
        print "}"

    else:
        print "missing file path"

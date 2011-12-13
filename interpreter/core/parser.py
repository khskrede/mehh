
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

x = hh.Var("x")


def rewrite_id(ident):

    str_ = ident.replace("\"","").replace("zi",".")
    l = str_.split(":")
    l2 = l[1].split(".")
    package=""
    mod=""
    func=""
    if len(l2) == 2:
        package = ""
        mod = l[0]+":"+l2[0]
        func = l2[0]+"."+l2[1]

    elif len(l2) == 3:
        print "l2: ", repr(l2)
        package = l2[0]
        mod = l2[1]
        func = l2[2].replace("zh", "")

    return package, mod, func


class AST(RPythonVisitor):

    modules = {}
    _vars = {}

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
                mident = left.children[1].additional_info.replace("\"","")

                mod = module.module()
                mod.name = mident
                self.modules[mident] = mod

                for vdef in node.children[2].children[1].children:
                    func = vdef.visit(self)
                    p, m, f = rewrite_id(vdef.children[0].children[1].additional_info)
                    mod.vdefg[ f ] = func

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
                ident = c.replace("\"","")
                package, mod, func_name = rewrite_id(ident)
                func = 0
                if not package == "":
                    package_name = "haskell.packages." + package + "." + mod
                    __import__(package_name)
                    mod = sys.modules[package_name]
                    func = getattr(mod, func_name)
                else:
                    func = self.modules[mod].vdefg[func_name]

                print "Wakka"
                print func_name

                print repr(func)

                return func

            elif type_ == "\"qvar\"" and len(node.children) == 2:
                raise NotImplementedError








            elif type_ == "\"qtycon\"":

                c = node.children[0].children[1].additional_info
                ident = c.replace("\"","")
                package, mod, type_name = rewrite_id(ident)
                type_ = 0
                if not package == "":
                    package_name = "haskell.packages." + package + "." + mod
                    __import__(package_name)
                    mod = sys.modules[package_name]
                    type_ = getattr(mod, type_name)
                else:
                    type_ = self.modules[mod].tdefg[func_name]

                #print "package name: \"", package_name, "\""
                #print "type name: \"", type_name, "\""

                return type_

            elif type_ == "\"exp\"":
                raise NotImplementedError

            elif type_ == "\"aexp\"" and len(node.children) == 2:

                app = 0
                func = node.children[0].children[1].visit(self)
                args = node.children[1].children[1].visit(self)

                type2_ = node.children[1].children[1].children[0].children[0].additional_info
                if type2_ == "\"aty\"":
                    # Type arguments have no operational effect?
                    return func
                else:
                    return hh.make_application( func, [args] )

            elif type_ == "\"aexp\"" and len(node.children) == 1:
                return node.children[0].children[1].visit(self)

            elif type_ == "\"aty\"" and len(node.children) == 1:
                return node.children[0].children[1].visit(self)

            elif type_ == "\"lambda\"":
                raise NotImplementedError

            elif type_ == "\"%let\"":
                raise NotImplementedError

            elif type_ == "\"%case\"":
                raise NotImplementedError

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

            elif type_ == "\"lit\"":
                return hh.CString(node.children[0].children[1].additional_info)

            elif type_ == "\"bty\"" and node.children[1].children[0].additional_info == "\"aty\"":
                return (node.children[0].children[1].visit(self), 
                       node.children[1].children[1].visit(self))

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
        return hh.CString(node.additional_info)

    def visit_NUMBER(self, node):
        raise NotImplementedError


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

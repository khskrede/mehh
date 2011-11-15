
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
    l = ident.split("\"")[1].split(":")[1].split("zi")
    package = l[0]
    l2 = l[1].split(".")
    mod = l2[0]
    func = l2[1]
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

        # We can figure out what kind of construct 
        # we are parsing here

        if len(node.children) > 0:
            left = node.children[0]
            type_ = left.children[0].additional_info

            # Module
            if type_ == "\"%module\"":
                mident = left.children[1].additional_info
                print "Module(", mident, "):"
                mod = module.module()
                mod.name = mident

                #for vdef in node.children[2].children[1].children:
                #    func = vdef.visit(self)
                #    mod.vdefg.append( func )

                vdef = node.children[2].children[1].children[0]
                func = vdef.visit(self)
                mod.vdefg.append( func )

                self.modules[mident] = mod
                return mod

            # Algebraic type

            # tdefg
            elif type_ == "\"%data\"":
                print "DATA:"

            # Newtype

            # tdefg
            elif type_ == "\"%newtype\"":
                print "NEWTYPE:"

            # Constr defn.

            # cdef
            elif type_ == "\"qdcon\"" and len(node.children) == 3:
                print "Constructor definition:"

            # Value defn.

            # Vdefg
            elif type_ == "\"%rec\"":
                print "Recursive value definition:"

            # Vdef
            elif type_ == "\"qvar\"" and len(node.children) == 3:

                name = node.children[0].children[1].additional_info
                print "Nonrec(", name, "):"

                args = [x]
                if name == "\"main:Main.main\"":
                    args = []

                exp_child = node.children[2].children[1]
                exp = exp_child.visit(self)

                func = hh.function( name, [(args, exp)], recursive=False )

                return func

            # Atomic expression

            # aexp
            elif type_ == "\"exp\"":
                print "Expression:"
                #return ([x], x)

            # Expression

            # application
            elif type_ == "\"aexp\"":

                ident = left.children[1].visit(self)
                package, mod, func_name = rewrite_id(ident)

                f = 0
                if func_name == "putStrLn":
                    import haskell.packages.System.IO as p
                    f = p.putStrLn
                else:
                    import haskell.packages.GHC.Base as p
                    f = p.unpackCString

                package_name = ("haskell.packages." + package + "." + mod)
                print package_name

                imp = __import__(package_name)
                print repr(imp)

                #func = getattr(imp, func_name)
                func = f

                args = node.children[1].children[1].visit(self)

                print "App()"

                app = hh.make_application( func, [args] )
                return app

            # abstraction
            elif type_ == "\"lambda\"":
                print "Lambda abstraction:"

            elif type_ == "\"%let\"":
                print "Local definition:"

            elif type_ == "\"%case\"":
                print "Case expression:"

            elif type_ == "\"%cast\"":
                print "Type coercion:"

            elif type_ == "\"%note\"":
                print "Note:"

            elif type_ == "\"%external ccal\"":
                print "external reference:"

            elif type_ == "\"%dynexternal ccal\"":
                print "external reference (dynamic):"

            elif type_ == "\"%label\"":
                print "external label:"

            elif type_ == "\"Lstring\"":
                #return node.children[0].children[1].visit(self)
                return hh.CString("Hello World!")

            # .... TODO

            # Basic type

            # type application
            elif type_ == "\"bty\"" and node.children[1].children[0].additional_info == "\"aty\"":
                print "type application: meh!"

            else:
                print type_

        #for child in node.children:
        #    child.visit(self)

    def visit_pair(self, node):
        left = node.children[0]
        right = node.children[1]

        return (left.visit(self), right.visit(self))

    def visit_array(self, node):
        l = []
        for child in node.children:
            l.append(child.visit(self))
        return l

    def visit_STRING(self, node):
        print node.symbol, ": ", node.additional_info, ": ", node.token
        return node.additional_info

    def visit_NUMBER(self, node):
        print "number: ", node.symbol


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

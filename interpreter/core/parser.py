
from pypy.rlib.parsing.ebnfparse import parse_ebnf, make_parse_function
from pypy.rlib.parsing.tree import RPythonVisitor, Nonterminal, Node, Symbol, VisitError
import sys
from haskell import haskell
import haskell.syntax
import module

ebnf = """
    STRING: "\\"[^\\\\"]*\\"";
    NUMBER: "\-?(0|[1-9][0-9]*)(\.[0-9]+)?([eE][\+\-]?[0-9]+)?";
    IGNORE: " |\n";
    value: <STRING> | <NUMBER> | <object> | <array> | <"null"> | <"true"> | <"false">;
    object: ["{"] (pair [","])* pair ["}"] | ["{}"];
    array: ["["] (value [","])* value ["]"] | ["[]"];
    pair: STRING [":"] value;
    """

class AST(RPythonVisitor):

    modules = {}
    _vars = {}

    x = haskell.haskell.Var("x")

    def __init__(self, mods):
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
                print "MODULE!"

                mident = left.children[1].additional_info
                mod = module.module()
                mod.name = mident

                vdefg = []
                for vdef in node.children[2].children:
                    vdefg.append( vdef.visit(self) )
                mod.vdefg = vdefg

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
                print "Nonrecursive value definition: Function(", name, " ++ )"

                exp_child = node.children[2].children[1]
                print repr(exp_child.visit(self))
                func = haskell.haskell.function( name, [exp_child.visit(self)], recursive=False )

                return func

            # Atomic expression

            # aexp
            elif type_ == "\"exp\"":
                x = haskell.haskell.Var("x")
                print "Expression:"
                return ([x], x)

            # Expression

            # application
            elif type_ == "\"aexp\"":
                x = haskell.haskell.Var("x")
                print "Atomic expression:"
                return ([x], x)

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


            # .... TODO

            # Basic type

            # type application
            elif type_ == "\"bty\"" and node.children[1].children[0].additional_info == "\"aty\"":
                print "type application:"

            else:
                print type_

        for child in node.children:
            child.visit(self)

    def visit_pair(self, node):
        left = node.children[0]
        right = node.children[1]

        left.visit(self)
        right.visit(self)

    def visit_array(self, node):
        for child in node.children:
            child.visit(self)

    def visit_STRING(self, node):
        #print node.symbol, ": ", node.additional_info, ": ", node.token
        #print node.additional_info
        pass

    def visit_NUMBER(self, node):
        #print node.symbol
        pass


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

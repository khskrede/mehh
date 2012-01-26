#!/usr/bin/python2.7

from pypy.rlib.parsing.ebnfparse import parse_ebnf, make_parse_function
from pypy.rlib.parsing.tree import RPythonVisitor, Nonterminal, Node, Symbol, VisitError
import sys


ebnf = """
    STRING: "\\"[^\\\\"]*\\"";
    NUMBER: "\-?(0|[1-9][0-9]*)(\.[0-9]+)?([eE][\+\-]?[0-9]+)?";
    IGNORE: " |\n";
    value: <STRING> | <NUMBER> | <object> | <array> | <"null"> | <"true"> | <"false">;
    object: ["{"] (pair [","])* pair ["}"] | ["{}"];
    array: ["["] (value [","])* value ["]"] | ["[]"];
    pair: STRING [":"] value;
    """

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

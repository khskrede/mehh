#!/usr/bin/python2.7

import sys
import jscparser as c
import haskell as p
import module as m

def interp(path):
    js = c.parse_js( path )
    ast = c.AST()
    mod = ast.get_ast( js )

    main = m.coremods["main:Main"].qvars["ZCMain.main"]

    p.evaluate_hnf( main )

def jitpolicy(self):
    from pypy.jit.codewriter.policy import JitPolicy
    return JitPolicy()

def entry_point(args):
    if len(args) > 1:
        interp(args[1])
        return 0
    else:
        print "missing file path"
        return 1

def target(*args):
    return entry_point, None

if __name__ == '__main__':
    entry_point(sys.argv)


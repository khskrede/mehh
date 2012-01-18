import sys
import core.parser as cp
import haskell.haskell as hh

modules = {}

# TEST!
import importlib
test = importlib.import_module("ghc.libraries.ghc-prim.GHC.Prim")
print repr(test)

def get_id():
    x = hh.Var("x")
    id_ = hh.function("id", [([x],x)])
    return id_

def get_print():
    import haskell.packages.System.IO as p
    print repr(p)
    func = p.putStrLn
    print repr(func)
    return func

def interp(path):
    js = cp.parse_js( path )
    ast = cp.AST( modules )

    mod = ast.get_ast( js )

    main = modules["main:Main"].qvars["ZCMain.main"]

    #main = modules["main:Main"].qvars["Main.fac"]

    fac = test.Intzh(5) 

    hh.evaluate_hnf( main )

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
    import sys
    entry_point(sys.argv)


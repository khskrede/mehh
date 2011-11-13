import sys
import core.parser
import haskell.haskell

modules = {}

x = haskell.haskell.Var("x")
id_ = haskell.haskell.function("id", [([x],x)])


def interp(path):
    js = core.parser.parse_js( path )
    ast = core.parser.AST( modules )

    mod = ast.get_ast( js )

    print len(mod.vdefg)

    exp = modules.values()[0].vdefg[0]
    exp = mod.vdefg[0]

    print repr(exp)

    print haskell.haskell.evaluate_hnf( \
        haskell.haskell.make_application( \
            exp, \
            [haskell.haskell.Integer(3)] ) )

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


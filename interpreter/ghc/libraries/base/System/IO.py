

# TODO

import haskell.haskell as hh

# TEST!
import importlib
unit = importlib.import_module("ghc.libraries.ghc-prim.GHC.Unit")
types = importlib.import_module("ghc.libraries.ghc-prim.GHC.Types")

IOZ0T = hh.make_application(types.IO, [unit.Z0T])

@hh.expose_primitive(1)
def putStrLn( args ):
    print args[0].value

    return args[0] #IOZ0T


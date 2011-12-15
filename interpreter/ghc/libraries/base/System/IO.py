

# TODO

import haskell.haskell as hh

@hh.expose_primitive(1)
def putStrLn( args ):
    print args[0].value
    return args[0]


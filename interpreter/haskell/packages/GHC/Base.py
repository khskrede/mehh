

import haskell.haskell as hh

@hh.expose_primitive(1)
def unpackCString( args ):
    return args[0]


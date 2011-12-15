

import haskell.haskell as hh

@hh.expose_primitive(1)
def unpackCStringzh( args ):
    return args[0]


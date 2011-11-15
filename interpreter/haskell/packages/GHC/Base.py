

from haskell import haskell

@haskell.expose_primitive(1)
def unpackCString( args ):
    return args[0]


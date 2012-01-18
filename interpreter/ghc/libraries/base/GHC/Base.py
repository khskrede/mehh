

import haskell.haskell as hh

@hh.expose_primitive(1)
def unpackCStringzh( args ):
    return args[0]


# (++)

@hh.expose_primitive(2)
def zpzp( args ):
    a = args[0]
    b = args[1]

    return a

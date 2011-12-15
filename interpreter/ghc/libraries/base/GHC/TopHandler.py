
#TODO

import haskell.haskell as hh

@hh.expose_primitive(1)
def runMainIO(args):
    return hh.make_application(args[0], [])

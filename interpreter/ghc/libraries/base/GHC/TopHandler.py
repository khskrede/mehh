
#TODO

import haskell.haskell as hh

# IO a -> IO a

@hh.expose_primitive(1)
def runMainIO(args):
    a = args[0]
    # clean up ?
    return a 

#main = hh.Var("main")
#runMainIO = hh.function("runMainIO", [([main], main)])


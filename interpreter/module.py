
import haskell

class coremod():
    name = "" 
    qvars = {}
    qtycons = {}
    qdcons = {}

    def __init__(self, mident):
        self.name = mident
        self.qvars = {}

    def loadmod(self,mod):
        for (key, value) in mod.qvars.items():
            self.qvars[key] = value
        for (key, value) in mod.qtycons.items():
            self.qtycons[key] = value
        for (key, value) in mod.qdcons.items():
            self.qdcons[key] = value

coremods = {}


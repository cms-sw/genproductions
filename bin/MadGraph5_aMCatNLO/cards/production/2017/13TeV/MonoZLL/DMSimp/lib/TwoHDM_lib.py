import copy
from card_lib import Parameter,ParameterPoint

class TwoHDMParameterPoint(ParameterPoint):
    '''Struct to hold parameters for single 2HDM dataset'''
    def __init__(self,other=None):
        if other:
            self.parameters = copy.deepcopy(other.parameters)
        else:
            tmp = []
            tmp.append( Parameter("mh1") )   # h(125)
            tmp.append( Parameter("mh2") )   # H
            tmp.append( Parameter("mhc") )   # H+-
            tmp.append( Parameter("mh3") )   # A
            tmp.append( Parameter("mh4") )   # a
            tmp.append( Parameter("tanbeta") ) # tan(beta)
            tmp.append( Parameter("lam3") )    # lambda3
            tmp.append( Parameter("laP1") )    # lambda3
            tmp.append( Parameter("laP2") )    # lambda3
            tmp.append( Parameter("sinp") )    # lambda3
            tmp.append( Parameter("sinbma") )    # lambda3
            tmp.append( Parameter("gPXd") )    # lambda3
            tmp.append( Parameter("Mxd") )    # lambda3
    
            self.parameters = {}
            for par in tmp:
                self.parameters[par.name] = par

    def get_dataset_identifier(self,tag=""):
        s = "Pseudoscalar2DHM_MonoZLL_"
        if(len(tag)): s = s + tag + "_"
        s = s + "_".join(["{PARAMETER}-{VALUE}".format(PARAMETER=p.name,VALUE=p.value) for p in self.parameters.values() if p.printme])
        s = s.replace(".","p")
        return s

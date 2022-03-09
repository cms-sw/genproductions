from card_lib import Parameter,ParameterPoint


class ScalarFirstGenLQ_ParameterPoint(ParameterPoint):
    '''Struct to hold parameters for single ScalarFirstGenLQ dataset'''
    def __init__(self,other=None):
        if other:
            self.parameters = copy.deepcopy(other.parameters)
        else:
            tmp = []
            tmp.append( Parameter("Mlq") ) #Mass
            tmp.append( Parameter("Ylq") ) #Coupling

            self.parameters = {}
            for par in tmp:
                self.parameters[par.name] = par

    def get_dataset_identifier(self,tag=""):
        s = "ScalarFirstGenLQ_"
        #if(len(tag)): s = s + tag + "_"
        s = s + "_".join(["{PARAMETER}-{VALUE}".format(PARAMETER=p.name,VALUE=p.value) for p in self.parameters.values() if p.printme])
        s = s.replace(".","p")
        return s

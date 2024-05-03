import copy
from card_lib import Parameter,ParameterPoint

class DMSimp_Spin0_ParameterPoint(ParameterPoint):
    '''Struct to hold parameters for single DMSimp spin 0 dataset'''
    def __init__(self,other=None):
        if other:
            self.parameters = copy.deepcopy(other.parameters)
        else:
            tmp = []
            tmp.append( Parameter("mxd") )
            tmp.append( Parameter("MY0") )

            # Pseudoscalar Y0-Fermion couplings
            tmp.append( Parameter("gpxd") )
            tmp.append( Parameter("gpd11") )
            tmp.append( Parameter("gpu11") )
            tmp.append( Parameter("gpd22") )
            tmp.append( Parameter("gpu22") )
            tmp.append( Parameter("gpd33") )
            tmp.append( Parameter("gpu33") )

            # Pseudoscalar Y0-Boson couplings
            tmp.append( Parameter("gpg") )
            tmp.append( Parameter("gpb") )
            tmp.append( Parameter("gpw") )

            # Scalar Y0-Fermion couplings
            tmp.append( Parameter("gsxd") )
            tmp.append( Parameter("gsxr") )
            tmp.append( Parameter("gsxc") )
            tmp.append( Parameter("gsd11") )
            tmp.append( Parameter("gsu11") )
            tmp.append( Parameter("gsd22") )
            tmp.append( Parameter("gsu22") )
            tmp.append( Parameter("gsd33") )
            tmp.append( Parameter("gsu33") )

            # Scalar Y0-Boson couplings
            tmp.append( Parameter("gsg") )
            tmp.append( Parameter("gsh1") )
            tmp.append( Parameter("gsh2") )
            tmp.append( Parameter("gsb") )
            tmp.append( Parameter("gsw") )


            self.parameters = {}
            for par in tmp:
                self.parameters[par.name] = par

    def get_dataset_identifier(self,tag=""):
        s = "DMSimp_"
        if(len(tag)): s = s + tag + "_"
        s = s + "_".join(["{PARAMETER}-{VALUE}".format(PARAMETER=p.name,VALUE=p.value) for p in self.parameters.values() if p.printme])
        s = s.replace(".","p")
        return s

class DMSimp_Spin1_ParameterPoint(ParameterPoint):
    '''Struct to hold parameters for single DMSimp spin 1 dataset'''
    def __init__(self,other=None):
        if other:
            self.parameters = copy.deepcopy(other.parameters)
        else:
            tmp = []
            tmp.append( Parameter("MXd") )
            tmp.append( Parameter("MY1") )

            # Vector Y1-Fermion couplings
            tmp.append( Parameter("gvxd") )
            tmp.append( Parameter("gvxc") )
            tmp.append( Parameter("gvd11") )
            tmp.append( Parameter("gvu11") )
            tmp.append( Parameter("gvd22") )
            tmp.append( Parameter("gvu22") )
            tmp.append( Parameter("gvd33") )
            tmp.append( Parameter("gvu33") )

            # Axial Y1-Fermion couplings
            tmp.append( Parameter("gaxd") )
            tmp.append( Parameter("gaxc") )
            tmp.append( Parameter("gad11") )
            tmp.append( Parameter("gau11") )
            tmp.append( Parameter("gad22") )
            tmp.append( Parameter("gau22") )
            tmp.append( Parameter("gad33") )
            tmp.append( Parameter("gau33") )


            # Vector Y1-boson couplings
            tmp.append( Parameter("gVh") )



            self.parameters = {}
            for par in tmp:
                self.parameters[par.name] = par

    def get_dataset_identifier(self,tag=""):
        s = "DMSimp_"
        if(len(tag)): s = s + tag + "_"
        s = s + "_".join(["{PARAMETER}-{VALUE}".format(PARAMETER=p.name,VALUE=p.value) for p in self.parameters.values() if p.printme])
        s = s.replace(".","p")
        return s

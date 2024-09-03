#! /usr/bin/env python

import sys
import os
def makeDataCards():
    MSuu = [4.0,5.0,6.0,7.0,8.0]
    MChi = [1.0,1.5,2.0,2.5,3.0]
    MSuu_str = ["4","5","6","7","8"]
    MChi_str = ["1","1p5","2","2p5","3"]
    templateFolder = "SuuToChiChi_FullyHadronic_ZtZt_MSuu8TeV_MChi3TeV"
    templateSuffix = ["_customizecards.dat", "_extramodels.dat", "_proc_card.dat","_run_card.dat"]
    for Suu_index,mass_Suu in enumerate(MSuu):
        for Chi_index, mass_Chi in enumerate(MChi):
            if ((2*mass_Chi) >=  mass_Suu):
                continue
            if mass_Chi == 3.0 and mass_Suu == 8.0:
                print "skipping template cards"
                continue
            newCardFolder = "SuuToChiChi_FullyHadronic_ZtZt_MSuu"+MSuu_str[Suu_index] + "TeV_MChi"+MChi_str[Chi_index]+"TeV"
            for suffix in templateSuffix:
                templateFile = open(templateFolder+"/"+templateFolder+suffix,"r")
                if not os.path.exists(newCardFolder):
                    os.makedirs(newCardFolder)
                cardToCreate = open(newCardFolder+"/"+newCardFolder+suffix,"w")
                #change lines 
                for line in templateFile:
                    splitLine = line.split()
                    if len(splitLine) and splitLine[0] == "output": # SuuToChiChi_FullyHadronic_MSuu8TeV_MChi3TeV":
                        cardToCreate.write("output SuuToChiChi_FullyHadronic_ZtZt_MSuu"+MSuu_str[Suu_index]+"TeV_MChi"+MChi_str[Chi_index]+"TeV\n")
                        #print(1)
                    elif (len(splitLine) > 3) and (splitLine[0] == "set") and (splitLine[2] == "mass") and (splitLine[3] == "9936661"):
                        cardToCreate.write("set param_card mass 9936661 "+ str(mass_Suu) +"00000e+03\n")
                        #print(2)
                    elif (len(splitLine) > 3) and (splitLine[0] == "set") and (splitLine[2] == "mass") and (splitLine[3] == "9936662"):
                        cardToCreate.write("set param_card mass 9936662 "+ str(mass_Chi) +"00000e+03\n")
                        #print(3)
                    else:
                        cardToCreate.write(line)   


##############################################################################
#MAIN
##############################################################################
def main(argv): 
    makeDataCards()
if __name__ == "__main__":
    main(sys.argv[1:])

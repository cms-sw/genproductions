import os

print "This script will create the DR2 as well as DS, DS_IS, DS_IS_runningBW and DS_runningBW variations for the inclusive tW sample. This should be executed from this very folder."

prename = "ST_tW_5f_NLO_inclusive_DR"

thedict = {"DR2"             : 2,
           "DS_IS"           : 3,
           "DS_IS_runningBW" : 4,
           "DS"              : 5,
           "DS_runningBW"    : 6,}

for el in thedict:
    newprename = prename.replace("DR", el)
    os.system("mkdir -p ../" + newprename)

    for ejC in ["_customizecards.dat", "_madspin_card.dat"]:
        os.system("cp ./" + prename + ejC + " ../" + newprename + "/")
        os.system("mv ../" + newprename + "/" + prename + ejC + " ../" + newprename + "/" + newprename + ejC)

    outproccard = ""
    with open("./" + prename + "_proc_card.dat", "r") as theF:
        for line in theF.readlines():
            outproccard += line.replace(prename, newprename)
    outprocF = open("../" + newprename + "/" + newprename + "_proc_card.dat", "w")
    outprocF.write(outproccard)
    outprocF.close(); del outprocF

    outruncard = ""
    with open("./" + prename + "_run_card.dat", "r") as theF:
        for line in theF.readlines():
            outruncard += line.replace("1    = istr", "{v}    = istr".format(v = thedict[el]))
    outrunF = open("../" + newprename + "/" + newprename + "_run_card.dat", "w")
    outrunF.write(outruncard)
    outrunF.close(); del outrunF

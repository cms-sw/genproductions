import os

print "This script will create the cards for ST processes produced by flavour-changing neutral currents."


sampledict = {"TLL_Tleptonic_kappa_zct_LO": {"MODELNAME": "TopFCNC_UFO-kappa_zct_ProdConventions_no_c_mass",},
              "TLL_Tleptonic_kappa_zut_LO": {"MODELNAME": "TopFCNC_UFO-kappa_zut_ProdConventions_no_c_mass",}
              }





for el in sampledict:
    os.system("mkdir -p ./" + el)

    for ejC in ["customizecards.dat", "extramodels.dat", "run_card.dat"]:
        os.system("cp ./orig_cards/" + ejC + " ./" + el + "/" + el + "_" + ejC)

    # proc card
    outproccard = ""
    with open("./orig_cards/proc_card.dat", "r") as theF:
        for line in theF.readlines():
            outproccard += line.replace("PROCNAME", el).replace("MODELNAME", sampledict[el]["MODELNAME"])
    outprocF = open("./" + el + "/" + el + "_proc_card.dat", "w")
    outprocF.write(outproccard)
    outprocF.close(); del outprocF

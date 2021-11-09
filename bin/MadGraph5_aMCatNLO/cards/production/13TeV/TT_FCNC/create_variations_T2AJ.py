import os

print "This script will create the cards for the ttbar process produced by flavour-changing neutral currents."


sampledict = {"TT_FCNC-aT2AJ_Tleptonic_kappa_act_LO": {"TOPDECAY":     "t  > w+ b,  w+ > l+ vl NP=0",
                                                       "ANTITOPDECAY": "t~ > a  c~ NP=1"},
              "TT_FCNC-aT2AJ_Tleptonic_kappa_aut_LO": {"TOPDECAY":     "t  > w+ b,  w+ > l+ vl NP=0",
                                                       "ANTITOPDECAY": "t~ > a  u~ NP=1"},
              "TT_FCNC-T2AJ_aTleptonic_kappa_act_LO": {"TOPDECAY":     "t  > a  c NP=1",
                                                       "ANTITOPDECAY": "t~ > w- b~, w- > l- vl~ NP=0"},
              "TT_FCNC-T2AJ_aTleptonic_kappa_aut_LO": {"TOPDECAY":     "t  > a  u NP=1",
                                                       "ANTITOPDECAY": "t~ > w- b~, w- > l- vl~ NP=0"},}





for el in sampledict:
    os.system("mkdir -p ./" + el)

    for ejC in ["customizecards.dat", "extramodels.dat", "run_card.dat"]:
        os.system("cp ./orig_cards/" + ejC + " ./" + el + "/" + el + "_" + ejC)

    # proc card
    outproccard = ""
    with open("./orig_cards/proc_card.dat", "r") as theF:
        for line in theF.readlines():
            outproccard += line.replace("PROCNAME", el)
    outprocF = open("./" + el + "/" + el + "_proc_card.dat", "w")
    outprocF.write(outproccard)
    outprocF.close(); del outprocF


    # param card
    os.system("cp ./orig_cards/" + el + "_param_card.dat ./" + el + "/")


    # madspin card
    outspincard = ""
    with open("./orig_cards/madspin_card.dat", "r") as theF:
        for line in theF.readlines():
            outspincard += line.replace("TOPDECAY", sampledict[el]["TOPDECAY"]).replace("ANTItOPDECAY", sampledict[el]["ANTITOPDECAY"])

    outspinF = open("./" + el + "/" + el + "_madspin_card.dat", "w")
    outspinF.write(outspincard)
    outspinF.close(); del outspinF

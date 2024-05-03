import os

print "This script will create the cards for all ST & TT processes produced by flavour-changing neutral currents."

models = {"zeta_zut"  : {"1 0.000000e+00 #  zzq1" : "1 9.916830e-01 #  zzq1"},
          "zeta_zct"  : {"2 0.000000e+00 #  zzq2" : "2 9.916831e-01 #  zzq2"},
          "kappa_zut" : {"1 0.000000e+00 #  kzq1" : "1 3.179691e-03 #  kzq1"},
          "kappa_zct" : {"2 0.000000e+00 #  kzq2" : "2 3.179691e-03 #  kzq2"},
          "kappa_aut" : {"1 0.000000e+00 #  kaq1" : "1 2.889977e-03 #  kaq1"},
          "kappa_act" : {"2 0.000000e+00 #  kaq2" : "2 2.889977e-03 #  kaq2"},
          "eta_hut"   : {"1 0.000000e+00 #  ehq1" : "1 9.306701e-01 #  ehq1"},
          "eta_hct"   : {"2 0.000000e+00 #  ehq2" : "2 9.306702e-01 #  ehq2"},
}


sampledict = {
# A
"TT_FCNC-aT2AJ_Tleptonic_kappa_act_LO": {"TOPDECAY"   : "t  > w+ b,  w+ > l+ vl NP=0",
                                        "ANTITOPDECAY": "t~ > a  c~ NP=1",
                                        "param_card"  : models["kappa_act"],
                                        "run_card"    : {"5.0    =  etaa": "3.0    =  etaa",
                                                         "5.0    =  etal": "3.0    =  etal",
                                                         },
                                        "run_card_extra" : """#*********************************************************************
# Store info for systematics studies                                 *
# WARNING: If use_syst is T, matched Pythia output is                *
#          meaningful ONLY if plotted taking matchscale              *
#          reweighting into account!                                 *
#*********************************************************************
True    =  use_syst       ! Enable systematics studies
""",
                                        },
"TT_FCNC-aT2AJ_Tleptonic_kappa_aut_LO": {"TOPDECAY"   : "t  > w+ b,  w+ > l+ vl NP=0",
                                        "ANTITOPDECAY": "t~ > a  u~ NP=1",
                                        "param_card"  : models["kappa_aut"],
                                        "run_card"    : {"5.0    =  etaa": "3.0    =  etaa",
                                                         "5.0    =  etal": "3.0    =  etal",
                                                         },
                                        "run_card_extra" : """#*********************************************************************
# Store info for systematics studies                                 *
# WARNING: If use_syst is T, matched Pythia output is                *
#          meaningful ONLY if plotted taking matchscale              *
#          reweighting into account!                                 *
#*********************************************************************
True    =  use_syst       ! Enable systematics studies
""",
                                        },
"TT_FCNC-T2AJ_aTleptonic_kappa_act_LO": {"TOPDECAY"   : "t  > a  c NP=1",
                                        "ANTITOPDECAY": "t~ > w- b~, w- > l- vl~ NP=0",
                                        "param_card"  : models["kappa_act"],
                                        "run_card"    : {"5.0    =  etaa": "3.0    =  etaa",
                                                         "5.0    =  etal": "3.0    =  etal",
                                                         },
                                        "run_card_extra" : """#*********************************************************************
# Store info for systematics studies                                 *
# WARNING: If use_syst is T, matched Pythia output is                *
#          meaningful ONLY if plotted taking matchscale              *
#          reweighting into account!                                 *
#*********************************************************************
True    =  use_syst       ! Enable systematics studies
""",
                                        },
"TT_FCNC-T2AJ_aTleptonic_kappa_aut_LO": {"TOPDECAY"   : "t  > a  u NP=1",
                                        "ANTITOPDECAY": "t~ > w- b~, w- > l- vl~ NP=0",
                                        "madspin_card_extra":{"define vl~   = ve~ vm~ vt~":"define vl~   = ve~ vm~ vt~\ndefine inclu = l- l+ j vl vl~"},
                                        "param_card"  : models["kappa_aut"],
                                        "run_card"    : {"5.0    =  etaa": "3.0    =  etaa",
                                                         "5.0    =  etal": "3.0    =  etal",
                                                         },
                                        "run_card_extra" : """#*********************************************************************
# Store info for systematics studies                                 *
# WARNING: If use_syst is T, matched Pythia output is                *
#          meaningful ONLY if plotted taking matchscale              *
#          reweighting into account!                                 *
#*********************************************************************
True    =  use_syst       ! Enable systematics studies
""",
                                        },

# Z
"TT_FCNC-aT2ZJ_Tleptonic_kappa_zut_LO": {"TOPDECAY"   : "t  > w+ b,  w+ > l+ vl NP=0",
                                        "ANTITOPDECAY": "t~ > z  u~ NP=1",
                                        "do3jets"     : False,
                                        "param_card"  : models["kappa_zut"],
                                        },
"TT_FCNC-T2ZJ_aTleptonic_kappa_zut_LO": {"TOPDECAY"   : "t  > z  u NP=1",
                                        "ANTITOPDECAY": "t~ > w- b~, w- > l- vl~ NP=0",
                                        "do3jets"     : False,
                                        "param_card"  : models["kappa_zut"],
                                        },
"TT_FCNC-aT2ZJ_Tleptonic_kappa_zct_LO": {"TOPDECAY"   : "t  > w+ b,  w+ > l+ vl NP=0",
                                        "ANTITOPDECAY": "t~ > z  c~ NP=1",
                                        "do3jets"     : False,
                                        "param_card"  : models["kappa_zct"],
                                        },
"TT_FCNC-T2ZJ_aTleptonic_kappa_zct_LO": {"TOPDECAY"   : "t  > z  c NP=1",
                                        "ANTITOPDECAY": "t~ > w- b~, w- > l- vl~ NP=0",
                                        "do3jets"     : False,
                                        "param_card"  : models["kappa_zct"],
                                        },
"TT_FCNC-aT2ZJ_Thadronic_kappa_zut_LO": {"TOPDECAY"   : "t  > w+ b,  w+ > j j NP=0",
                                        "ANTITOPDECAY": "t~ > z  u~ NP=1",
                                        "do3jets"     : False,
                                        "param_card"  : models["kappa_zut"],
                                        },
"TT_FCNC-T2ZJ_aThadronic_kappa_zut_LO": {"TOPDECAY"   : "t  > z  u NP=1",
                                        "ANTITOPDECAY": "t~ > w- b~, w- > j j NP=0",
                                        "do3jets"     : False,
                                        "param_card"  : models["kappa_zut"],
                                        },
"TT_FCNC-aT2ZJ_Thadronic_kappa_zct_LO": {"TOPDECAY"   : "t  > w+ b,  w+ > j j NP=0",
                                        "ANTITOPDECAY": "t~ > z  c~ NP=1",
                                        "do3jets"     : False,
                                        "param_card"  : models["kappa_zct"],
                                        },
"TT_FCNC-T2ZJ_aThadronic_kappa_zct_LO": {"TOPDECAY"   : "t  > z  c NP=1",
                                        "ANTITOPDECAY": "t~ > w- b~, w- > j j NP=0",
                                        "do3jets"     : False,
                                        "param_card"  : models["kappa_zct"],
                                        },
"TT_FCNC-aT2ZJ_Tleptonic_zeta_zut_LO" : {"TOPDECAY"   : "t  > w+ b,  w+ > l+ vl NP=0",
                                        "ANTITOPDECAY": "t~ > z  u~ NP=1",
                                        "do3jets"     : False,
                                        "param_card"  : models["zeta_zut"],
                                        },
"TT_FCNC-T2ZJ_aTleptonic_zeta_zut_LO" : {"TOPDECAY"   : "t  > z  u NP=1",
                                        "ANTITOPDECAY": "t~ > w- b~, w- > l- vl~ NP=0",
                                        "do3jets"     : False,
                                        "param_card"  : models["zeta_zut"],
                                        },
"TT_FCNC-aT2ZJ_Tleptonic_zeta_zct_LO" : {"TOPDECAY"   : "t  > w+ b,  w+ > l+ vl NP=0",
                                        "ANTITOPDECAY": "t~ > z  c~ NP=1",
                                        "do3jets"     : False,
                                        "param_card"  : models["zeta_zct"],
                                        },
"TT_FCNC-T2ZJ_aTleptonic_zeta_zct_LO" : {"TOPDECAY"   : "t  > z  c NP=1",
                                        "ANTITOPDECAY": "t~ > w- b~, w- > l- vl~ NP=0",
                                        "do3jets"     : False,
                                        "param_card"  : models["zeta_zct"],
                                        },
"TT_FCNC-aT2ZJ_Thadronic_zeta_zut_LO" : {"TOPDECAY"   : "t  > w+ b,  w+ > j j NP=0",
                                        "ANTITOPDECAY": "t~ > z  u~ NP=1",
                                        "do3jets"     : False,
                                        "param_card"  : models["zeta_zut"],
                                        },
"TT_FCNC-T2ZJ_aThadronic_zeta_zut_LO" : {"TOPDECAY"   : "t  > z  u NP=1",
                                        "ANTITOPDECAY": "t~ > w- b~, w- > j j NP=0",
                                        "madspin_card_extra":{"define vl~   = ve~ vm~ vt~":"define vl~   = ve~ vm~ vt~\ndefine inclu = l- l+ j vl vl~"},
                                        "do3jets"     : False,
                                        "param_card"  : models["zeta_zut"],
                                        },
"TT_FCNC-aT2ZJ_Thadronic_zeta_zct_LO" : {"TOPDECAY"   : "t  > w+ b,  w+ > j j NP=0",
                                        "ANTITOPDECAY": "t~ > z  c~ NP=1",
                                        "do3jets"     : False,
                                        "param_card"  : models["zeta_zct"],
                                        },
"TT_FCNC-T2ZJ_aThadronic_zeta_zct_LO" : {"TOPDECAY"   : "t  > z  c NP=1",
                                        "ANTITOPDECAY": "t~ > w- b~, w- > j j NP=0",
                                        "do3jets"     : False,
                                        "param_card"  : models["zeta_zct"],
                                        },

## H
"TT_FCNC-aT2HJ_Thadronic_eta_hct_LO"  : {"TOPDECAY"   : "t  > w+ b,  w+ > j j NP=0",
                                        "ANTITOPDECAY": "t~ > h  c~ NP=1",
                                        "do3jets"     : False,
                                        "param_card"  : models["eta_hct"],
                                        },
"TT_FCNC-aT2HJ_Thadronic_eta_hut_LO"  : {"TOPDECAY"   : "t  > w+ b,  w+ > j j NP=0",
                                        "ANTITOPDECAY": "t~ > h  u~ NP=1",
                                        "do3jets"     : False,
                                        "param_card"  : models["eta_hut"],
                                        },
"TT_FCNC-aT2HJ_Tleptonic_eta_hct_LO"  : {"TOPDECAY"   : "t  > w+ b,  w+ > l+ vl NP=0",
                                        "ANTITOPDECAY": "t~ > h  c~ NP=1",
                                        "do3jets"     : False,
                                        "param_card"  : models["eta_hct"],
                                        },
"TT_FCNC-aT2HJ_Tleptonic_eta_hut_LO"  : {"TOPDECAY"   : "t  > w+ b,  w+ > l+ vl NP=0",
                                        "ANTITOPDECAY": "t~ > h  u~ NP=1",
                                        "do3jets"     : False,
                                        "param_card"  : models["eta_hut"],
                                        },
"TT_FCNC-T2HJ_aThadronic_eta_hct_LO"  : {"TOPDECAY"   : "t  > h  c NP=1",
                                        "ANTITOPDECAY": "t~ > w- b~, w- > j j NP=0",
                                        "do3jets"     : False,
                                        "param_card"  : models["eta_hct"],
                                        },
"TT_FCNC-T2HJ_aThadronic_eta_hut_LO"  : {"TOPDECAY"   : "t  > h  u NP=1",
                                        "ANTITOPDECAY": "t~ > w- b~, w- > j j NP=0",
                                        "do3jets"     : False,
                                        "param_card"  : models["eta_hut"],
                                        },
"TT_FCNC-T2HJ_aTleptonic_eta_hct_LO"  : {"TOPDECAY"   : "t  > h  c NP=1",
                                        "ANTITOPDECAY": "t~ > w- b~, w- > l- vl~ NP=0",
                                        "do3jets"     : False,
                                        "param_card"  : models["eta_hct"],
                                        },
"TT_FCNC-T2HJ_aTleptonic_eta_hut_LO"  : {"TOPDECAY"   : "t  > h  u NP=1",
                                        "ANTITOPDECAY": "t~ > w- b~, w- > l- vl~ NP=0",
                                        "do3jets"     : False,
                                        "param_card"  : models["eta_hut"],
                                        },
}


for el in sampledict:
    os.system("mkdir -p ./" + el)

    for ejC in ["extramodels.dat"]:
        os.system("cp ./orig_cards/" + ejC + " ./" + el + "/" + el + "_" + ejC)

    # proc card
    outproccard = ""
    with open("./orig_cards/proc_card.dat", "r") as theF:
        for line in theF.readlines():
            outproccard += line.replace("PROCNAME", el)
            if "NP=0 @12" in line and "do3jets" in sampledict[el]:
                if sampledict[el]["do3jets"]:
                    outproccard += "add process p p > t t~ j j j NP=0 @13\n"

    outprocF = open("./" + el + "/" + el + "_proc_card.dat", "w")
    outprocF.write(outproccard)
    outprocF.close(); del outprocF


    # run card
    outruncard = ""
    with open("./orig_cards/run_card.dat", "r") as theF:
        for line in theF.readlines():
            tmpline = line
            if "run_card" in sampledict[el]:
                for subel,subrep in sampledict[el]["run_card"].iteritems():
                    tmpline = tmpline.replace(subel, subrep)
            outruncard += tmpline
        if "run_card_extra" in sampledict[el]:
            outruncard += sampledict[el]["run_card_extra"]

    outrunF = open("./" + el + "/" + el + "_run_card.dat", "w")
    outrunF.write(outruncard)
    outrunF.close(); del outrunF


    # param card
    outparamcard = ""
    with open("./orig_cards/param_card.dat", "r") as theF:
        for line in theF.readlines():
            tmpline = line
            for subel,subrep in sampledict[el]["param_card"].iteritems():
                tmpline = tmpline.replace(subel, subrep)
            outparamcard += tmpline
    outparamF = open("./" + el + "/" + el + "_param_card.dat", "w")
    outparamF.write(outparamcard)
    outparamF.close(); del outparamF


    # madspin card
    outspincard = ""
    with open("./orig_cards/madspin_card.dat", "r") as theF:
        for line in theF.readlines():
            tmpline = line
            if "madspin_card_extra" in sampledict[el]:
                for subel,subrep in sampledict[el]["madspin_card_extra"].iteritems():
                    tmpline = tmpline.replace(subel, subrep)

            outspincard += tmpline.replace("TOPDECAY", sampledict[el]["TOPDECAY"]).replace("ANTItOPDECAY", sampledict[el]["ANTITOPDECAY"])

    outspinF = open("./" + el + "/" + el + "_madspin_card.dat", "w")
    outspinF.write(outspincard)
    outspinF.close(); del outspinF

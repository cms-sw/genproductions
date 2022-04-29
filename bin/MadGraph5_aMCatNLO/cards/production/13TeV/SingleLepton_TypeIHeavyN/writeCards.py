import os
import sys

mixings = ["El", "Mu"]
processes = ["DY", "W"]
amcatnlo = {"W": "generate p p > llp n1 \[QCD\]\\nadd process p p > llm n1 \[QCD\]\\n", "DY": "generate p p > vv n1 \[QCD\]\\nadd process p p > vvx n1 \[QCD\]\\n"}
madspin = {"W": "decay n1 > vv z, z > j j\\ndecay n1 > vvx z, z > j j\\ndecay n1 > vv h, h > b b~\\ndecay n1 > vvx h, h > b b~\\n", "DY": "decay n1 > llp w-, w- > j j\\ndecay n1 > llm w+, w- > j j\\n"}
masses = ["100", "200", "300", "500", "1000", "1500", "2000"]

mixings = ["Mu"]
masses = ["500"]

for m in masses:
    for p in processes:
        for mi in mixings:
            datasetname = p + "toLNuJJ_TypeIHeavyN-" + mi + "_1L_NLO_MN" + m + "_TuneCP5_13TeV-amcatnlo-pythia8"
            process = amcatnlo[p]
            decay = madspin[p]
            os.system("mkdir -p " + p + "/" + datasetname)

            os.system("cp skeleton/extramodels.dat " + p + "/" + datasetname + "/" + datasetname + "_extramodels.dat")
            os.system("cp skeleton/madspin_card.dat " + p + "/" + datasetname + "/" + datasetname + "_madspin_card.dat")
            os.system("cp skeleton/customizecards.dat " + p + "/" + datasetname + "/" + datasetname + "_customizecards.dat")
            os.system("cp skeleton/proc_card.dat " + p + "/" + datasetname + "/" + datasetname + "_proc_card.dat")
            os.system("cp skeleton/run_card.dat " + p + "/" + datasetname + "/" + datasetname + "_run_card.dat")

            os.system("sed -i 's|__mass__|" + m + "|g' " + p + "/" + datasetname + "/" + datasetname + "_customizecards.dat")
            if mi == "El":
                particle = "define llp = e+\\ndefine llm = e-\\ndefine vv = ve\\ndefine vvx = ve~\\n"
                os.system("sed -i 's|__particle__|" + particle + "|g' " + p + "/" + datasetname + "/" + datasetname + "_proc_card.dat")
                os.system("sed -i 's|__ven1__|0.01|g' " + p + "/" + datasetname + "/" + datasetname + "_customizecards.dat") 
                os.system("sed -i 's|__vmun1__|0.0|g' " + p + "/" + datasetname + "/" + datasetname + "_customizecards.dat")

            if mi == "Mu":
                particle = "define llp = mu+\\ndefine llm = mu-\\ndefine vv = vm\\ndefine vvx = vm~\\n"
                os.system("sed -i 's|__particle__|" + particle + "|g' " + p + "/" + datasetname + "/" + datasetname + "_proc_card.dat")
                os.system("sed -i 's|__vmun1__|0.01|g' " + p + "/" + datasetname + "/" + datasetname + "_customizecards.dat")
                os.system("sed -i 's|__ven1__|0.0|g' " + p + "/" + datasetname + "/" + datasetname + "_customizecards.dat")

            os.system("sed -i 's|__decay__|" + decay + "|g' " + p + "/" + datasetname + "/" + datasetname + "_madspin_card.dat")
            os.system("sed -i 's|__process__|" + process + "|g' " + p + "/" + datasetname + "/" + datasetname + "_proc_card.dat")
            os.system("sed -i 's|__output__|output " + datasetname + "|g' " + p + "/" + datasetname + "/" + datasetname + "_proc_card.dat")

            print("./gridpack_generation.sh " + datasetname + " cards/production/13TeV/SingleLepton_TypeIHeavyN/" + p + "/" + datasetname)

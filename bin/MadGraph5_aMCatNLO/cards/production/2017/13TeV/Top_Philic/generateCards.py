#! /bin/env python

import os, shutil, subprocess

REFERENCE_DIR = "TopPhilicZprimeToTTbar_M500"

MASSES = [750, 1000, 1250, 1500, 2000, 2500, 3000, 3500, 4000, 5000, 6000, 7000, 8000, 9000]
#MASSES = [7000]
#MASSES = [8000, 6000]


def formatName(mass):
    return "TopPhilicZprimeToTTbar_M%d" % (mass)

def copyCard(cardName, from_, to):
    fromFilename = "{0}/{0}_{1}.dat".format(from_, cardName)
    toFilename = "{0}/{0}_{1}.dat".format(to, cardName)

    shutil.copyfile(fromFilename, toFilename)

    # Execute sed to substitute the old sample name with the new one
    command = ["sed", "-i", "s/%s/%s/g" % (from_, to), toFilename]
    subprocess.call(command)


for mass in MASSES:
    # Ignore 500 GeV as it's our reference cards
    if mass == 500:
        continue

    sampleName = formatName(mass)

    if os.path.isdir(sampleName):
        shutil.rmtree(sampleName)
        
    os.makedirs(sampleName)
        
    # Copy cards
    copyCard("proc_card", REFERENCE_DIR, sampleName)
    copyCard("run_card", REFERENCE_DIR, sampleName)
    copyCard("customizecards", REFERENCE_DIR, sampleName)
    copyCard("extramodels", REFERENCE_DIR, sampleName)

    # Overwrite customizecards with correct mass and width
    with open("{0}/{0}_customizecards.dat".format(sampleName), "w") as f:
        f.write("set param_card mass 6000055 %d\n" % mass)
        f.write("set param_card mass 6 172.5\n")
        f.write("set param_card yukawa 6 172.5\n")


      #  f.write("set param_card decay 6000047 %.2f" % (mass * width))

    # Overwrite prod_cards with correct mass and width
    with open("{0}/{0}_proc_card.dat".format(sampleName), "w") as f:
        f.write("set group_subprocesses Auto\n")
        f.write("set ignore_six_quark_processes False\n")
        f.write("set loop_optimized_output True\n")
        f.write("set complex_mass_scheme False\n")
        f.write("import model Top_Philic_V1_NLO\n")
        f.write("define p = g u c d s b u~ c~ d~ s~ b~\n") 
        f.write("define j = g u c d s b u~ c~ d~ s~ b~\n")
        f.write("generate p p > v1 j [noborn=QCD]\n")
        f.write("output TopPhilicZprimeToTTbar_M%d -nojpeg\n" % mass)
        #f.write("\n")




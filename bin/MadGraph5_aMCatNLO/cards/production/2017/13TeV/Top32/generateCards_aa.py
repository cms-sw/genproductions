#! /bin/env python

import os, shutil, subprocess

REFERENCE_DIR = "Top32_1000_aaChannel"

MASSES = [700,800,900,1200,1300,1400,1500,1600,1700,1800,1900,2000,2500,3000]
#MASSES = [700]


def formatName(mass):
    return "Top32_%d_aaChannel" % (mass)

def copyCard(cardName, from_, to):
    fromFilename = "{0}/{0}_{1}.dat".format(from_, cardName)
    toFilename = "{0}/{0}_{1}.dat".format(to, cardName)

    shutil.copyfile(fromFilename, toFilename)

    # Execute sed to substitute the old sample name with the new one
    command = ["sed", "-i", "s/%s/%s/g" % (from_, to), toFilename]
    subprocess.call(command)


for mass in MASSES:
    # Ignore 1000 GeV as it's our reference cards
    if mass == 1000:
        continue
    decay = mass*0.1
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
        f.write("set param_card mass 9000005 %d\n" % mass)
        f.write("set param_card decay 9000005 %d\n" % decay)

    # Overwrite prod_cards with correct mass and width
    with open("{0}/{0}_proc_card.dat".format(sampleName), "w") as f:
        f.write("set group_subprocesses Auto\n")
        f.write("set ignore_six_quark_processes False\n")
        f.write("set loop_optimized_output True\n")
        f.write("set complex_mass_scheme False\n")
        f.write("import model Top32_UFO\n")
        f.write("define p = g u c d s u~ c~ d~ s~\n")
        f.write("define j = g u c d s u~ c~ d~ s~\n")
        f.write("define l+ = e+ mu+\n")
        f.write("define l- = e- mu-\n")
        f.write("define vl = ve vm vt\n")
        f.write("define vl~ = ve~ vm~ vt~\n")
        f.write("define wdec = u d s c b u~ d~ s~ c~ b~ vl vl~ l+ l- ta+ ta-\n")
        f.write("generate p p > tstar tstar~ , ( tstar > t a , ( t > b w+ , w+ > wdec wdec ) ) , ( tstar~ > t~ a , ( t~ > b~ w- , w- > wdec wdec ) )\n")
        f.write("output Top32_%d_aaChannel -nojpeg\n" % mass)


#! /bin/env python

import os, shutil, subprocess

REFERENCE_DIR = "ZPrimeToTTJets_M500GeV_W5GeV"
# MASSES = [5000]
# WIDTHS = [0.10]

MASSES = [500, 750, 1000, 1250, 1500, 2000, 2500, 3000, 3500, 4000, 5000, 6000, 7000, 8000, 9000]
WIDTHS = [0.01, 0.10, 0.30]

def widthToString(width):
    return ("%g" % width).replace('.', 'p')

def formatName(mass, width):
    return "ZPrimeToTTJets_M%dGeV_W%sGeV" % (mass, width)

def copyCard(cardName, from_, to):
    fromFilename = "{0}/{0}_{1}.dat".format(from_, cardName)
    toFilename = "{0}/{0}_{1}.dat".format(to, cardName)

    shutil.copyfile(fromFilename, toFilename)

    # Execute sed to substitute the old sample name with the new one
    command = ["sed", "-i", "s/%s/%s/g" % (from_, to), toFilename]
    subprocess.call(command)


for mass in MASSES:
    for width in WIDTHS:

        # Ignore 500 GeV narrow as it's our reference cards
        if mass == 500 and width == 0.01:
            continue

        # if width == 0.30 and not mass == 1000 and not mass == 2000 and not mass == 3000 and not mass == 4000:
        #     continue

        sampleName = formatName(mass, widthToString(mass * width))

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
            f.write("set param_card mass 6000047 %d\n" % mass)
            f.write("set param_card decay 6000047 %.2f" % (mass * width))





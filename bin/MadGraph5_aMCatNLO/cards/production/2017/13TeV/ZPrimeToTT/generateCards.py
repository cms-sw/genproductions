#! /bin/env python

import os, shutil, subprocess

REFERENCE_DIR = "ZPrimeToTT_M500GeV_W50GeV"

MASSES = [400, 500, 600, 700, 800, 900, 1000, 1200, 1400, 1600, 1800, 2000, 2500, 3000, 3500, 4000, 4500]
WIDTHS = [0.10, 0.30]

def widthToString(width):
    return ("%g" % width).replace('.', 'p')

def formatName(mass, width):
    return "ZPrimeToTT_M%dGeV_W%sGeV" % (mass, width)

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
        if mass == 500 and width == 0.10:
            continue

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





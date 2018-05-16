#! /bin/env python

import os, shutil, subprocess

REFERENCE_DIR = "reference"

# MASSES = [500, 750, 1000, 1250, 1500, 2000, 2500, 2750, 3000, 3250, 3500, 3750, 4000]
# WIDTHS = [0.01, 0.10, 0.30]

#format  [mass, [widths] ]
MASSES = [
           [2000, [0.01, 0.03, 0.10, 0.20, 0.30]],
           [3000, [0.03, 0.10, 0.20, 0.30]],
           [4000, [0.01, 0.03, 0.10, 0.20, 0.30]],
           [5000, [0.03, 0.10, 0.20, 0.30]],
           [6000, [0.03, 0.10, 0.20, 0.30]],
           [8000, [0.30]],
           [10000,[0.30]],
         ]

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


for setup in MASSES:
    mass=setup[0]
    for width in setup[1]:

        # Ignore 500 GeV narrow as it's our reference cards
        # if mass == 500 and width == 0.01:
        #     continue

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
            f.write("set param_card CKMBLOCK 1 0.0\n")
            f.write("set param_card mass 6000047 %d\n" % mass)
            f.write("set param_card decay 6000047 %.2f" % (mass * width))

        with open("{0}/{0}_proc_card.dat".format(sampleName), "a") as f:
            f.write("output {0} -nojpeg\n".format(sampleName))


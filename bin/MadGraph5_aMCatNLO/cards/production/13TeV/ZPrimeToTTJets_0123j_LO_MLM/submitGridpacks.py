#! /bin/env python

import os, shutil, subprocess

REFERENCE_DIR = "ZPrimeToTTJets_M500GeV_W5GeV"

MASSES = [500, 750, 1000, 1250, 1500, 2000, 2500, 2750, 3000, 3250, 3500, 3750, 4000]
WIDTHS = [0.01, 0.10, 0.30]

def widthToString(width):
    return ("%g" % width).replace('.', 'p')

def formatName(mass, width):
    return "ZPrimeToTTJets_M%dGeV_W%sGeV" % (mass, width)

for mass in MASSES:
    for width in WIDTHS:

        if width == 0.30 and not mass == 1000 and not mass == 2000 and not mass == 4000:
            continue

        sampleName = formatName(mass, widthToString(mass * width))

        print("./submit_gridpack_generation_local.sh 30000 2nd {0} cards/production/13TeV/ZPrimeToTTJets_0123j_LO_MLM/{0}/ 2nd".format(sampleName))

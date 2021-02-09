#! /bin/env python

import os, shutil, subprocess

REFERENCE_DIR = "ZPrimeToTT_M500GeV_W50GeV"


MASSES = [400, 500, 600, 700, 800, 900, 1000, 1200, 1400, 1600, 1800, 2000, 2500, 3000, 3500, 4000, 4500]
WIDTHS = [0.10, 0.30]

def widthToString(width):
    return ("%g" % width).replace('.', 'p')

def formatName(mass, width):
    return "ZPrimeToTT_M%dGeV_W%sGeV" % (mass, width)

for mass in MASSES:
    for width in WIDTHS:

        sampleName = formatName(mass, widthToString(mass * width))
        print("./gridpack_generation.sh {0} cards/production/2017/13TeV/ZPrimeToTT/{0} 1nd".format(sampleName))
        print("rm -r {0} ".format(sampleName))

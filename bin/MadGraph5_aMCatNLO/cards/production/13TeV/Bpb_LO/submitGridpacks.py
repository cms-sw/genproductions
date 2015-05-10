#! /bin/env python

import os, shutil, subprocess

MASSES = [800, 900, 1000, 1100, 1200, 1300, 1400, 1500, 1600, 1700, 1800]
WIDTHS = [0.01]

def widthToString(width):
    return ("%g" % width).replace('.', 'p')

def formatName(mass, width):
    return "Bpb_M%dGeV_W%sGeV" % (mass, width)

for mass in MASSES:
    for width in WIDTHS:

        sampleName = formatName(mass, widthToString(mass * width))

        print("./submit_gridpack_generation_local.sh 30000 8nh {0} cards/production/13TeV/Bpb_LO_MLM/{0}/ 8nh".format(sampleName))

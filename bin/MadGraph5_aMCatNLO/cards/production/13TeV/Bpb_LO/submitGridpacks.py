#! /bin/env python

import os, shutil, subprocess

FinalStates = ["tW","Zb","Hb"]
Chirality =["LH","RH"]
MASSES = [700, 800, 900, 1000, 1100, 1200, 1300, 1400, 1500, 1600, 1700, 1800]
WIDTHS = [0.01]

def widthToString(width):
    return ("%g" % width).replace('.', 'p')

def formatName(mass, width, final, hand):
    return "Bpb_M%dGeV_W%sGeV_%s_%s" % (mass, width, final, hand)


for state in FinalStates:
    for hand in Chirality:
        for mass in MASSES:
            for width in WIDTHS:
                sampleName = formatName(mass, widthToString(mass * width), state, hand)

                print("./submit_gridpack_generation_local.sh 30000 8nh {0} cards/production/13TeV/Bpb_LO_MLM/{0}/ 1nh".format(sampleName))

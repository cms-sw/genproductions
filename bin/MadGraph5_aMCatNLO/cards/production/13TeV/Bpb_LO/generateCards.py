#! /bin/env python

import os, shutil, subprocess

REFERENCE_DIR = "Bpb_conf"

FinalStates = ["tW"]
Chirality =["LH","RH"]
MASSES = [700, 800, 900, 1000, 1100, 1200, 1300, 1400, 1500, 1600, 1700, 1800]
WIDTHS = [0.01]

def widthToString(width):
    return ("%g" % width).replace('.', 'p')

def formatName(mass, width, final, hand):
    return "Bpb_M%dGeV_W%sGeV_%s_%s" % (mass, width, final, hand)

def copyCard(prefix, cardName, from_, to):
    #print from_+"/"+from_+prefix+"_"+cardName+".dat",  to+"/"+to
    fromFilename = (str("{0}/{0}"+prefix+"_{1}.dat")).format(from_, cardName)
    toFilename = "{0}/{0}_{1}.dat".format(to, cardName)

    print 'copy',fromFilename,'-->', toFilename
    shutil.copyfile(fromFilename, toFilename)

    # Execute sed to substitute the old sample name with the new one
    command = ["sed", "-i", "s/%s/%s/g" % (from_, to), toFilename]
    subprocess.call(command)

for state in FinalStates:
    for hand in Chirality:
        for mass in MASSES:
            for width in WIDTHS:

                sampleName = formatName(mass, widthToString(mass * width), state, hand)
                print sampleName
                
                if os.path.isdir(sampleName):
                    shutil.rmtree(sampleName)
    
                os.makedirs(sampleName)
                    
                # Copy cards
                copyCard("_"+state,"proc_card", REFERENCE_DIR, sampleName)
                copyCard("","run_card", REFERENCE_DIR, sampleName)
                copyCard("_"+hand,"customizecards", REFERENCE_DIR, sampleName)
                if state == "tW":
                    copyCard("","madspin_card", REFERENCE_DIR, sampleName)

		# Add the name of file
		with open("{0}/{0}_proc_card.dat".format(sampleName), "a") as f: 
		    f.write("output "+sampleName)

                # Add in the customizecards the correct mass and width
                with open("{0}/{0}_customizecards.dat".format(sampleName), "a") as f:
                    f.write("set param_card mass 8000002 %d\n" % mass)
                    f.write("set param_card decay 8000002 %.2f" % (mass * width))

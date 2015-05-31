# Create directories for various mass points 
# Run with:
# $ python makeDirs.py  

import os


# first mass is chargino, second is neutralino
# Mass spectra calculated for AMSB model with Isajet 7.80, stored in:
# https://github.com/wulsin/DisappTrks/blob/master/SignalMC/data/AMSB_chargino_*GeV_Isajet780.slha 
masses = [ [100,  99.8], 
           [200, 199.8],
           [300, 299.9],
           [400, 399.9],
           [500, 499.8],
           [600, 599.9],
           [700, 699.9],
           [800, 799.9],
           [900, 899.9],
]

basenameDir = "AMSBcharginoXXXGeV"  

for mass in masses: 
    onedir = basenameDir.replace("XXX", str(mass[0]))  
    os.system("mkdir " + onedir)
    os.system("cp C1C1_C1N1_2j_proc_card.dat " + onedir)    
    os.system("cp C1C1_C1N1_2j_run_card.dat "  + onedir) 

    # For the customize file, modify the masses.  
    filein  = open("C1C1_C1N1_2j_customizecards.dat", "r")
    content = filein.read()
    content = content.replace("CHARGINOMASS",   str(mass[0]))
    content = content.replace("NEUTRALINOMASS", str(mass[1]))
    customizeName = onedir + "/C1C1_C1N1_2j_C1mass" + str(mass[0]) + "GeV_customizecards.dat"
    fileout = open(customizeName, "w")
    fileout.write(content)
    fileout.close()




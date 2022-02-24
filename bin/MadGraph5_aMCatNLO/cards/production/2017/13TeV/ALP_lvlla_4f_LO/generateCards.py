#! /bin/env python

import os, shutil, subprocess

Ref_dir = "./"
Ref_name = "ALP_lvlla_4f_LO"
ALP_pdgid = '9000005' #ALP pdgid number
MassesALP = [50, 90, 100, 110, 160, 200, 300, 400]

for mALP in MassesALP:
    sampleName = 'ALP_lvlla_4f_LO_m' + str(mALP)
    print sampleName
    if os.path.isdir(sampleName):
        shutil.rmtree(sampleName)
    os.makedirs(sampleName)
    # Copy cards
    shutil.copyfile(Ref_dir + Ref_name + '_run_card.dat',sampleName+'/'+sampleName+'_run_card.dat')
    shutil.copyfile(Ref_dir + Ref_name + '_extramodels.dat',sampleName+'/'+sampleName+'_extramodels.dat')
    shutil.copyfile(Ref_dir + Ref_name + '_proc_card.dat',sampleName+'/'+sampleName+'_proc_card.dat')
    #customization
    os.system('sed -i "s/output\ ALP_lvlla_4f_LO/output\ {0}/g" {0}/{0}_proc_card.dat'.format(sampleName))
    #create customization card
    with open("{0}/{0}_customizecards.dat".format(sampleName), "w") as f:
        f.write("set param_card mass %s %s\n" % (ALP_pdgid, str(mALP)))
        f.write("set param_card decay %s  %s\n" % (ALP_pdgid, 'AUTO'))
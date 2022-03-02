#! /bin/env python

import os, shutil, subprocess

Ref_dir = "Template/"
Y_pdgid = '39' #Y pdgid number
H_pdgid = '25' #H pdgid number

MassesY = [2000, 3000, 5000]
MassesH = [400]

WidthY = 0.03
WidthH = 0.03

for mY in MassesY:
    for mH in MassesH:
        sampleName = 'YtoHH_Htott_Y'+str(mY)+'_H'+str(mH)
        print sampleName
        if os.path.isdir(sampleName):
            shutil.rmtree(sampleName)
        os.makedirs(sampleName)
        # Copy cards
        shutil.copyfile(Ref_dir + 'run_card.dat',sampleName+'/'+sampleName+'_run_card.dat')
        shutil.copyfile(Ref_dir+ 'madspin_card.dat',sampleName+'/'+sampleName+'_madspin_card.dat')
        shutil.copyfile(Ref_dir+ 'extramodels.dat',sampleName+'/'+sampleName+'_extramodels.dat')
        shutil.copyfile(Ref_dir + 'proc_card.dat',sampleName+'/'+sampleName+'_proc_card.dat')
        #customization
        with open("{0}/{0}_proc_card.dat".format(sampleName), "a") as f:
            f.write("output "+sampleName)
        #create customization card
        with open("{0}/{0}_customizecards.dat".format(sampleName), "w") as f:
            f.write("set param_card mass %s %e\n" % (H_pdgid, mH))
            f.write("set param_card mass %s %e\n" % (Y_pdgid, mY))
            f.write("set param_card decay %s  %e\n" % (H_pdgid, mH * WidthH))
            f.write("set param_card decay %s  %e\n" % (Y_pdgid, mY * WidthY))

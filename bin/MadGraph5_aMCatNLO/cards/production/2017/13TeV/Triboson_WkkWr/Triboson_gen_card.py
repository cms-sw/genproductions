#! /bin/env python

import os, shutil, subprocess

Ref_dir = "Triboson"
pdgidWkk = '9000024' #Wkk pdgid number
pdgidWr = '9000025' #Wr pdgid number

MassesWkk = [2000, 3000, 5000]
MassesWr = [25,80,170,400]

WidthWkk = 0.03
WidthWr = 0.03

for mwkk in MassesWkk:
        for mwr in MassesWr:
                sampleName = Ref_dir+'_M'+str(mwkk)+'_Mr'+str(mwr)
                print sampleName
                if os.path.isdir(sampleName):
                        shutil.rmtree(sampleName)
                os.makedirs(sampleName)
                # Copy cards
                shutil.copyfile(Ref_dir+'_M_R/Triboson_M_R_run_card.dat',sampleName+'/'+sampleName+'_run_card.dat')
                shutil.copyfile(Ref_dir+'_M_R/Triboson_M_R_proc_card.dat',sampleName+'/'+sampleName+'_proc_card.dat')
                shutil.copyfile(Ref_dir+'_M_R/Triboson_M_R_customizecards.dat',sampleName+'/'+sampleName+'_customizecards.dat')
                shutil.copyfile(Ref_dir+'_M_R/Triboson_M_R_extramodels.dat',sampleName+'/'+sampleName+'_extramodels.dat')
                #customization
                with open("{0}/{0}_proc_card.dat".format(sampleName), "a") as f:
                        f.write("output "+sampleName+" -nojpeg")
                with open("{0}/{0}_customizecards.dat".format(sampleName), "a") as f:
                        f.write("set param_card kkinputs 5 %e\n" % mwkk)
                        f.write("set param_card mass "+pdgidWr+" %e\n" % mwr)
                        f.write("set param_card mass "+pdgidWkk+" %e\n" % mwkk)
                        f.write("set param_card DECAY "+pdgidWr+" %e\n" % (mwr * WidthWr))
                        f.write("set param_card DECAY "+pdgidWkk+" %e\n" % (mwkk * WidthWkk))

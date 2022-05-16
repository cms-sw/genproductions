#! /bin/env python

import os, shutil, subprocess

Ref_dir = "tZ_b_LH"

#MassesTp = [2000, 3000, 5000]
#MassesZ = [25,80,170,400]

MassesTp = [2000]
MassesZ = [170]

for mtp in MassesTp:
        for mz in MassesZ:
                sampleName = Ref_dir+'_MT'+str(mtp)+'_MZ'+str(mz)
                print sampleName
                if os.path.isdir(sampleName):
                        shutil.rmtree(sampleName)
                os.makedirs(sampleName)
                # Copy cards
                shutil.copyfile(Ref_dir+'_conf/run_card.dat',sampleName+'/'+sampleName+'_run_card.dat')
                shutil.copyfile(Ref_dir+'_conf/madspin_card.dat',sampleName+'/'+sampleName+'_madspin_card.dat')
                shutil.copyfile(Ref_dir+'_conf/proc_card.dat',sampleName+'/'+sampleName+'_proc_card.dat')
                shutil.copyfile(Ref_dir+'_conf/customizecards.dat',sampleName+'/'+sampleName+'_customizecards.dat')
                shutil.copyfile(Ref_dir+'_conf/extramodels.dat',sampleName+'/'+sampleName+'_extramodels.dat')
                #customization
                with open("{0}/{0}_proc_card.dat".format(sampleName), "a") as f:
                        f.write("output "+sampleName)
                with open("{0}/{0}_customizecards.dat".format(sampleName), "a") as f:
                        f.write("set param_card DECAY 8000001 %e\n" % (mtp * 0.03))
                        f.write("set param_card mass 8000001 %e\n" % mtp)
                        f.write("set param_card mass 23 %e\n" % mz)


#! /bin/env python

import os, shutil, subprocess

Ref_dir = "WpToBpT"
pdgid = '6000007' #Bp pdgid number

FinalState = "Zbt" #Zb in one cone from Bp decay.  t is another 3-prong jet, though it's mass is not traditional t mass

Chirality = "LH"

MassesWp = [2000, 3000, 5000]
MassesBp = [25,80,170,400]
MassesT = [25,80,170,400]

WidthWp = 0.03
WidthBp = 0.03

for mwp in MassesWp:
        for mbp in MassesBp:
                for mt in MassesT:
                        sampleName = Ref_dir+'_Wp'+str(mwp)+'_Bp'+str(mbp)+'_Top'+str(mt)+'_'+FinalState
                        print sampleName
                        if os.path.isdir(sampleName):
                                shutil.rmtree(sampleName)
                        os.makedirs(sampleName)
                        # Copy cards
                        shutil.copyfile(Ref_dir+'_conf/run_card.dat',sampleName+'/'+sampleName+'_run_card.dat')
                        shutil.copyfile(Ref_dir+'_conf/Zt_madspin_card.dat',sampleName+'/'+sampleName+'_madspin_card.dat')
                        shutil.copyfile(Ref_dir+'_conf/Zt_proc_card.dat',sampleName+'/'+sampleName+'_proc_card.dat')
                        shutil.copyfile(Ref_dir+'_conf/Zt_customizecards.dat',sampleName+'/'+sampleName+'_customizecards.dat')
                        shutil.copyfile(Ref_dir+'_conf/extramodels.dat',sampleName+'/'+sampleName+'_extramodels.dat')
                        #customization
                        with open("{0}/{0}_proc_card.dat".format(sampleName), "a") as f:
                                f.write("output "+sampleName)
                        with open("{0}/{0}_customizecards.dat".format(sampleName), "a") as f:
                                f.write("set param_card mass "+pdgid+" %e\n" % mbp)
                                f.write("set param_card mass 6000024 %e\n" % mwp)
                                f.write("set param_card decay 6000024 %e\n" % (mwp * WidthWp))
                                f.write("set param_card decay "+pdgid+" %e\n" % (mbp * WidthBp))
                                f.write("set param_card mass 6 %e\n" % mt)

#! /bin/env python

import os, shutil, subprocess

Ref_dir = "bstar_tW"
pdgidBStar = '1005'
pdgidZ = '23'

MassesBStar = [2000, 3000, 5000]
MassesW = [29,80,170,400]

for mbs in MassesBStar:
        for mw in MassesW:
                sampleName = ''
                if(mw == 29):
                        sampleName = Ref_dir+'_MB'+str(mbs)+'_MW'+str(25) #here, MW will be about 25.  Other MZ points give MZ ~ MW
                else:
                        sampleName = Ref_dir+'_MB'+str(mbs)+'_MW'+str(mw)
                print sampleName
                if os.path.isdir(sampleName):
                        shutil.rmtree(sampleName)
                os.makedirs(sampleName)
                # Copy cards
                shutil.copyfile(Ref_dir+'_MB_MW/'+Ref_dir+'_MB_MW_run_card.dat',sampleName+'/'+sampleName+'_run_card.dat')
                shutil.copyfile(Ref_dir+'_MB_MW/'+Ref_dir+'_MB_MW_proc_card.dat',sampleName+'/'+sampleName+'_proc_card.dat')
                shutil.copyfile(Ref_dir+'_MB_MW/'+Ref_dir+'_MB_MW_customizecards.dat',sampleName+'/'+sampleName+'_customizecards.dat')
                shutil.copyfile(Ref_dir+'_MB_MW/'+Ref_dir+'_MB_MW_extramodels.dat',sampleName+'/'+sampleName+'_extramodels.dat')
                #customization
                with open("{0}/{0}_proc_card.dat".format(sampleName), "a") as f:
                        f.write("output "+sampleName)
                with open("{0}/{0}_customizecards.dat".format(sampleName), "a") as f:
                        f.write("set param_card mass "+pdgidBStar+" %e\n" % mbs)
                        f.write("set param_card mass "+pdgidZ+" %e\n" % mw)
                        f.write("set param_card DECAY "+pdgidBStar+" Auto\n")


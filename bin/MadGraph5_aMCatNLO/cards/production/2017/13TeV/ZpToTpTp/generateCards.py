#! /bin/env python

import os, shutil, subprocess

Ref_dir = "ZpToTpTp"
Zp_pdgid = '6000023' #Z' pdgid number
Tp_pdgid = '6000006' #T' pdgid number

MassesZp = [2000, 3000, 5000]
MassesTp = [400]

WidthZp = 0.03
WidthTp = 0.03

for mzp in MassesZp:
    for mtp in MassesTp:
        sampleName = Ref_dir+'_Zp'+str(mzp)+'_Tp'+str(mtp)
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
            f.write("set param_card mass %s %e\n" % (Zp_pdgid, mzp))
            f.write("set param_card mass %s %e\n" % (Tp_pdgid, mtp))
            f.write("set param_card decay %s  %e\n" % (Zp_pdgid, mzp * WidthZp))
            f.write("set param_card decay %s  %e\n" % (Tp_pdgid, mtp * WidthTp))

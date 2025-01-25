#! /bin/env python

import os, shutil, subprocess

Ref_dir = "./"
T_pdgid = '6000006' #T pdgid number
S_pdgid = '6100001' #S pdgid number
couplL = '3' #id ks10tl third generation
couplR = '3' #id ks10tr third generation
couplSWW = '2' #id ks10vv W
couplSZZ = '3' #id ks10vv ZZ
couplSZA = '4' #id ks10vv ZA
couplSAA = '5' #id ks10vv AA

MassesT = [800,900,1000,1100,1200,1300,1400,1500]
MassesS = [50,100,150,200,250,300,350]

decayT = 0.1
decayS = 0.01

couplST = 4.0
couplSV = 1.0

for mT in MassesT:
    for mS in MassesS:
        sampleName = 'TTtoSStt_StoAA_'+str(mT)+'_'+str(mS)
        print sampleName
        if os.path.isdir(sampleName):
            shutil.rmtree(sampleName)
        os.makedirs(sampleName)
        # Copy cards
        shutil.copyfile(Ref_dir + 'TT_SStt_run_card.dat',sampleName+'/'+sampleName+'_run_card.dat')
        shutil.copyfile(Ref_dir+ 'TT_SStt_customizecards.dat',sampleName+'/'+sampleName+'_customizecards.dat')
        shutil.copyfile(Ref_dir+ 'TT_SStt_extramodels.dat',sampleName+'/'+sampleName+'_extramodels.dat')
        shutil.copyfile(Ref_dir + 'TT_SStt_proc_card.dat',sampleName+'/'+sampleName+'_proc_card.dat')
        #customization
        with open("{0}/{0}_proc_card.dat".format(sampleName), "a") as f:
            f.write("output "+sampleName)
        #create customization card
        with open("{0}/{0}_customizecards.dat".format(sampleName), "w") as f:
            f.write("set param_card mass %s %e\n" % (T_pdgid, mT))
            f.write("set param_card mass %s %e\n" % (S_pdgid, mS))
            f.write("set param_card decay %s %e\n" % (T_pdgid, decayT))
            f.write("set param_card decay %s %e\n" % (S_pdgid, decayS))
            f.write("set param_card ks10tl %s %e\n" % (couplL, couplST))
            f.write("set param_card ks10tr %s %e\n" % (couplR, couplST))
            f.write("set param_card ks10vv %s %e\n" % (couplSWW, couplSV))
            f.write("set param_card ks10vv %s %e\n" % (couplSZZ, couplSV))
            f.write("set param_card ks10vv %s %e\n" % (couplSZA, couplSV))
            f.write("set param_card ks10vv %s %e\n" % (couplSAA, couplSV))

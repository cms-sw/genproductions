#! /bin/env python

import os, shutil, subprocess

Ref_dir = "XToYYprime"

MassesX = [2000, 3000, 5000]
MassesY = [25,80,170,400]
MassesYprime = [25,80,170,400]

for mx in MassesX:
        for my in MassesY:
                for myp in MassesYprime:
                        sampleName = Ref_dir+'_MX'+str(mx)+'_MY'+str(my)+'_MYprime'+str(myp)+'_narrow'
                        print sampleName
                        if os.path.isdir(sampleName):
                                shutil.rmtree(sampleName)
                        os.makedirs(sampleName)
                        # Copy cards
                        shutil.copyfile('template_cards/XToYYprime_MX_MY_MYprime_narrow_run_card.dat',sampleName+'/'+sampleName+'_run_card.dat')
                        shutil.copyfile('template_cards/XToYYprime_MX_MY_MYprime_narrow_proc_card.dat',sampleName+'/'+sampleName+'_proc_card.dat')
                        shutil.copyfile('template_cards/XToYYprime_MX_MY_MYprime_narrow_customizecards.dat',sampleName+'/'+sampleName+'_customizecards.dat')
                        shutil.copyfile('template_cards/XToYYprime_MX_MY_MYprime_narrow_extramodels.dat',sampleName+'/'+sampleName+'_extramodels.dat')
                        #customization
                        with open("{0}/{0}_proc_card.dat".format(sampleName), "a") as f:
                                f.write("output "+sampleName+" -nojpeg") #output XToYYprime_MX<MASS_X>_MY<MASS_Y>_MYprime<MASS_Yprime>_narrow -nojpeg)
                        with open("{0}/{0}_customizecards.dat".format(sampleName), "a") as f:
                                f.write("set param_card mass 9000001 %e\n" % mx)
                                f.write("set param_card mass 9000002 %e\n" % mx)
                                f.write("set param_card rhoinputs 2 %e\n" % mx)
                                f.write("set param_card mass 24 %e\n" % myp)
                                f.write("set param_card mass 23 %e\n" % my)

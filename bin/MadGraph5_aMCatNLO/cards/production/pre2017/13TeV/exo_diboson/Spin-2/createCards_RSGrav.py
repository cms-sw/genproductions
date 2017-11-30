#!/usr/bin/python

import sys
import os

nInputParams = len(sys.argv)

generate = 0
fileSuffName = ""

for iParam in range(0, nInputParams) :
    if sys.argv[iParam] == "-g" :
        generate = 1
        print "generate jobs"
    if sys.argv[iParam] == "-f":
        iParam = 1+iParam
        fileSuffName = sys.argv[iParam]

if fileSuffName == "" :
    print "ERROR!! Please could you check more carefully your input options; -f (card template suffix) have not been set! "
    exit()


run_card_template = open(fileSuffName+'_run_card.dat','r')
customizecards_template = open(fileSuffName+'_customizecards.dat','r')
proc_card_template = open(fileSuffName+'_proc_card.dat','r')
extra_card_template = open(fileSuffName+'_extramodels.dat')

run_content = run_card_template.read()
custom_content = customizecards_template.read()
proc_content = proc_card_template.read()
extra_content = extra_card_template.read()

# select name for the H decay process
H_decays = ['']

#select mass points
mass_points = ['260','270','300','350','400','450','500','550','600','650','700','800','900','1000','1100','1200','1300','1500','2000']

import fileinput    
for H_decay in H_decays :
    for mass in mass_points :
        print 'decay , mass point: ', H_decay, mass
        directoryName = str('./RSGraviton_hh_narrow/RSGraviton_hh'+H_decay+'_narrow_M'+mass)
        print directoryName
        if generate==1 :
            os.makedirs( directoryName )
            os.chdir( directoryName )

            run_card = open( 'RSGraviton_hh'+H_decay+'_narrow_M'+mass+'_run_card.dat', 'w' )
            run_card.write( run_content )
            run_card.close()

            extra_card = open( 'RSGraviton_hh'+H_decay+'_narrow_M'+mass+'_extramodels.dat', 'w' )
            extra_card.write( extra_content )
            extra_card.close()

            customize_card = open( 'RSGraviton_hh'+H_decay+'_narrow_M'+mass+'_customizecards.dat', 'w' )
            customize_card.write( custom_content )
            customize_card.close()

            for line in fileinput.input('RSGraviton_hh'+H_decay+'_narrow_M'+mass+'_customizecards.dat', inplace=True): 
                print line.rstrip().replace('set param_card mass 39 MASS', 'set param_card mass 39 '+mass),'\n' 
                
            for line in fileinput.input('RSGraviton_hh'+H_decay+'_narrow_M'+mass+'_customizecards.dat', inplace=True):
                if not line.isspace():
                    sys.stdout.write(line)

            proc_card = open( 'RSGraviton_hh'+H_decay+'_narrow_M'+mass+'_proc_card.dat', 'w' )            
            proc_card.write( proc_content )
            proc_card.close()
            
            for line in fileinput.input('RSGraviton_hh'+H_decay+'_narrow_M'+mass+'_proc_card.dat', inplace=True): 
                print line.rstrip().replace('RSGraviton_hh_hdecay_narrow_Mmass', 'RSGraviton_hh'+H_decay+'_narrow_M'+mass),'\n'
                
            for line in fileinput.input('RSGraviton_hh'+H_decay+'_narrow_M'+mass+'_proc_card.dat', inplace=True):
                if not line.isspace():
                    sys.stdout.write(line)

            os.chdir( '../../' )
            

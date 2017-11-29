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

run_content = run_card_template.read()
custom_content = customizecards_template.read()
proc_content = proc_card_template.read()

# select name for the H decay process
H_decays = ['GF_HH']

#select mass points
mass_points = ['250','260','270','280','290','300','350','400','450','500','550','600','650','700','750','800','900','1000','1200','1400','1600','1800','2000','2500','3000','3500','4000','4500']

import fileinput    
for H_decay in H_decays :
    for mass in mass_points :
        print 'decay , mass point: ', H_decay, mass
        directoryName = str('./BulkGraviton_hh_'+H_decay+'_narrow_M'+mass)
        print directoryName
        if generate==1 :
            os.makedirs( directoryName )
            os.chdir( directoryName )

            run_card = open( 'BulkGraviton_hh_'+H_decay+'_narrow_M'+mass+'_run_card.dat', 'w' )
            run_card.write( run_content )
            run_card.close()

            customize_card = open( 'BulkGraviton_hh_'+H_decay+'_narrow_M'+mass+'_customizecards.dat', 'w' )
            customize_card.write( custom_content )
            customize_card.close()

            proc_card = open( 'BulkGraviton_hh_'+H_decay+'_narrow_M'+mass+'_proc_card.dat', 'w' )            
            proc_card.write( proc_content )
            proc_card.close()
            
            for line in fileinput.input('BulkGraviton_hh_'+H_decay+'_narrow_M'+mass+'_proc_card.dat', inplace=True): 
                print line.rstrip().replace('BulkGraviton_hh_hdecay_narrow_Mmass', 'BulkGraviton_hh_'+H_decay+'_narrow_M'+mass),'\n'
                
            os.chdir( '../' )
            

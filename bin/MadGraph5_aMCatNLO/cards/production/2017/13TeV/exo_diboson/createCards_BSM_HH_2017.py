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


if "Radion" in fileSuffName :
    H_decay = ''
    process = 'Radion_hh'
    outDir = 'Spin-0/'+process+'_narrow/'+process+H_decay+'_narrow_M'
elif "RSGraviton" in fileSuffName :
    H_decay = ''
    process = 'RSGraviton_hh'
    outDir = 'Spin-2/'+process+'_narrow/'+process+H_decay+'_narrow_M'
elif "BulkGraviton" in fileSuffName :
    H_decay = '_GF_HH'
    process = 'BulkGraviton_hh'
    outDir = 'Spin-2/'+process+'_narrow/'+process+H_decay+'_narrow_M'
    
outFileSuff = process+H_decay+'_narrow_M'

run_card_template = open(fileSuffName+'_run_card.dat','r')
customizecards_template = open(fileSuffName+'_customizecards.dat','r')
proc_card_template = open(fileSuffName+'_proc_card.dat','r')
extra_card_template = open(fileSuffName+'_extramodels.dat')

run_content = run_card_template.read()
custom_content = customizecards_template.read()
proc_content = proc_card_template.read()
extra_content = extra_card_template.read()

#select mass points
mass_points = ['260','270','300','350','400','450','500','550','600','650','700','800','900','1000','1100','1200','1300','1500','2000']
if 'BulkGraviton' in process :
    mass_points = ['250','260','270','280','290','300','350','400','450','500','550','600','650','700','750','800','900','1000','1200','1400','1600','1800','2000','2500','3000','3500','4000','4500']

import fileinput    

for mass in mass_points :
    print 'decay , mass point: ', H_decay, mass
    directoryName = str(outDir+mass)
    print directoryName
    if generate==1 :
        os.makedirs( directoryName )
        os.chdir( directoryName )
        
        run_card = open( outFileSuff+mass+'_run_card.dat', 'w' )
        run_card.write( run_content )
        run_card.close()
        
        extra_card = open( outFileSuff+mass+'_extramodels.dat', 'w' )
        extra_card.write( extra_content )
        extra_card.close()
        
        customize_card = open( outFileSuff+mass+'_customizecards.dat', 'w' )
        customize_card.write( custom_content )
        customize_card.close()
        
        for line in fileinput.input(outFileSuff+mass+'_customizecards.dat', inplace=True): 
            
            if "BulkGraviton" or "RSGraviton" in process : 
                pdgId_resonance = '39'
            if "Radion" in process : 
                pdgId_resonance = '35'
            print line.rstrip().replace('set param_card mass '+pdgId_resonance+' MASS', 'set param_card mass '+pdgId_resonance+' '+mass),'\n'
 
        for line in fileinput.input(outFileSuff+mass+'_customizecards.dat', inplace=True):
            if not line.isspace():
                sys.stdout.write(line)

        proc_card = open( outFileSuff+mass+'_proc_card.dat', 'w' )            
        proc_card.write( proc_content )
        proc_card.close()
            
        for line in fileinput.input(outFileSuff+mass+'_proc_card.dat', inplace=True): 
            print line.rstrip().replace(process+'_hdecay_narrow_Mmass', outFileSuff+mass),'\n' 
            
        for line in fileinput.input(outFileSuff+mass+'_proc_card.dat', inplace=True):
            if not line.isspace():
                sys.stdout.write(line)
       
        os.chdir( '../../../' )


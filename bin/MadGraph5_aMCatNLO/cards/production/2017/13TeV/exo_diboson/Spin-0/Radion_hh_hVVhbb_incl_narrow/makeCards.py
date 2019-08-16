#!/usr/bin/python

# Run this script by executing 'python makeCards.py' in the directory 'Radion_hh_hVVhbb_incl_narrow_2017'
import os

# set the desired mass points for production here
masses = [600,800,900,1000,1200,1400,1600,1800,2000,2500,3000,3500,4000,4500]

sample = 'Radion_hh_hVVhbb_incl_narrow_MMASS' 
template_run_card = 'Template/'+sample+'_run_card.dat'
template_proc_card = 'Template/'+sample+'_proc_card.dat'
template_extramodels = 'Template/'+sample+'_extramodels.dat'
template_customizecards = 'Template/'+sample+'_customizecards.dat'

for mass in masses:
    with open(template_proc_card,'r') as file_pc:
        content_pc = file_pc.read()
    content_pc = content_pc.replace('MASS',str(mass))
   
    with open(template_customizecards,'r') as file_cc:
        content_cc = file_cc.read()
    content_cc = content_cc.replace('MASS',str(mass))

    with open(template_run_card,'r') as file_rc:
        content_rc = file_rc.read()
    with open(template_extramodels,'r') as file_em:
        content_em = file_em.read()

    os.system('mkdir -p M%i' % mass)
    file_run_card = 'M%i/Radion_hh_hVVhbb_incl_narrow_M%i_run_card.dat' % (mass,mass)
    file_proc_card = 'M%i/Radion_hh_hVVhbb_incl_narrow_M%i_proc_card.dat' % (mass,mass)
    file_customizecards = 'M%i/Radion_hh_hVVhbb_incl_narrow_M%i_customizecards.dat' % (mass,mass)
    file_extramodels = 'M%i/Radion_hh_hVVhbb_incl_narrow_M%i_extramodels.dat' % (mass,mass)

    with open(file_run_card,'w') as rc_file:
        rc_file.write(content_rc)
    with open(file_proc_card,'w') as pc_file:
        pc_file.write(content_pc)
    with open(file_customizecards,'w') as cc_file:
        cc_file.write(content_cc)
    with open(file_extramodels,'w') as em_file:
        em_file.write(content_em)
pass

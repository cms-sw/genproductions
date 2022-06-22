from __future__ import print_function

import os, glob
import numpy as np

# Script for replacing hToaaTo4gamma MG5 template cards with actual mass points

process = 'NMSSM_XYH_YTobb_HToWWTo2QLNu_MX_mMX_MY_mMY'
cards = glob.glob('%s_*.dat'%process)
[print(card) for card in cards]
assert len(cards) == 4

outdir_parent = 'mass_cards'
if not os.path.isdir(outdir_parent):
    os.makedirs(outdir_parent)

import math

def get_pt(M, m1, m2):
    xx = (M**2 - (m1+m2)**2)*(M**2 - (m1-m2)**2)
    if xx>0:
        pt = math.sqrt(xx)*1./(2*M)
    else: 
        pt=0
    return pt

def is_boosted(pT, m):
    tag = bool(False)
    if pT>(1.5*m/0.8):
        tag = True
    return tag

X_mass = [800,900,1000,1100,1200,1300,1400,1500,1600,1700,1800,1900,2000,2200,2400,2600,2800,3000,3300,3600,4000]
Y_mass = [40,50,60,70,80,90,100,125,150,200,250,300,350,400,450,500,600,700,800,900,1000]
H_mass = 125


for X_m in X_mass:
    print("X: ", X_m)
    print("Y: ", end = ' ')
    for Y_m in Y_mass:
        if is_boosted(get_pt(X_m,Y_m,H_mass),max(H_mass,Y_m)):
           converted_X_m = str(X_m)
           converted_Y_m = str(Y_m)
           X_m_sci = format(X_m, 'e')
           Y_m_sci = format(Y_m, 'e')
           outdir = '%s/NMSSM_XYH_YTobb_HToWWTo2QLNu_MX_%s_MY_%s'%(outdir_parent,converted_X_m,converted_Y_m)
           if not os.path.isdir(outdir):
                 os.makedirs(outdir)
           print(Y_m, end = ' ')
           for card in cards:
               src_file = card
               tgt_file = '%s/%s'%(outdir, src_file.replace('mMX', converted_X_m).replace('mMY', converted_Y_m))
               print('%s -> %s'%(src_file, tgt_file))
               src_txt = open(src_file, 'r').read()

               if 'proc_card' in card:
                      tgt_txt = src_txt.replace('mMX', converted_X_m).replace('mMY', converted_Y_m)
               elif 'customizecards' in card:
                      tgt_txt = src_txt.replace('mMX', X_m_sci).replace('mMY', Y_m_sci)
               else:
                      tgt_txt = src_txt
               with open(tgt_file, 'w') as f:
                      f.write(tgt_txt)


'''
for m in masses:

    # Set mass string to be used for file naming
    # Decimal points should be replaced with 'p'
    m_name = ('%.1f'%m).replace('.','p') if m%1. != 0. else '%.f'%m
    # Set mass string in scientific notation for customize cards
    m_sci = format(m, 'e')
    zd_mass = format(2.*m, 'e')
    print('ma: %s %s GeV, Zd: %s'%(m_name, m_sci, zd_mass))

    for card in cards:

        # Define template source file and target file to contain actual mass
        src_file = card
        tgt_file = '%s/%s'%(outdir, src_file.replace('_AMASS_', m_name))
        #shutil.copyfile(src_file, tgt_file)
        print('%s -> %s'%(src_file, tgt_file))

        # Read in template text and replace _AMASS_ with actual mass values
        src_txt = open(src_file, 'r').read()

        # proc_card uses mass name
        if 'proc_card' in card:
            tgt_txt = src_txt.replace('_AMASS_', m_name)
            #print('_AMASS_ -> %s'%m_name)
        # customize card uses scientific notation
        elif 'customizecards' in card:
            tgt_txt = src_txt.replace('_AMASS_', m_sci).replace('_ZDMASS_', zd_mass)
            #print('_AMASS_  -> %s'%m_sci)
            #print('_ZDMASS_ -> %s'%zd_mass)
        # other cards don't need replacement
        else:
            tgt_txt = src_txt

        # Write out new cards with actual masses
        with open(tgt_file, 'w') as f:
            f.write(tgt_txt)
            '''

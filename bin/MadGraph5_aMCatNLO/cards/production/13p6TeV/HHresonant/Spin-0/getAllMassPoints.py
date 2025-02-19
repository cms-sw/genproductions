from __future__ import print_function

import os, glob
import numpy as np

# Script for replacing X->HH MG5 template cards with actual mass points

process = 'Radion_hh_narrow_M'
card_dir = './'

cards = glob.glob('{}{}*_*.dat'.format(card_dir,process))

for card in cards:
    print(card)

# mass grid agreed from by B2G dib / MC&I conveners:
# https://indico.cern.ch/event/1319579/contributions/5594760/attachments/2720893/4727074/HHnews_25sep2023-1.pdf
m_X_HH_full = [250, 260, 270, 280, 300, 320, 350, 360, 400, 450, 500, 550, 600, 650, 700, 750, 800, 850, 900, 1000, 1100, 1200, 1300, 1400, 1500, 1600, 1700, 1800, 1900, 2000, 2200, 2400, 2500, 2600, 2800, 3000, 3500, 4000, 4500, 5000]


Ngridpacks=0
for X_m in m_X_HH_full:
    print("X: ", X_m)

    Ngridpacks+=1
    
    outdir = card_dir + process + str(X_m)

    if not os.path.isdir(outdir):
        os.makedirs(outdir)

    for card in cards:
        src_file = card
        tgt_file = outdir + "/" + src_file.replace('M900','M{}'.format(X_m))
        
        print('{} -> {}'.format(src_file,tgt_file))
        
        src_txt = open(src_file, 'r').read()

        if 'proc_card' in card:
            tgt_txt = src_txt.replace('900', str(X_m))
        elif 'customizecards' in card:
            tgt_txt = src_txt.replace('900', str(X_m))
        else:
            tgt_txt = src_txt
            
        with open(tgt_file, 'w') as f:
            f.write(tgt_txt)


print(Ngridpacks)

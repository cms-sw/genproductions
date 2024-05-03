from __future__ import print_function

import os, glob
import numpy as np

# Script for replacing X->YH MG5 template cards with actual mass points

process = 'NMSSM_XToYH_YToHH_MX_'
card_dir = './'

cards = glob.glob('{}{}*_*.dat'.format(card_dir,process))

for card in cards:
    print(card)

# mass grid agreed by B2G dib / MC&I conveners:
# https://gitlab.cern.ch/cms-b2g/b-2-g-m-csample-requests/-/issues/6
X_mass = [240, 280, 300, 320, 350, 400, 450, 500, 550, 600, 650, 700, 750, 800, 850, 900, 950, 1000, 1200, 1400, 1600, 1800, 2000, 2200, 2400, 2500, 2600, 2800, 3000, 3500, 4000]
Y_mass = [50, 60, 70, 80, 90, 95, 100, 125, 150, 170, 200, 250, 300, 350, 400, 450, 500, 550, 600, 650, 700, 800, 1000, 1200, 1400, 1600, 1800, 2000, 2200, 2300, 2400, 2600, 2800, 3000, 3300, 3500, 3800]


def is_mass_ok(m_x,m_y):
    return m_y >= 250. and m_y < (m_x-125.)

Ngridpacks=0
for X_m in X_mass:
    print("X: ", X_m)

    for Y_m in Y_mass:
        if not is_mass_ok(X_m,Y_m):
            continue
        print("Y:", Y_m)
        Ngridpacks+=1

        outdir = card_dir + process + str(X_m) + "_MY_" + str(Y_m)

        if not os.path.isdir(outdir):
            os.makedirs(outdir)

        for card in cards:
            src_file = card
            tgt_file = outdir + "/" + src_file.replace('xxx',str(X_m))\
                                              .replace('yyy',str(Y_m))

            print('{} -> {}'.format(src_file,tgt_file))

            src_txt = open(src_file, 'r').read()

            if ('proc_card' in card) or ('customizecards' in card):
                tgt_txt = src_txt.replace('xxx', str(X_m)).replace('yyy', str(Y_m))
            else:
                tgt_txt = src_txt
            
            with open(tgt_file, 'w') as f:
                f.write(tgt_txt)


print(Ngridpacks)

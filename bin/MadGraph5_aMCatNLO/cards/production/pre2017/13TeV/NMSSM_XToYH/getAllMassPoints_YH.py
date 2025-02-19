from __future__ import print_function

import os, glob
import numpy as np

# Script for replacing X->YH MG5 template cards with actual mass points

process = 'NMSSM_XToYH_MX_'
card_dir = './'

cards = glob.glob('{}NMSSM_XToYH*/*.dat'.format(card_dir))

for card in cards:
    print(card)

X_mass = [240,280,300,320,360,400,500,600,700,800,2200,2400,2500,2600,2800,3000]

Y_mass = [60,70,80,90,100,125,150,170,190,200,250,300,350,400,450,500,550,600,650,700,800,900,1000,1100,1200,1300,1400,1600,1800,2000,2200,2400,2600,2800]

def is_mass_ok(m_x,m_y):
    return m_y < (m_x-125.)

Ngridpacks=0
for X_m in X_mass:
    #print("X: ", X_m)

    for Y_m in Y_mass:
        if not is_mass_ok(X_m,Y_m):
            continue
        #print("Y:", Y_m)
        Ngridpacks+=1

        outdir = card_dir + process + str(X_m) + "_MY_" + str(Y_m)

        if not os.path.isdir(outdir):
            os.makedirs(outdir)

        for card in cards:
            src_file = card
            tgt_file = src_file.replace('InputCards_example_','NMSSM_XToYH_').replace('MX500_MY300','MX_{}_MY_{}'.format(X_m,Y_m)).replace('MX_500_MY_300','MX_{}_MY_{}'.format(X_m,Y_m)).replace('XYo','XTo')

            print('{} -> {}'.format(src_file,tgt_file))

            src_txt = open(src_file, 'r').read()

            if 'proc_card' in card:
                tgt_txt = src_txt.replace('MX_500', 'MX_{}'.format(X_m)).replace('MY_300', 'MY_{}'.format(Y_m))
            elif 'customizecards' in card:
                tgt_txt = src_txt.replace('45 500', '45 {}'.format(X_m)).replace('35 300', '35 {}'.format(Y_m))
            else:
                tgt_txt = src_txt

            with open(tgt_file, 'w') as f:
                f.write(tgt_txt)


print(Ngridpacks)

from __future__ import print_function

import os, glob
import numpy as np

# Script for replacing hToaaTo4gamma MG5 template cards with actual mass points

process = 'hToaaTo4gamma_ma_AMASS_GeV_MLM_4f_max1j'
cards = glob.glob('%s_*.dat'%process)
[print(card) for card in cards]
assert len(cards) == 4

outdir = 'mass_cards'
if not os.path.isdir(outdir):
    os.makedirs(outdir)

# Define ma points
masses = np.concatenate([
    #np.arange(1e2,   1e3, 1e2)/1.e3, # [100 MeV,  1   GeV) in 100 MeV steps
    #np.arange(1e3, 2.6e3, 2e2)/1.e3, # [  1 GeV,  2.6 GeV) in 200 MeV steps
    [0.1], # 100 MeV
    np.arange(  2e2, 1.2e3, 2e2)/1.e3, # [200 MeV,  1.2 GeV) in 200 MeV steps
    np.arange(1.2e3, 2.8e3, 4e2)/1.e3, # [1.2 GeV,  2.4 GeV] in 400 MeV steps
    [3.], # 3 GeV
    np.arange(5e3,  65e3, 5e3)/1.e3,   # [  5 GeV,   60 GeV] in   5 GeV steps
])

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

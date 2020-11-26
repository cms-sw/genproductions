#!/usr/bin/env python
from __future__ import print_function
import os


process = 'BsTo2DpTo4Mu' #BsTo2DpTo2Mu2e
template = 'SIDM_BsTo2DpTo4l_ctau-XXX_TuneCP5_13TeV_pythia8_cff.py'

verbose = False

paramList = [
        (10, 0.3, 2.4),
        (10, 0.3, 24),
        (10, 0.3, 240),
        (10, 1.2, 9.6),
        (10, 1.2, 96),
        (10, 1.2, 960),
        (20, 0.3, 1.2),
        (20, 0.3, 12),
        (20, 0.3, 120),
        (20, 1.2, 4.8),
        (20, 1.2, 48),
        (20, 1.2, 480),
        (40, 0.3, 0.6),
        (40, 0.3, 6),
        (40, 0.3, 60),
        (40, 1.2, 2.4),
        (40, 1.2, 24),
        (40, 1.2, 240),
        (200, 0.3, 0.12),
        (200, 0.3, 1.2),
        (200, 0.3, 12),
        (200, 1.2, 0.48),
        (200, 1.2, 4.8),
        (200, 1.2, 48),
        (400, 0.3, 0.06),
        (400, 0.3, 0.6),
        (400, 0.3, 6),
        (400, 1.2, 0.24),
        (400, 1.2, 2.4),
        (400, 1.2, 24),
        (1000, 0.3, 0.024),
        (1000, 0.3, 0.24),
        (1000, 0.3, 2.4),
        (1000, 1.2, 0.096),
        (1000, 1.2, 0.96),
        (1000, 1.2, 9.6)
        ]

if __name__ == '__main__':
    
    for mBs, mDp, cTau in paramList:
        fragName = 'SIDM_{0}_ctau-{1}_TuneCP5_13TeV_pythia8_cff.py'.format(process, str(cTau).replace('.', 'p'))
        if os.path.isfile(fragName): continue
        os.system('cp {0} {1}'.format(template, fragName))
        cmd = 'sed -i "s#X__CTAU__X#{0}#g" {1}'.format(cTau, fragName)
        if verbose: print(cmd)
        os.system(cmd)

#!/usr/bin/env python
from __future__ import print_function
import os
import math

process = 'BsTo2DpTo4Mu'
nameTag = 'MBs-{0}_MDp-{1}_ctau-{2}' # ctau is mm here!!

verbose = True

# ctau [mm] was chosen such that:
# ===============================
# darkphoton has decay length in lab frame ~ 300 cm in transverse plane
# assume factor of 0.8 from lv to lxy: 300/0.8 = 375
# boost factor: beta = mbs/2/mdp; ctau * beta = 375
# => ctau [mm] = 3750 * 2 * mdp/mbs

paramList = [
        (50, 0.25, 37.5),
        (50, 0.80, 120),
        (50, 1.20, 180),
        (50, 2.50, 375),
        (50, 5,    750),

        (100, 0.25, 18.75),
        (100, 0.80, 60),
        (100, 1.20, 90),
        (100, 2.50, 187.5),
        (100, 5,    375),

        (150, 0.25, 12.5),
        (150, 0.80, 40),
        (150, 1.20, 60),
        (150, 2.50, 125),
        (150, 5,    250),

        (200, 0.25, 9.375),
        (200, 0.80, 30),
        (200, 1.20, 45),
        (200, 2.50, 93.75),
        (200, 5,    187.5),

        (500, 0.25, 3.75),
        (500, 0.80, 12),
        (500, 1.20, 18),
        (500, 2.50, 37.5),
        (500, 5,    75),

        (800, 0.25, 2.344),
        (800, 0.80, 7.5),
        (800, 1.20, 11.25),
        (800, 2.50, 23.438),
        (800, 5,    46.875),

        (1000, 0.25, 1.875),
        (1000, 0.80, 6),
        (1000, 1.20, 9),
        (1000, 2.50, 18.75),
        (1000, 5,    37.5),
        ]

if __name__ == '__main__':

    for mBs, mDp, cTau in paramList:
        ## ctau = 0.08 * (0.1/mDp) * (1e-4/epsilon)**2  [mm]
        epsilon = math.sqrt(80./mDp/cTau) * 1e-6
        if verbose: print(mBs, mDp, epsilon)

        mBs_str = str(mBs)
        mDp_str = str(mDp).replace('.', 'p')
        ctau_str = str(cTau).replace('.', 'p')
        toDir = 'SIDM_'+process+'_'+nameTag.format(mBs_str, mDp_str, ctau_str)
        if verbose: print(toDir)

        if os.path.isdir(toDir):
            os.system('rm -r {0}'.format(toDir))
        os.system('cp -r templates {0}'.format(toDir))
        cmd0 = 'sed -i "s#XMASS#{0}#g" {1}'.format(mBs, toDir+'/'+process+'_customizecards.dat')
        cmd1 = 'sed -i "s#MED#{0}#g" {1}'.format(mDp, toDir+'/'+process+'_customizecards.dat')
        cmd2 = 'sed -i "s#EPSILON#{0}#g" {1}'.format('{0:6e}'.format(epsilon), toDir+'/'+process+'_customizecards.dat')
        cmd3 = 'sed -i "s#PROC#{0}#g" {1}'.format(toDir, toDir+'/'+process+'_proc_card.dat')
        for cmd in [cmd0, cmd1, cmd2, cmd3]:
            if verbose: print(cmd)
            os.system(cmd)
        for c in ['customizecards.dat', 'extramodels.dat', 'proc_card.dat', 'run_card.dat']:
            oldname = toDir+'/'+process+'_'+c
            newname = toDir+'/'+toDir+'_'+c
            cmd = 'mv {0} {1}'.format(oldname, newname)
            if verbose: print(cmd)
            os.system(cmd)

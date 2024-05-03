import numpy as np
from numpy import arange
import argparse
import os
import math

parser = argparse.ArgumentParser()
parser.add_argument('--masses', '-m', help= 'Masses to use for the heavy Higgs', default='600')
parser.add_argument('--widths', '-w', help= 'Relative Widths to use for the heavy Higgs', default='0.001')
args = parser.parse_args()

def WriteExtraModelsCard(out_name):
    with open(out_name+'_extramodels.dat', "w") as extramodels_file:
        extramodels_file.write('loop_sm_twoscalar.tar.gz')

def WriteRunCard(out_name):
    os.system('cp run_card_template.dat %s_run_card.dat' % out_name)

def WriteProcCard(out_name, extra=''):
    proccard_out = 'import model loop_sm_twoscalar\n'
    proccard_out += 'generate p p > h h [QCD]' + (' %s\n' % extra if extra else '\n') 
    proccard_out += 'output %s -nojpeg' % out_name.split('/')[0]
    with open(out_name+'_proc_card.dat', "w") as proccard_file:
        proccard_file.write(proccard_out)

def WriteCustomizeCard(out_name, out_str):
    with open(out_name+'_customizecards.dat', "w") as customizecard_file:
        customizecard_file.write(out_str)

def WriteReweightCard(out_name, mass, widths=[0.001,0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1,0.11,0.12,0.13,0.14,0.15,0.16]):
    reweight_out_string='\
change rwgt_dir ./rwgt\n\
\n'

    width_dep_string = '\
launch --rwgt_name=$postfix\n\
  set decay 99925 $W\n\
\n'

    for w in widths:
        postfix = ('RelWidth_%g' % (w)).replace('.','p')
        reweight_out_string += width_dep_string.replace('$W', '%g' % (float(mass)*w)).replace('$postfix', postfix)

    with open(out_name+'_reweight_card.dat', "w") as reweightcard_file:
        reweightcard_file.write(reweight_out_string)

if len(args.masses) == 0 or len(args.widths) == 0:
    print 'At least one mass point and one width must be specified! Exiting.'
    exit()


masses = args.masses.split(',')
widths = args.widths.split(',')

masses = [float(x) for x in masses]
widths = [float(x) for x in widths]


# first make cards for non-resonant contributions that don't depend on the mass or the widths of the heavy scalar
# we set A12 to 0 and KAP111 to 31.803878252 which corresponds to the SM values of the Yukawas and triple Higgs couplings
# mass and width of the eta0 set to some arbitrary values, these will have no influence anyway
customizecards_base_file = open("customizecards_template.dat","r")
nonres_customizecards_out = customizecards_base_file.read().replace('$Weta0','6').replace('$Meta0','600').replace('$KAP112','0.0').replace('$KAP111','31.803878252').replace('$A12','0.0')

for x in ['BOX', 'SChan_h', 'BOX_SChan_h_inteference']:
    os.system('mkdir -p HH_loop_sm_twoscalar_%s' % x)

    if x == 'BOX': extra = '/ eta0 iota0 LAM111=0'
    elif x == 'SChan_h': extra = '/ eta0 iota0 LAM111^2==2'
    elif x == 'BOX_SChan_h_inteference' : extra = '/ eta0 iota0 LAM111^2==1'

    card_name = "HH_loop_sm_twoscalar_%(x)s/HH_loop_sm_twoscalar_%(x)s" % vars()

    WriteProcCard(card_name, extra)
    WriteExtraModelsCard(card_name)
    WriteRunCard("HH_loop_sm_twoscalar_%(x)s/HH_loop_sm_twoscalar_%(x)s" % vars())
    WriteCustomizeCard(card_name, nonres_customizecards_out)

# now make cards for resonant contributions (including inteferences with non-resonant part)
for m in masses:
    for w in widths:
        ca = 1./math.sqrt(2)
        customizecards_base_file = open("customizecards_template.dat","r")
        res_customizecards = customizecards_base_file.read().replace('$Weta0','%g' % (w*m)).replace('$Meta0','%g' % m).replace('$KAP111','31.803878252').replace('$KAP112', '%.9f' % (31.803878252))
        for x in ['SChan_eta0', 'BOX_SChan_eta0_inteference', 'SChan_h_SChan_eta0_inteference']:
            out_name = ('%s_M_%g_RelWidth_%g' % (x,m,w)).replace('.','p')

            # for resonant s-channel we set A12 to pi/2 to give Yukawas equal to SM values
            # for the inteference terms we will require A12 != 0 or pi/2 to have non-zero Yukawas for both the h and the eta0
            # We set this to pi/4, which effectivly means that the Yukawas for both scalars equal 1/sqrt(2) * the SM value
            # Therefore, the individual templates will be need to be scaled up by a factor of 2 (1./ca**2) for SChan_h_SChan_eta0_inteference and a factor of 2^1.5 (1/ca**3) for BOX_SChan_eta0_inteference to account for this

            res_customizecards_out = res_customizecards.replace('$Weta0','%g' % (w*m)).replace('$A12','%.6f' % (1.570796 if x == 'SChan_eta0' else 0.785398))
            os.system('mkdir -p HH_loop_sm_twoscalar_%s' % out_name)

            if x == 'SChan_eta0': extra = '/ h iota0 LAM112^2==2'
            elif x == 'BOX_SChan_eta0_inteference': extra = '/ h iota0 LAM112^2==1'
            elif x == 'SChan_h_SChan_eta0_inteference' : extra = '/ iota0 LAM111^2==1 LAM112^2==1'

            card_name = "HH_loop_sm_twoscalar_%(out_name)s/HH_loop_sm_twoscalar_%(out_name)s" % vars()

            WriteProcCard(card_name, extra)
            WriteExtraModelsCard(card_name)
            WriteRunCard(card_name)
            WriteCustomizeCard(card_name, res_customizecards_out)
            WriteReweightCard(card_name, m)


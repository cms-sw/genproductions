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
        extramodels_file.write('loop_sm_twoscalar.tgz')

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

def WriteReweightCard(out_name, mass, non_res_only=False, widths=[0.001,0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1,0.11,0.12,0.13,0.14,0.15,0.16]):
    reweight_out_string='\
change process p p > h h [QCD] / iota0\n\
\n\
launch --rwgt_name=box\n\
  set bsm 6 0.785398\n\
  set bsm 15 0.000000e+00\n\
  set bsm 16 0.000000e+00\n\
\n\
launch --rwgt_name=box_and_schannel_h_1\n\
  set bsm 6 0.785398\n\
  set bsm 15 31.803878252\n\
  set bsm 16 0.000000e+00\n\
\n\
launch --rwgt_name=box_and_schannel_h_2\n\
  set bsm 6 0.785398\n\
  set bsm 15 318.03878252\n\
  set bsm 16 0.000000e+00\n\
\n\
launch --rwgt_name=all\n\
  set bsm 6 0.785398\n\
  set bsm 15 31.803878252\n\
  set bsm 16 31.803878252\n\
\n'

    if not non_res_only:
        reweight_out_string += '\
launch --rwgt_name=schannel_H\n\
  set bsm 6 1.570796\n\
  set bsm 15 0.000000e+00\n\
  set bsm 16 31.803878252\n\
\n\
launch --rwgt_name=box_and_schannel_H_1\n\
  set bsm 6 0.785398\n\
  set bsm 15 0.000000e+00\n\
  set bsm 16 31.803878252\n\
\n\
launch --rwgt_name=box_and_schannel_H_2\n\
  set bsm 6 0.785398\n\
  set bsm 15 0.000000e+00\n\
  set bsm 16 318.03878252\n\n'


    width_dep_string = '\
launch --rwgt_name=all_$postfix\n\
  set bsm 6 0.785398\n\
  set bsm 15 31.803878252\n\
  set bsm 16 31.803878252\n\
  set decay 99925 $W\n\
\n\
launch --rwgt_name=schannel_H_$postfix\n\
  set bsm 6 1.570796\n\
  set bsm 15 0.000000e+00\n\
  set bsm 16 31.803878252\n\
  set decay 99925 $W\n\
\n\
launch --rwgt_name=box_and_schannel_H_1_$postfix\n\
  set bsm 6 0.785398\n\
  set bsm 15 0.000000e+00\n\
  set bsm 16 31.803878252\n\
  set decay 99925 $W\n\
\n\
launch --rwgt_name=box_and_schannel_H_2_$postfix\n\
  set bsm 6 0.785398\n\
  set bsm 15 0.000000e+00\n\
  set bsm 16 318.03878252\n\
  set decay 99925 $W\n\n'

    for w in widths:
        reweight_out_string += width_dep_string.replace('$W', '%g' % (float(mass)*w))

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
    WriteReweightCard(card_name, 600, non_res_only=True)

# now make cards for resonant contributions (including inteferences with non-resonant part)
for m in masses:
    for w in widths:
        ca = 1./math.sqrt(2)
        customizecards_base_file = open("customizecards_template.dat","r")
        res_customizecards = customizecards_base_file.read().replace('$Weta0','%g' % (w*m)).replace('$Meta0','%g' % m).replace('$A12','%.6f' % 0.785398).replace('$KAP111','31.803878252')
        for x in ['SChan_eta0', 'BOX_SChan_eta0_inteference', 'SChan_h_SChan_eta0_inteference']:
            out_name = ('%s_M_%g_RelWidth_%g' % (x,m,w)).replace('.','p')

            # for the non-resonant case we have to set A12 to an non-zero value to ensure we have non-zero Yukawas for both scalars. 
            # We set this to pi/4, which effectivly means that the Yukawas for both scalars equal 1/sqrt(2) * the SM value
            # To make sure we end up with cross-sections that correspond to kappa=1 (Yukawas = SM value and lambda_hhh and lambda_Hhh equal SM lambda_hhh) we scale KAP112 to compensate for the smaller Yukawa values
            # the sf will be different for each component as they include depend on different orders of the yukawa couplings
            if x == 'SChan_eta0' or x == 'SChan_h_SChan_eta0_inteference': sf = 1./ca**2
            elif x == 'BOX_SChan_eta0_inteference': sf = 1./ca**3
            res_customizecards_out = res_customizecards.replace('$Weta0','%g' % (w*m)).replace('$KAP112', '%.9f' % (31.803878252*sf))
            os.system('mkdir -p HH_loop_sm_twoscalar_%s' % out_name)

            if x == 'SChan_eta0': extra = '/ h iota0 LAM112^2==2'
            elif x == 'BOX_SChan_eta0_inteference': extra = '/ h iota0 LAM112^2==1'
            elif x == 'SChan_h_SChan_eta0_inteference' : extra = '/ iota0 LAM111^2==1 LAM112^2==1'

            card_name = "HH_loop_sm_twoscalar_%(out_name)s/HH_loop_sm_twoscalar_%(out_name)s" % vars()

            WriteProcCard(card_name, extra)
            WriteExtraModelsCard(card_name)
            WriteRunCard(card_name)
            WriteCustomizeCard(card_name, res_customizecards_out)
            WriteReweightCard(card_name, m, non_res_only=False)



exit()
#generate p p > h h [QCD] / h iota0 LAM112^2==2
#output BSM_hh_SChan_eta0
#
#generate p p > h h [QCD] / h iota0 LAM112^2==1
#output BSM_hh_BOX_SChan_eta0_inteference
#
#generate p p > h h [QCD] / iota0 LAM111^2==1 LAM112^2==1
#output BSM_hh_SChan_h_SChan_eta0_inteference

reweight_out_string='\
change process p p > h h [QCD] / iota0\n\
\n\
launch --rwgt_name=box\n\
  set bsm 6 0.785398\n\
  set bsm 15 0.000000e+00\n\
  set bsm 16 0.000000e+00\n\
\n\
launch --rwgt_name=box_and_schannel_h_1\n\
  set bsm 6 0.785398\n\
  set bsm 15 31.803878252\n\
  set bsm 16 0.000000e+00\n\
\n\
launch --rwgt_name=box_and_schannel_h_2\n\
  set bsm 6 0.785398\n\
  set bsm 15 318.03878252\n\
  set bsm 16 0.000000e+00\n\
\n\
launch --rwgt_name=all\n\
  set bsm 6 0.785398\n\
  set bsm 15 31.803878252\n\
  set bsm 16 31.803878252\n\
\n\
launch --rwgt_name=schannel_H\n\
  set bsm 6 1.570796\n\
  set bsm 15 0.000000e+00\n\
  set bsm 16 31.803878252\n\
\n\
launch --rwgt_name=box_and_schannel_H_1\n\
  set bsm 6 0.785398\n\
  set bsm 15 0.000000e+00\n\
  set bsm 16 31.803878252\n\
\n\
launch --rwgt_name=box_and_schannel_H_2\n\
  set bsm 6 0.785398\n\
  set bsm 15 0.000000e+00\n\
  set bsm 16 318.03878252\n\n'

mass_and_width_dep_string='\
launch --rwgt_name=all_$postfix\n\
  set bsm 6 0.785398\n\
  set bsm 15 31.803878252\n\
  set bsm 16 31.803878252\n\
  set mass 99925 $M\n\
  set DECAY 99925 $W\n\
\n\
launch --rwgt_name=schannel_H_$postfix\n\
  set bsm 6 1.570796\n\
  set bsm 15 0.000000e+00\n\
  set bsm 16 31.803878252\n\
  set mass 99925 $M\n\
  set DECAY 99925 $W\n\
\n\
launch --rwgt_name=box_and_schannel_H_1_$postfix\n\
  set bsm 6 0.785398\n\
  set bsm 15 0.000000e+00\n\
  set bsm 16 31.803878252\n\
  set mass 99925 $M\n\
  set DECAY 99925 $W\n\
\n\
launch --rwgt_name=box_and_schannel_H_2_$postfix\n\
  set bsm 6 0.785398\n\
  set bsm 15 0.000000e+00\n\
  set bsm 16 318.03878252\n\
  set mass 99925 $M\n\
  set DECAY 99925 $W\n\n'

param_base_file=open("../Cards/param_card_BSM.dat","r")

nominal_frac_width = args.width

## for generating samples scale the s-channel H production to ensure we get plenty of events near resonance peak but still a good number of events in the non-resonant part
## this method is quite approximate and could be improved
##kap_sf = (1.5/300.*(mass-300.) + 0.5)*nominal_frac_width/0.05
##kap112_param = 31.80387825*kap_sf

kap112_param = 31.80387825

# set width of generated sample to largest width
param_out_string = param_base_file.read().replace('$W','%g' % (nominal_frac_width*mass)).replace('$M','%g' % mass).replace('$k','%g' % kap112_param)

with open("%s/param_card.dat" % args.output, "w") as param_file:
    param_file.write(param_out_string)

for m in masses:

    widths = np.array([0.001,0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1,0.011,0.012,0.013,0.014,0.15])*m 

    # append exact widths for BM scenarios
    if m==600:
        widths = np.append(widths,[4.979180/m])
    elif m==300:
        widths = np.append(widths,[0.5406704/m])
    
    for width in widths:
        if len(masses)==1 and float(masses[0]) == float(mass):
            reweight_out_string+=mass_and_width_dep_string.replace('$postfix',('RelativeWidt%g' % (width)).replace('.','p')).replace('$W','%g' % width*m).replace('$M','%g' % m)
 
        else: reweight_out_string+=mass_and_width_dep_string.replace('$postfix',('Mass%g_RelativeWidth%g' % (m,width)).replace('.','p')).replace('$W','%g' % width*m).replace('$M','%g' % m)
    
    with open("%s/reweight_card.dat" % args.output, "w") as reweight_file:
        reweight_file.write(reweight_out_string)

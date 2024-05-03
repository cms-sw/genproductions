#!/bin/env python
# how to make mc request soon your grid pack are ready :https://indico.cern.ch/event/321290/contributions/744568/attachments/620452/853749/McM_Tutorial_OPT.pdf
#                                                       https://twiki.cern.ch/twiki/bin/view/CMSPublic/SWGuideSubgroupMC
import os
import glob
import math
import json
import shutil
import stat
import numpy as np
import argparse, optparse
from cp3_llbb.Calculators42HDM.Calc2HDM import *

import logging
LOG_LEVEL = logging.DEBUG
stream = logging.StreamHandler()
stream.setLevel(LOG_LEVEL)
logger = logging.getLogger("ZA GridPack-PRODUCTION")
logger.setLevel(LOG_LEVEL)
logger.addHandler(stream)
try:
    import colorlog
    from colorlog import ColoredFormatter
    formatter = ColoredFormatter(
                "%(log_color)s%(levelname)-8s%(reset)s %(log_color)s%(message)s",
                datefmt=None,
                reset=True,
                log_colors={
                        'DEBUG':    'cyan',
                        'INFO':     'green',
                        'WARNING':  'blue',
                        'ERROR':    'red',
                        'CRITICAL': 'red',
                        },
                secondary_log_colors={},
                style='%'
                )
    stream.setFormatter(formatter)
except ImportError:
    # https://pypi.org/project/colorlog/
    pass

if "CMSSW_BASE" not in os.environ:
    raise RuntimeError("This script needs to be run in a CMSSW environment, with cp3_llbb/Calculators42HDM set up")
CMSSW_Calculators42HDM = os.path.join(os.environ["CMSSW_BASE"], "src", "cp3_llbb", "Calculators42HDM")


def which_points(fullsim=False, benchmarks=False, test=False, dataDir="./data"):
    grid = {}
    grid['example_card'] = [
        ( 500, 300),]
    grid['benchmarks'] = [
        ( 200, 125),
        # ATLAS benchmarks : https://arxiv.org/pdf/1804.01126.pdf
        ( 500, 250), 
        ( 240, 130), 
       #( 500, 300), ( 550, 300), ( 670, 500), # resolved region 
        ( 780, 680), 
        ( 700, 200), 
        ( 510, 130),
        ( 750, 610), 
        ( 800, 140)] # forward region
    grid['fullsim'] = [
        #(MH, MA)
        ( 200, 50), ( 200, 100),
        ( 250, 50), ( 250, 100),
        ( 300, 50), ( 300, 100), ( 300, 200),
        ( 500, 50), ( 500, 100), ( 500, 200), ( 500, 300), ( 500, 400),
        ( 650, 50),
        ( 800, 50), ( 800, 100), ( 800, 200),              ( 800, 400),              ( 800, 700),
        (1000, 50),              (1000, 200),                           (1000, 500), ]
    
    with open(os.path.join(dataDir,'points_1.000000_1.000000.json')) as f:
        d = json.load(f)
        grid['ellipses_rho_1'] = [(mH, mA,) for mA, mH in d]
    
    with open(os.path.join(dataDir,'points_0.500000_0.500000.json')) as f:
        d = json.load(f)
        grid['ellipses_rho_0p5'] = [(mH, mA,) for mA, mH in d]
    griddata = (grid['example_card'] if test else(grid['benchmarks'] if benchmarks else( grid['fullsim'] if fullsim else(grid['fullsim'] + grid['ellipses_rho_1']))))
    return griddata

def mass_to_string(m):
    r = '{:.2f}'.format(m)
    r = r.replace('.', 'p')
    return r

def float_to_mass(m):
    r = '{:.2f}'.format(m)
    return float(r)

def getLHAPDF(lhaid=None, lhapdfsets="DEFAULT", flavourscheme=None):
    if lhapdfsets == 'DEFAULT':
        logger.warning( '''The following ** $DEFAULT_PDF_SETS ** is shortcuts to have the PDF sets automatically added to the run_card at run time to avoid specifying them directly.\n 
                           Be careful this is valid at both LO and NLO !\n''')
        lhaid = '$DEFAULT_PDF_SETS'
    elif lhapdfsets == 'NNPDF31': 
        if flavourscheme == '4FS':
            logger.info( '''No PDFSETS is given !**  LHA PDF set = NNPDF31  # Positive definite 4-FlavourScheme set will be used instead\n 
                            LHA Name = NNPDF31_nnlo_as_0118_nf_4_mc_hessian\n 
                            LHA ID = 325500\n 
                            make sure this is compatible with the generated process in the proc_card and lhaid in the run_card **\n''')
            lhaid= 325500
        else:    
            logger.info( '''No PDFSETS is given !**  LHA PDF set = NNPDF31  # Positive definite set will be used instead\n 
                            LHA Name = NNPDF31_nnlo_as_0118_mc_hessian_pdfas\n 
                            LHA ID = 325300\n 
                            make sure this is compatible with the generated process in the proc_card and lhaid in the run_card **\n''')
            lhaid = 325300
    else:
        if lhaid is None:
            logger.error( "CRITICAL: lhaid can't be NONE ")

    return lhapdfsets, lhaid

def Fix_Yukawa_sector(mh2=None, mh3=None, tanbeta=None, sinbma=None, wh2tobb=None, wh3tobb=None, customizecards=False):
    """
        most of the sm values are changed in 2HDMC and sushi, to avoid having them in the customize_cards !!
        small difference won't make change, but I decided to keep them exactly the same 
        you can check the default values in your model : mg_ver_XXX/models/2HDMtII_NLO/paramaters.py
        ** Fyenman Rule : 
        PartialDecay(h3 > bb ) = (3*mh3**2*tanbeta**2*TH3x3**2*ymb**2*math.sqrt(-4*MB**2*mh3**2 + mh3**4))/(8.*math.pi*vev**2*abs(mh3)**3)
        PartialDecay(h2 > bb~ ) = ((-12*MB**2*TH1x2**2*yb**2 + 3*mh2**2*TH1x2**2*yb**2 - (24*MB**2*tanbeta**2*TH2x2**2*ymb**2)/vev**2 + (6*mh2**2*tanbeta**2*TH2x2**2*ymb**2)/vev**2 - (24*MB**2*tanbeta*TH1x2*TH2x2*yb*ymb*math.sqrt(2))/vev + (6*mh2**2*tanbeta*TH1x2*TH2x2*yb*ymb*math.sqrt(2))/vev)*math.sqrt(-4*MB**2*mh2**2 + mh2**4))/(16.*math.pi*abs(mh2)**3)
    """
    aEWM1= 127.9
    aEW = 1./aEWM1
    Gf = 1.166390e-05

    MB = 4.92 # mb pole mass
    MZ= 9.118760e+01
    MW= math.sqrt(MZ**2/2. + math.sqrt(MZ**4/4. - (aEW*math.pi*MZ**2)/(Gf*math.sqrt(2))))

    ee = 2.*math.sqrt(aEW)*math.sqrt(math.pi)
    sw2 = 1. - MW**2/MZ**2
    sw = math.sqrt(sw2)
    vev = (2.*MW*sw)/ee
    
    TH1x1 = sinbma
    TH1x2 = math.sqrt(1. - sinbma**2)
    TH2x1 = -math.sqrt(1. - sinbma**2)
    TH2x2 = sinbma
    TH3x3 = 1.
    
    const1_A = (3.*mh3**2*tanbeta**2*TH3x3**2*math.sqrt(-4.*MB**2*mh3**2 + mh3**4))
    const2_A = (8.*math.pi*vev**2*abs(mh3)**3)

    ymb_A = math.sqrt((const2_A * wh3tobb)/const1_A)
    yb_A = ((ymb_A*math.sqrt(2))/vev)

    const1_H = (2.*(-12.*MB**2*TH1x2**2 + 3.*mh2**2*TH1x2**2)/(vev**2))
    const2_H = ((24.*MB**2*tanbeta**2*TH2x2**2)/vev**2)
    const3_H = ((6.*mh2**2*tanbeta**2*TH2x2**2)/vev**2)
    const4_H = (((math.sqrt(2))/vev )*((24.*MB**2*tanbeta*TH1x2*TH2x2*math.sqrt(2))/vev))
    const5_H = (((math.sqrt(2))/vev)*((6.*mh2**2*tanbeta*TH1x2*TH2x2*math.sqrt(2))/vev ))
    const6_H = math.sqrt(-4.*MB**2*mh2**2 + mh2**4)
    const7_H = (16.*math.pi*abs(mh2)**3)

    ymb_H= math.sqrt(((const7_H * wh2tobb ) /(const6_H *(const1_H - const2_H + const3_H - const4_H + const5_H))))
    yb_H = ((ymb_H*math.sqrt(2))/vev)

    recalculated_width_A= (MB**2 *const1_A)/const2_A
    width_in_the_banner_A = wh3tobb
    relative_diff_A=abs(recalculated_width_A-width_in_the_banner_A)/recalculated_width_A

    recalculated_width_H = (ymb_H**2*(const1_H - const2_H + const3_H - const4_H + const5_H )*const6_H )/const7_H
    width_in_the_banner_H = wh2tobb
    relative_diff_H=abs(recalculated_width_H-width_in_the_banner_H)/recalculated_width_H
    
    if customizecards:
        #for id, diff in zip([36, 35], [relative_diff_A, relative_diff_H]):
        for id, diff in zip([36], [relative_diff_A]):
            if (diff > 0.05):
                logger.critical('The LO estimate for the width of particle %s ' % id)
                logger.critical('will differs from the one in the banner by %s percent if you do not pass the param_card and you pass the customized cards instead' % (diff*100))
    return ymb_H, ymb_A 

def compute_widths_BR_and_lambdas(mH, mA, mh, tb, process =None, pdfName="DEFAULT", saveprocessinfos=False):
    xsec_ggH = 0.
    xsec_bbH = 0.
    err_integration_ggH = 0.
    err_integration_bbH = 0.
   
    mb = 4.92 # mb(OS) pole mass
    mb__tilde__ = 4.92 # mb~
    MZ= 9.118760e+01
    
    if mA > mH:
        print("MA_{} > MH_{} switching to A->ZH mode!".format(mA, mH))
        mode = 'A'
    elif mH >= mA and mH> 125.:
        print("MA_{} =< MH_{} switching to H->ZA mode!".format(mA, mH))
        mode = 'H'
    elif mH >= mA and mH <= 125.:
        print("MA_{} >= MH_{} && H <= 125. GeV switching to h->ZH mode!".format(mA, mH))
        mode ='h'
    
    sqrts = 13000
    type = 2
    mh = mh
    cba = 0.01  #  cos( beta -alpha) " should not be changed: that's the alignement limit 
    alpha=math.atan(tb)-math.acos(cba)
    sinbma = math.sin(math.atan(tb)-alpha)
    #sinbma = math.sqrt(1 - pow(cba, 2))
    mhc = max(mH, mA)
    m12 = math.sqrt(pow(mhc, 2) * tb / (1 + pow(tb, 2)))
    outputFile = 'madgraphInputs_mH-{}_mA-{}_tb-{}_mode{}.dat'.format(mass_to_string(mH), mass_to_string(mA), mass_to_string( tb), mode)
    cwd = os.getcwd()
    #os.chdir(os.path.join(CMSSW_Calculators42HDM, 'out'))
    os.chdir(CMSSW_Calculators42HDM)
    if process =='ggH':
        muR = mH/2
        muF = muR
    elif process == 'bbH':
        muR = (mA + MZ + mb + mb__tilde__ )
        muF =muR
    res = Calc2HDM(mode = mode, sqrts = sqrts, type = type,
                   tb = tb, m12 = m12, mh = mh, mH = mH, mA = mA, mhc = mhc, sba = sinbma,
                   outputFile = outputFile, muR =muR, muF =muF)
    # A PDF is used, so alpha_s(MZ) is going to be modifie
    # I set the lhapdf to NNPDF31_nnlo_as_0118_nf_4_mc_hessian because it's the same lhapdf used in the default seeting by the genproductions
    # make sure to change it If you're planing to use 5FS PDFSETS !!
    res.setpdf('NNPDF31_nnlo_as_0118_nf_4_mc_hessian')    
    res.computeBR()
    wH = float(res.Hwidth)
    wA = float(res.Awidth)
    l2 = float(res.lambda_2)
    l3 = float(res.lambda_3)
    lR7 = float(res.lambda_7)
    AtoZhBR = res.AtoZhBR
    AtoZHBR = res.AtoZHBR
    AtobbBR = res.AtobbBR
    HtoZABR = res.HtoZABR
    HtobbBR = res.HtobbBR
    wh3tobb = res.wh3tobb
    wh2tobb = res.wh2tobb
    
    if saveprocessinfos:
        sushiCardName = '{}_{}_{}_{}_{}'.format(mass_to_string(mH), mass_to_string(mA), mass_to_string( tb), mass_to_string( muR), mass_to_string( muF))
        #xsec_ggH, err_integration_ggH, err_muRm_ggH, err_muRp_ggH, xsec_bbH, err_integration_bbH, mb_MSscheme_muR=  res.getXsecFromSusHi(sushiCardName=sushiCardName, return_xsc_byComputationOrder = False)
    
    os.chdir(cwd)
    return wH, wA, wh2tobb, wh3tobb, l2, l3, lR7, sinbma, tb , xsec_ggH, err_integration_ggH, xsec_bbH, err_integration_bbH, HtoZABR, AtobbBR, AtoZHBR, HtobbBR

def filename(suffix=None, smpdetails=None, production_mode=None, template=False, mH=None, mA=None, tb=None):
     
    if template:
        cardNames = glob.glob(os.path.join(os.path.dirname(os.path.abspath(__file__)), 'run2Template_cards/template_{}_*'.format(production_mode), '*_{}_{}.dat'.format(smpdetails, suffix)))
        cardName = cardNames[0].split('run2Template_cards/')[-1]

    else:
        if production_mode.startswith('A'):
            masses = '{}_{}_{}'.format(mass_to_string(mA), mass_to_string(mH), mass_to_string( tb))
        else:
            masses = '{}_{}_{}'.format(mass_to_string(mH), mass_to_string(mA), mass_to_string( tb))
        cardName = production_mode+'_' + masses + '_' + smpdetails +'/'+ production_mode +'_' + masses + '_' + smpdetails+ '_' + suffix +'.dat'
    return cardName


def prepare_param_cards(mH=None, mA=None, mh=None, mhc=None, MB=None, l2=None, l3=None, lR7=None, sinbma=None, tb=None, ymb=None, carddir=None, template=None, cardname=None, pass_ymbandmb_toparamcards=False):
    
    if carddir==None:
        carddir = './widths_crosschecks/{}/inputs'.format('run_afterYukawaFix' if pass_ymbandmb_toparamcards else('run_beforeYukawaFix') )
    if cardname==None:
        cardname= "in_param_card_{}_{}_{}.dat".format(mass_to_string(mH), mass_to_string(mA), mass_to_string(tb))
    if template==None:
        template= os.path.join('widths_crosschecks', 'template_param_card.dat')
    
    if not os.path.exists(carddir):
        os.makedirs(carddir)
        
    with open(template, 'r') as inf:
        with open(os.path.join(carddir, cardname), 'w+') as outf:
            for line in inf:
                # BLOCK MASS #
                if " MB " in line and pass_ymbandmb_toparamcards:
                    outf.write('    5 {}   # MB\n'.format(MB))
                elif "mhc" in line and pass_ymbandmb_toparamcards:
                    outf.write('   37 {:.8e}   # mhc\n'.format(mhc))
                # BLOCK YUKAWA # 
                elif "ymb" in line and pass_ymbandmb_toparamcards:
                    outf.write('    5 {}   # ymb\n'.format(ymb))
                # BLOCK FRBLOCK # 
                elif "tanbeta" in line:
                    outf.write('    1 {:.8e}   # tanbeta\n'.format(tb))
                elif "sinbma" in line:
                    outf.write('    2 {:.8e}   # sinbma\n'.format(sinbma))
                # BLOCK HIGGS # 
                elif "l2" in line:
                    outf.write('    1 {:.8e}   # l2\n'.format(l2))
                elif "l3" in line:
                    outf.write('    2 {:.8e}   # l3\n'.format(l3))
                elif "lR7" in line:
                    outf.write('    3 {:.8e}   # lR7\n'.format(lR7))
                # BLOCK MASS #
                elif "mh1" in line:
                    outf.write('   25 {:.8e}   # mh1\n'.format(mh))
                elif "mh2" in line:
                    outf.write('   35 {:.8e}   # mh2\n'.format(mH))
                elif "mh3" in line:
                    outf.write('   36 {:.8e}   # mh3\n'.format(mA))
                else:
                    outf.write(line)
    return

def prepare_cards(mH, mA, mh, mHc, mb, wH, wA, l2, l3, lR7, sinbma, tb, ymb, lhaid, smpdetails, templateDIR, outputDIR, customizecards, production_mode):
    if production_mode.startswith('A'):
        process_name = '{}_{}_{}_{}_{}'.format(production_mode, mass_to_string(mA), mass_to_string(mH), mass_to_string( tb), smpdetails)
    else:
        process_name = '{}_{}_{}_{}_{}'.format(production_mode, mass_to_string(mH), mass_to_string(mA), mass_to_string( tb), smpdetails)
    directory = '{}/'.format(outputDIR) + process_name
    # First: create directory if it doesn't exist
    if not os.path.exists(directory):
        os.makedirs(directory)
    # customizecards
    suffix = 'customizecards'
    if customizecards:
        template_line = 'set param_card'
        with open(os.path.join(templateDIR, filename(suffix, smpdetails, production_mode, template=True)), 'r') as inf:
            with open(os.path.join(outputDIR, filename(suffix, smpdetails, production_mode, mH=mH, mA=mA, tb=tb)), 'w+') as outf:
                for line in inf:
                    if template_line in line and 'higgs 1' in line:
                        outf.write('{} higgs 1 {:.6f}\n'.format(template_line, l2))
                    elif template_line in line and 'higgs 2' in line:
                        outf.write('{} higgs 2 {:.6f}\n'.format(template_line, l3))
                    elif template_line in line and 'higgs 3' in line:
                        outf.write('{} higgs 3 {:.6f}\n'.format(template_line, lR7))
                    
                    elif template_line in line and 'mass 25' in line:
                        outf.write('{} mass 25 {:.2f}\n'.format(template_line, mh))
                    elif template_line in line and 'mass 35' in line:
                        outf.write('{} mass 35 {:.2f}\n'.format(template_line, mH))
                    elif template_line in line and 'mass 36' in line:
                        outf.write('{} mass 36 {:.2f}\n'.format(template_line, mA))
                    elif template_line in line and 'mass 37' in line:
                        outf.write('{} mass 37 {:.2f}\n'.format(template_line, mHc))
                    elif template_line in line and 'mass 5' in line:
                        outf.write('{} mass 5 {:.2f}\n'.format(template_line, mb))
                    
                    elif template_line in line and 'width 36' in line:
                        outf.write('{} width 36 {:.6f}\n'.format(template_line, wA))
                    elif template_line in line and 'width 35' in line:
                        outf.write('{} width 35 {:.6f}\n'.format(template_line, wH))
                    
                    elif template_line in line and 'frblock 1' in line:
                        outf.write('{} frblock 1 {:.6f}\n'.format(template_line, tb))
                    elif template_line in line and 'frblock 2' in line:
                        outf.write('{} frblock 2 {:.6f}\n'.format(template_line, sinbma))
                    
                    elif template_line in line and 'yukawa 5' in line:
                        outf.write('{} yukawa 5 {:.6f}\n'.format(template_line, ymb))
                    
                    else:
                        outf.write(line)
                outf.write('# higgs 1: lambda 2\n')
                outf.write('# higgs 2: lambda 3\n')
                outf.write('# higgs 3: lambda Real 7\n')
                outf.write('# mass 25: mh\n')
                outf.write('# mass 35: mH\n')
                outf.write('# mass 36: mA\n')
                outf.write('# mass 37: mHc\n')
                outf.write('# mass 5: MB (mb pole mass )\n')
                outf.write('# width 36: wA\n')
                outf.write('# width 35: wH\n')
                outf.write('# frblock 1: tb\n')
                outf.write('# frblock 2: sinbma\n')
                outf.write('# yukawa 5: ymb (bottom yukawa coupling)\n')
    # extramodels: no change needed
    suffix = 'extramodels'
    shutil.copyfile(os.path.join(templateDIR, filename(suffix, smpdetails, production_mode, template=True)), os.path.join(outputDIR, filename(suffix, smpdetails, production_mode, mH=mH, mA=mA, tb=tb)))
    # proc_card: change the output name
    suffix = 'proc_card'
    with open(os.path.join(templateDIR, filename(suffix, smpdetails, production_mode, template=True)), 'r') as inf:
        with open(os.path.join(outputDIR, filename(suffix, smpdetails, production_mode, mH=mH, mA=mA, tb=tb)), 'w+') as outf:
            for line in inf:
                if ('output') in line:
                    if production_mode.startswith('A'):
                        outf.write('output {}_{}_{}_{}_{} -nojpeg'.format(production_mode, mass_to_string(mA), mass_to_string(mH), mass_to_string(tb), smpdetails))
                    else:
                        outf.write('output {}_{}_{}_{}_{} -nojpeg'.format(production_mode, mass_to_string(mH), mass_to_string(mA), mass_to_string(tb), smpdetails))
                else:
                    outf.write(line)
    suffix = 'run_card'
    suf = '  ' if 'ggH' in smpdetails else ' '
    with open(os.path.join(templateDIR, filename(suffix, smpdetails, production_mode, template=True)), 'r') as inf:
         with open(os.path.join(outputDIR, filename(suffix, smpdetails, production_mode, mH=mH, mA=mA, tb=tb)), 'w+') as outf:
             for line in inf:
                 if 'lhaid' in line:
                     outf.write('{}{} = lhaid ! if pdlabel=lhapdf, this is the lhapdf number. Only\n'.format(suf, lhaid))
                     outf.write('       ! numbers for central PDF sets are allowed. Can be a list;\n')
                     outf.write('       ! PDF sets beyond the first are included via reweighting.\n')
                     if lhaid == '$DEFAULT_PDF_SETS':
                         outf.write('{}$DEFAULT_PDF_MEMBERS  = reweight_PDF ! Reweight to get PDF uncertainty. Should be a\n'.format(suf))
                         outf.write('                            ! list booleans of equal length to lhaid to specify for\n')
                         outf.write('                            !  which PDF set to include the uncertainties.\n')
                 else:
                     outf.write(line)
    suffix = 'madspin_card'
    #if 'ggH' not in smpdetails:
    madspin_card = os.path.join(templateDIR, filename(suffix, smpdetails, production_mode, template=True))
    if os.path.exists(madspin_card):
        shutil.copyfile(madspin_card, os.path.join(outputDIR, filename(suffix, smpdetails, production_mode, mH=mH, mA=mA, tb=tb)))
    mass_mother = ( mA if production_mode.startswith('A') else (mH))
    mass_daughter = ( mH if production_mode.startswith('A') else (mA))
    print ('MG5 files prepared in {}/{}_{}_{}_{}_{}'.format(outputDIR, production_mode, mass_to_string(mass_mother), mass_to_string(mass_daughter), mass_to_string(tb), smpdetails))
    return

def prepare_all_MG5_cards(process=None, flavourscheme=None, lhapdfsets=None, lhaid=None, queue="condor_spool", test=False, benchmarks= False, fullsim=False, gridpointsdata=None, templateDIR=None, saveprocessinfos=False, customizecards=False, mode=None):
    
    griddata = which_points(fullsim, benchmarks, test, gridpointsdata)
    suffix= ('example' if test else( 'benchmarks' if benchmarks else('fullsim' if fullsim else('all'))))
    outputDIR = ( 'example_cards' if test else('benchmarks' if benchmarks else('fullsim' if fullsim else('PrivateProd_run2'))))
    production_mode = 'HToZATo2L2B' if mode =='H' else ( 'AToZHTo2L2B' if mode=='A' else ('hToZATo2L2B'))
    scenario = 'HToZA' if mode =='H' else ( 'AToZH' if mode=='A' else ('hToZA'))
    
    mh=125.
    mb = 4.92 # mb(OS) https://arxiv.org/pdf/1610.07922.pdf page 7
    pdfName, lhaid = getLHAPDF(lhapdfsets=lhapdfsets, lhaid=lhaid, flavourscheme=flavourscheme)
  
    if process=='ggH':
        OrderOfcomputation= 'LO'
        smpdetails= 'ggH_TuneCP5_13TeV_pythia8'
        tb_list = [1.5]
        #tb_list =[ 0.5 ,  1.5 ,  4.5,  8. ,  20. ]
    else:
        OrderOfcomputation= 'NLO'
        smpdetails= 'bbH4F_TuneCP5_13TeV-amcatnlo_pythia8'
        tb_list = [20.0]
        #tb_list = [ 0.5 ,  1.5 ,  4.5,  8. ,  20. ]

    logger.info(" Please NOTE : For ggH process the Z decay is included in the Matrix Elemet, and h3 should be added in pythia8 gen-fragment or in madspin card !")
    logger.info( " You choose {} to be generated for your gridpack production ! ".format( "customize_card.dat" if customizecards else "param_card.dat"))
    with open('prepare_{}_{}_{}_gridpacks.sh'.format(suffix, OrderOfcomputation.lower(), scenario.lower()), 'w+') as outf, open('run_madwidths.sh', 'w+') as outf2, open('run_yukawa_to_mbonshell.sh', 'w+') as outf3:
        outf2.write('import model 2HDMtII_NLO\n')
        
        outf.write('# Notes:\n')
        outf.write('# - This script was generated by prepare_MG5_cards.py\n')
        outf.write('# - It should be run from the genproductions/bin/MadGraph5_aMCatNLO directory in a clean environment and will submit the gridpack generation for all points.\n')
        outf.write('# - each gridpack generation should take about 20 minutes\n')
        outf.write('set -x\n')
        outf.write("GenDIR='genproductions'\n")
        outf.write('if [[ ! -d "$GenDIR" ]]; then\n')
        outf.write('    git clone  -o origin https://github.com/cms-sw/genproductions.git\n')
        outf.write('    git remote add upstream git@github.com:kjaffel/genproductions.git\n')
        outf.write('fi\n')
        outf.write('pushd genproductions\n')
        #outf.write('git checkout UL2019\n')
        #outf.write('git checkout mg27x\n')
        outf.write('git checkout master\n')
        outf.write('git pull\n')
        outf.write('pushd bin/MadGraph5_aMCatNLO/cards/production/13TeV/\n')
        # Copy only the example cards for review  !
        if test:
            outf.write("CardsDIR='{}_ggfusion_b-associatedproduction'\n".format(production_mode))
            outf.write('if [[ ! -d "$CardsDIR" ]]; then\n')
            outf.write('    mkdir {}_ggfusion_b-associatedproduction/\n'.format(production_mode))
            outf.write('fi\n')
            outf.write('cp -r ../../../../../../example_cards {}_ggfusion_b-associatedproduction/.\n'.format(production_mode))
        else:
            outf.write('ln -s -d ../../../../../../{}/ .\n'.format('benchmarks' if benchmarks else('fullsim'if fullsim else('PrivateProd_run2'))))
        outf.write('popd\n')
        outf.write('pushd bin/MadGraph5_aMCatNLO\n')

        if 'condor' in queue:
            outf.write('./submit_condor_gridpack_generation.sh\n')
        else:
            outf.write('# kEEP IN MIND : IF You are submitting from lxplus and the local directory is not on AFS \n')
            outf.write('# Automatically will switch to condor spool mode.\n')
            outf.write('# So you have to call : ./submit_condor_gridpack_generation.sh \n')
        outf.write('# Now for the real gridpack production\n')
        
        datasetName_xsc_file =os.path.join(gridpointsdata,"list_{}_{}_{}_datasetnames.txt".format(suffix, process, scenario))
        if saveprocessinfos:
            if os.path.exists(datasetName_xsc_file):
                os.remove(datasetName_xsc_file)
            f= open(datasetName_xsc_file,"a")
            precision =("NNLO" if process=='bbH' else"NLO")
            daughter = ('A ' if mode =='H' else( 'H'))
            f.write('DatasetName  Sushi_xsc@{}[pb]  Sushi_xsc_err[pb]  BR({} -> Z{})  BR({} -> bb)  Ymb,H[GeV]  Ymb,A[GeV]  Partialwidth(H ->bb)[GeV]  Partialwidth(A ->bb)[GeV]  Totalwidth,H[GeV]  Totalwidth,A[GeV]  TotalWidth_H/Mass_H [%]  TotalWidth_A/Mass_A [%]\n'.format(precision, mode, daughter, daughter))
        
        for i, j in griddata:
            if mode == 'H':
                H= i
                A= j 
            else:
                H = j
                A = i
            mH = float_to_mass(H)
            mA = float_to_mass(A)
            mHc= max(mH, mA)
            mother_mass = ( mA if mode=='A' else ( mH))
            daughter_mass = (mH  if mode=='A' else ( mA))
            if mH < 125.:
                logger.debug("2HDMC can simulate points where mH < smHiggs for {} scenario ".format(scenario))
                print('# skipping point (mH, mA) = ({}, {})'.format(mH, mA))
                continue
            for tb in tb_list:
                wH, wA, wh2tobb, wh3tobb, l2, l3, lR7, sinbma, tb, xsec_ggH, err_integration_ggH, xsec_bbH, err_integration_bbH, HtoZABR, AtobbBR, AtoZHBR, HtobbBR = compute_widths_BR_and_lambdas(mH, mA, mh, tb, process = process, pdfName=pdfName, saveprocessinfos=saveprocessinfos)
                ymb_H, ymb_A = Fix_Yukawa_sector(H, A, tb, sinbma, wh2tobb, wh3tobb, customizecards)
                logger.info( 'ymb_H: {}       ymb_A: {} '.format(ymb_H, ymb_A))
                prepare_cards(mH, mA, mh, mHc, mb, wH, wA, l2, l3, lR7, sinbma, tb, ymb_A, lhaid, smpdetails, templateDIR, outputDIR, customizecards, production_mode)
                
                cardname = "{}_{}_{}_{}_{}".format(production_mode, mass_to_string(mother_mass), mass_to_string(daughter_mass), mass_to_string(tb), smpdetails)
                if wH/mH > 0.1 or wA/mA > 0.1:
                    logger.critical(' width/mass >> 10 % Narrow-width approximation may not be valid for mass point: {}'.format(cardname)) 
                if wH/mH < 10e-8 or wA/mA < 10e-8:
                    logger.critical(' width/mass < 10e-8 : Slows down the code and can lead to numerical instability')
                if saveprocessinfos:
                    xsc = (xsec_ggH if process=='ggH' else(xsec_bbH))
                    err = (err_integration_ggH if process=='ggH' else( err_integration_bbH))
                    mother_decay = ( AtoZHBR if mode=='A' else ( HtoZABR))
                    daughter_decay = ( HtobbBR if mode == 'A' else( AtobbBR))
                    f.write('{} {} {} {} {} {} {} {} {} {} {} {} {}\n'.format(cardname, xsc ,err, mother_decay, daughter_decay, ymb_H, ymb_A, wh2tobb, wh3tobb, wH, wA, wH*100/mH, wA*100/mA))
                loc = ('{}_ggfusion_b-associatedproduction/example_cards'.format(production_mode) if test else ('benchmarks' if benchmarks else('fullsim'if fullsim else('PrivateProd_run2'))))
                carddir ="cards/production/13TeV/{}/{}".format(loc, cardname)
                workqueue='{}'.format(queue)
                scram_arch="slc7_amd64_gcc820"
                cmssw_version="CMSSW_11_2_0_pre7"
                
                # https://github.com/cms-sw/genproductions/blob/mg27x/bin/MadGraph5_aMCatNLO/submit_condor_gridpack_generation.sh
                # bash gridpack_generation.sh ${cardname} ${carddir} ${workqueue} ALL ${scram_arch} ${cmssw_version}
                #outf.write( "./gridpack_generation.sh {} {} {} ALL {} {}\n".format(cardname, carddir, workqueue, scram_arch, cmssw_version ))
                outf.write( "./gridpack_generation.sh {} {} {} \n".format(cardname, carddir, workqueue))
                outf.write( "rm -rf {} \n".format(cardname))
                process_name = '{}_{}_{}_{}_{}'.format(production_mode, mass_to_string(mother_mass), mass_to_string(daughter_mass), mass_to_string( tb), smpdetails)
                directory = '{}/'.format(outputDIR) + process_name
                if not customizecards:
                    param_card_decayh2= './{}/{}_param_card.dat.decay_h2'.format(directory, cardname)
                    param_card_decayh3z= './{}/{}_param_card.dat.decay_h3z'.format(directory, cardname)
                    template = os.path.join(templateDIR, filename("param_card", smpdetails, production_mode, template=True))
                    # keep them seprate just because they're using different ymb in the param_card!
                    # https://arxiv.org/pdf/1402.1178.pdf page 21
                    prepare_param_cards(mH, mA, mh, mHc, mb, l2, l3, lR7, sinbma, tb, ymb_H, carddir=directory, template=template, cardname='{}_param_card.dat.decay_h2'.format(cardname), pass_ymbandmb_toparamcards=True)
                    outf2.write('compute_widths 35 --path={} --output={} --body_decay=2 --min_br=0.000625\n'.format(param_card_decayh2, param_card_decayh2))
                     
                    prepare_param_cards(mH, mA, mh, mHc, mb, l2, l3, lR7, sinbma, tb, ymb_A, carddir=directory, template=template, cardname='{}_param_card.dat.decay_h3z'.format(cardname), pass_ymbandmb_toparamcards=True)
                    outf2.write('compute_widths 23 36 37 --path={} --output={} --body_decay=2 --min_br=0.000625\n'.format(param_card_decayh3z, param_card_decayh3z))
                    outf3.write('python set_bottomYukawa_coupling_onshell.py --param_card1 {} --param_card2 {}\n'.format(param_card_decayh2, param_card_decayh3z))
                #if fullsim:
                #    outf.write('mv {}* /afs/cern.ch/user/k/kjaffel/public/{}_run2gridpacks/\n'.format(cardname, production_mode))
        
        outf.write('# uncomment these lines to Add more commits by pushing to the {}_run2Cards branch on kjaffel/genproductions.!\n'.format(production_mode))
        outf.write('# pushd cards/production/13TeV/\n')
        outf.write('# git checkout -b {}_run2Cards\n'.format(production_mode))
        outf.write('# git add {}_ggfusion_b-associatedproduction\n'.format(production_mode))
        outf.write("# git commit -m  'update {} cards'\n".format(production_mode))
        outf.write('# git push upstream {}_run2Cards\n'.format(production_mode))

        outf.write('set +x\n')
        
        if saveprocessinfos:
            f.close()
     
    os.chmod('prepare_{}_{}_gridpacks.sh'.format(suffix, OrderOfcomputation.lower()), os.stat('prepare_{}_{}_gridpacks.sh'.format(suffix, OrderOfcomputation.lower())).st_mode | stat.S_IXUSR)
    os.chmod('run_madwidths.sh', os.stat('run_madwidths.sh').st_mode | stat.S_IXUSR)
    os.chmod('run_yukawa_to_mbonshell.sh', os.stat('run_yukawa_to_mbonshell.sh').st_mode | stat.S_IXUSR)
    if not customizecards:
        logger.warning(' Please run_madwidths.sh to overwrite the param_card before you lunch your gridpack generations !')
        logger.warning('cd MG5_aMC_vX_X_X')
        logger.warning('./bin/mg5_aMC run_madwidths.sh')
        logger.warning('bash ./run_yukawa_to_mbonshell.sh')
    print ('All commands prepared in ./prepare_{}_{}_{}_gridpacks.sh'.format(suffix, OrderOfcomputation.lower(), scenario.lower()))

if __name__ == '__main__':

    parser = argparse.ArgumentParser(description='Preparing Grid Pack for 2HDM H/A-> Z(->ll) A/H(->bb) for full run2 Ultra Legacy Campaigns', formatter_class=argparse.RawTextHelpFormatter)
    parser.add_argument('-q', '--queue', action='store', dest= 'queue', default='1nh', choices=['condor', 'condor_spool', '1nh', 'slurm'], type=str, help='more options : [pbs|sge|condor|lsf|ge|slurm|htcaas|htcaas2] Use for cluster run only')
    parser.add_argument('-m', '--mode', action='store', default='H', choices=['H','A'],  help='production mode : H-> ZA or A-> ZH')
    parser.add_argument('-s', '--flavourscheme', action='store', default='4FS', choices=[None, '4FS', '5FS'],  help='production shceme')
    parser.add_argument('--customizecards', action='store_true', default=False, help='use customize_card.dat or param_card.dat for gridpackproduction, note that for the param_card you have to compute width and the BR, details in README.md ')
    parser.add_argument('-p', '--process', action='store', default='ggH', choices=['ggH', 'bbH'], help='production shceme')
    parser.add_argument("--gridpoints", default="./data", help="Directory with grid points data in JSON format")
    parser.add_argument('--test', action='store_true', help='Generate 1 set of cards stored by default in  example_cards/')
    parser.add_argument('--benchmarks', action='store_true', help='Generate 3benchmarks scenarios for at high and low mass region of (MH, MA) for 5 different tb values, cards stored by default in  benchmarks/')
    parser.add_argument('--fullsim', action='store_true', help='Generate 21 signal mass points saved by default in fullsim/')
    parser.add_argument('--saveprocessinfos', action='store_true', default= True, help='store xsc for each process in .txt file in ./gridpoints')
    parser.add_argument('--templates', required=True, help='''Directory with 5 templates cards for your requested process:\n 
                            [ template_customizecards.dat, template_extramodels.dat, template_madspin_card.dat,  template_proc_card.dat, template_run_card.dat]\n''')
    # If you are not sure about your pdf sets setting, better use DEFAULT !
    parser.add_argument('-pdf', '--lhapdfsets',   action='store', dest='lhapdfsets', default='DEFAULT', type=str, 
                    help=('Few links may help you to make the choice:\n'
                          'https://twiki.cern.ch/twiki/bin/view/CMS/QuickGuideMadGraph5aMCatNLO#PDF_Choice_for_2017_production\n'
                          'https://monte-carlo-production-tools.gitbook.io/project/mccontact/info-for-mc-production-for-ultra-legacy-campaigns-2016-2017-2018\n'))
    parser.add_argument('-lhaid', '--lhaid', action='store',dest='lhaid', type=int, help = 'LHAPDF ID(ver 6.3.0) : Full list here : https://lhapdf.hepforge.org/pdfsets')
    options = parser.parse_args()
    
    prepare_all_MG5_cards(process=options.process, flavourscheme=options.flavourscheme, lhapdfsets=options.lhapdfsets, lhaid=options.lhaid, test=options.test, benchmarks=options.benchmarks, fullsim=options.fullsim, queue=options.queue, gridpointsdata=options.gridpoints, templateDIR=options.templates, saveprocessinfos=options.saveprocessinfos, customizecards=options.customizecards, mode=options.mode)

#!/bin/env python
# how to make mc request soon your grid pack are ready :https://indico.cern.ch/event/321290/contributions/744568/attachments/620452/853749/McM_Tutorial_OPT.pdf
#                                                       https://twiki.cern.ch/twiki/bin/view/CMSPublic/SWGuideSubgroupMC
import os
import math
import json
import shutil
import stat
import numpy as np
import argparse, optparse
from cp3_llbb.Calculators42HDM.Calc2HDM import *

import logging
LOG_LEVEL = logging.DEBUG
LOGFORMAT = "  %(log_color)s%(levelname)-8s%(reset)s | %(log_color)s%(message)s%(reset)s"
import colorlog
from colorlog import ColoredFormatter
logging.root.setLevel(LOG_LEVEL)
formatter = ColoredFormatter(LOGFORMAT)
stream = logging.StreamHandler()
stream.setLevel(LOG_LEVEL)
stream.setFormatter(formatter)
logger = logging.getLogger("ZA GridPack-PRODUCTION")
logger.setLevel(LOG_LEVEL)
logger.addHandler(stream)

CMSSW_Calculators42HDM = '/home/ucl/cp3/kjaffel/ZAPrivateProduction/CMSSW_10_2_22/src/cp3_llbb/Calculators42HDM'
MASSES_TEMPLATE='200_50_1'

parser = argparse.ArgumentParser(description='Preparing Grid Pack for 2HDM H/A-> Z(->ll) A/H(->bb) for full run2 Ultra Legacy Campaigns', 
                                formatter_class=argparse.RawTextHelpFormatter,
                                usage = "")
parser.add_argument('-q', '--queue', action='store',
                                    dest= 'queue',
                                    default='1nh',
                                    choices=['condor', 'condor_spool', '1nh'],
                                    type=str,
                                    help='more options : [pbs|sge|condor|lsf|ge|slurm|htcaas|htcaas2] Use for cluster run only')
parser.add_argument('-s', '--scheme', action='store', 
                                      default='', 
                                      choices=['', '4FS', '5FS'], 
                                      help='production shceme')
parser.add_argument('-o', '--order', action='store', 
                                      default='NLO', 
                                      choices=['LO', 'NLO'], 
                                      help='production shceme')
parser.add_argument('--test', action='store_true', 
                                      help='Generate 1 set of cards stored by default in  example_cards/')
# If you are not sure about your pdf sets setting, better use DEFAULT !
parser.add_argument('-pdf', '--lhapdfsets',   action='store', 
                                              dest='lhapdfsets', 
                                              default='DEFAULT', 
                                              type=str, 
                                              help=''' Few links may help you to make the choice:'\n
                                                       https://twiki.cern.ch/twiki/bin/view/CMS/QuickGuideMadGraph5aMCatNLO#PDF_Choice_for_2017_production\n
                                                       https://monte-carlo-production-tools.gitbook.io/project/mccontact/info-for-mc-production-for-ultra-legacy-campaigns-2016-2017-2018\n
                                                    ''')
parser.add_argument('-lhaid', '--lhaid', action='store',
                                         dest='lhaid',
                                         type=int,
                                         help = 'LHAPDF ID(ver 6.3.0) : Full list here : https://lhapdf.hepforge.org/pdfsets')
options = parser.parse_args()
options.order= options.order.upper()
options.scheme= options.scheme.upper()

def which_points(grid):
    grid['example_card'] = [
        ( 500, 300),
        ]
    grid['fullsim'] = [
        #(MH, MA)
        ( 500, 300),
        ( 200, 50), ( 200, 100),
        ( 250, 50), ( 250, 100),
        ( 300, 50), ( 300, 100), ( 300, 200),
        ( 500, 50), ( 500, 100), ( 500, 200), ( 500, 300), ( 500, 400),
        ( 650, 50),
        ( 800, 50), ( 800, 100), ( 800, 200),              ( 800, 400),              ( 800, 700),
        (1000, 50),              (1000, 200),                           (1000, 500),
    ]
    
    with open('data/points_1.000000_1.000000.json') as f:
        d = json.load(f)
        # format the output into tuples
        grid['ellipses_rho_1'] = [(mH, mA,) for mA, mH in d]
    
    with open('data/points_0.500000_0.500000.json') as f:
        d = json.load(f)
        # format the output into tuples
        grid['ellipses_rho_0p5'] = [(mH, mA,) for mA, mH in d]
    return grid

def mass_to_string(m):
    r = '{:.2f}'.format(m)
    r = r.replace('.', 'p')
    return r

def float_to_mass(m):
    r = '{:.2f}'.format(m)
    return float(r)

def compute_widths_BR_and_lambdas(mH, mA, mh, tb):
    global options
    global lhaid

    mode = 'H'
    if mA > mH:
        logger.info("MA_{} > MH_{} switching to A->ZH mode!".format(mA, mH))
        mode = 'A'
    elif mH >= mA and mH> 125.:
        logger.info("MA_{} =< MH_{} switching to H->ZA mode!".format(mA, mH))
        mode = 'H'
    elif mH >= mA and mH <= 125.:
        logger.info("MA_{} >= MH_{} && H <= 125. GeV switching to h->ZH mode!".format(mA, mH))
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
    outputFile = 'madgraphInputs_mH-{}_mA-{}_tb-{}.dat'.format(mass_to_string(mH), mass_to_string(mA), mass_to_string( tb))
    cwd = os.getcwd()
    #os.chdir(os.path.join(CMSSW_Calculators42HDM, 'out'))
    os.chdir(CMSSW_Calculators42HDM)
    res = Calc2HDM(mode = mode, sqrts = sqrts, type = type,
                   tb = tb, m12 = m12, mh = mh, mH = mH, mA = mA, mhc = mhc, sba = sinbma,
                   outputFile = outputFile, muR = 1., muF = 1.)
   
    if options.lhapdfsets == 'DEFAULT':
        logger.warning( 'The following ** $DEFAULT_PDF_SETS ** is shortcuts to have the PDF sets automatically added to the run_card at run time to avoid specifying them directly\n. Be careful this is valid at both LO and NLO !\n')
        lhaid = '$DEFAULT_PDF_SETS'
    elif options.lhapdfsets == 'NNPDF31': 
        if options.scheme == '4FS':
            logger.info( '''No PDFSETS is given !**  LHA PDF set = NNPDF31  # Positive definite 4Flavor-scheme set will be used instead\n 
                            LHA Name = NNPDF31_nnlo_as_0118_nf_4_mc_hessian\n 
                            LHA ID = 325500\n 
                            make sure this is compatible with the generated process in the proc_card and lhaid in the run_card **\n'''
                )
            res.setpdf('NNPDF31_nnlo_as_0118_nf_4_mc_hessian')
            lhaid= 325500
        else:    
            logger.info( '''No PDFSETS is given !**  LHA PDF set = NNPDF31  # Positive definite set will be used instead\n 
                            LHA Name = NNPDF31_nnlo_as_0118_mc_hessian_pdfas\n 
                            LHA ID = 325300\n 
                            make sure this is compatible with the generated process in the proc_card and lhaid in the run_card **\n'''
                )
            res.setpdf('NNPDF31_nnlo_as_0118_mc_hessian_pdfas')
            lhaid = 325300
    else:
        res.setpdf(options.lhapdfsets )
        lhaid = options.lhaid
        if lhaid is None:
            logger.error( "CRITICAL: lhaid can't be NONE ")
    
    res.computeBR()
    wH = float(res.Hwidth)
    wA = float(res.Awidth)
    l2 = float(res.lambda_2)
    l3 = float(res.lambda_3)
    lR7 = float(res.lambda_7)
    AtoZhBR = res.AtoZhBR
    AtobbBR = res.AtobbBR
    HtoZABR = res.HtoZABR
    HtobbBR = res.HtobbBR
    os.chdir(cwd)
    return wH, wA, l2, l3, lR7, sinbma, tb #, AtoZhBR, AtobbBR, HtoZABR, HtobbBR

def filename(suffix, template=False, mH=None, mA=None, tb=None):
    tmp = ''
    masses = ''
    if template:
        tmp = 'template_'
        masses = MASSES_TEMPLATE
    else:
        masses = '{}_{}_{}'.format(mass_to_string(mH), mass_to_string(mA), mass_to_string( tb))
    return '{}/'.format(ouputDIR) + tmp + 'HToZATo2L2B_' + masses + '_' + smpdetails +'/HToZATo2L2B_' + masses + '_' + smpdetails+ '_' + suffix +'.dat'

def prepare_cards(mH, mA, mh, wH, wA, l2, l3, lR7, sinbma, tb):
    process_name = 'HToZATo2L2B_{}_{}_{}_{}'.format(mass_to_string(mH), mass_to_string(mA), mass_to_string( tb), smpdetails)
    directory = '{}/'.format(ouputDIR) + process_name
    # First: create directory if it doesn't exist
    if not os.path.exists(directory):
        os.makedirs(directory)
    # customizecards
    suffix = 'customizecards'
    template_line = 'set param_card'
    with open(filename(suffix, template=True), 'r') as inf:
        with open(filename(suffix, mH=mH, mA=mA, tb=tb), 'w+') as outf:
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
                
                elif template_line in line and 'width 36' in line:
                    outf.write('{} width 36 Auto\n'.format(template_line))
                elif template_line in line and 'width 35' in line:
                    outf.write('{} width 35 {:.6f}\n'.format(template_line, wH))
                
                elif template_line in line and 'frblock 1' in line:
                    outf.write('{} frblock 1 {:.6f}\n'.format(template_line, tb))
                elif template_line in line and 'frblock 2' in line:
                    outf.write('{} frblock 2 {:.6f}\n'.format(template_line, sinbma))
                
                else:
                    outf.write(line)
            outf.write('# higgs 1: lambda 2\n')
            outf.write('# higgs 2: lambda 3\n')
            outf.write('# higgs 3: lambda Real 7\n')
            outf.write('# mass 25: mh\n')
            outf.write('# mass 36: mA\n')
            outf.write('# mass 35: mH\n')
            outf.write('# width 36: wA\n')
            outf.write('# width 35: wH\n')
            outf.write('# frblock 1: tb\n')
            outf.write('# frblock 2: sinbma\n')
    # extramodels: no change needed
    suffix = 'extramodels'
    shutil.copyfile(filename(suffix, template=True), filename(suffix, mH=mH, mA=mA, tb=tb))
    # proc_card: change the output name
    suffix = 'proc_card'
    with open(filename(suffix, template=True), 'r') as inf:
        with open(filename(suffix, mH=mH, mA=mA, tb=tb), 'w+') as outf:
            for line in inf:
                if ('output HToZATo2L2B_' + MASSES_TEMPLATE) in line:
                    outf.write('output HToZATo2L2B_{}_{}_{}_{} -nojpeg'.format(mass_to_string(mH), mass_to_string(mA), mass_to_string(tb), smpdetails))
                else:
                    outf.write(line)
    suffix = 'run_card'
    with open(filename(suffix, template=True), 'r') as inf:
         with open(filename(suffix, mH=mH, mA=mA, tb=tb), 'w+') as outf:
             for line in inf:
                 if 'lhaid' in line:
                     outf.write('{} = lhaid ! if pdlabel=lhapdf, this is the lhapdf number\n'.format(lhaid))
                     if lhaid is '$DEFAULT_PDF_SETS':
                         outf.write('$DEFAULT_PDF_MEMBERS  = reweight_PDF\n')
                 else:
                     outf.write(line)
    suffix = 'madspin_card'
    shutil.copyfile(filename(suffix, template=True), filename(suffix, mH=mH, mA=mA, tb=tb))
    print ('MG5 files prepared in {}/HToZATo2L2B_{}_{}_{}_{}'.format(ouputDIR, mass_to_string(mH), mass_to_string(mA), mass_to_string(tb), smpdetails))
    return

def prepare_all_MG5_cards():
    grid = {}
    grid = which_points(grid)
    griddata = 'example_card' if options.test else('fullsim')
    suffix= ('example' if options.test else('all'))
    if options.test:
        if options.order =='LO':
            tb_list = [1.5] 
        else:
            tb_list = [20.0]
    else:
        tb_list = [0.5,1.0,1.5,2.0,5.0,6.0,8.0,10.0,15.0,20.0,30.0,40.0,50.0]

    mh=125.
    global smpdetails
    global ouputDIR
    ouputDIR = ( 'example_cards' if options.test else( 'PrivateProd_run2'))
    if options.order=='LO':
        smpdetails= 'ggH_TuneCP5_13TeV_pythia8'
    else:
        smpdetails= 'bbH4F_TuneCP5_13TeV-amcatnlo_pythia8'
    with open('prepare_{}_{}_gridpacks.sh'.format(suffix, options.order.lower()), 'w+') as outf:
        outf.write('# Please run the following on lxplus\n')
        outf.write('# Notes:\n')
        outf.write('# - you must not have setup any cmsenv\n')
        outf.write('# - each gridpack generation should take about 20 minutes\n')
        outf.write('set -x\n')
        outf.write("ZADIR= 'ZAPrivateProduction'\n")
        outf.write('if [[ ! -d "$ZADIR" ]]; then\n')
        outf.write('    git clone -o upstream git@github.com:cp3-llbb/ZAPrivateProduction.git\n')
        outf.write('    git remote add origin git@github.com:kjaffel/ZAPrivateProduction.git\n')
        outf.write('fi\n')
        outf.write('pushd ZAPrivateProduction\n')
        outf.write('git fetch origin\n')
        outf.write('git checkout origin/master\n')
        outf.write("GenDIR= 'genproductions'\n")
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
        if options.test:
            outf.write("CardsDIR= 'HToZATo2L2B_ggfusion_b-associatedproduction'\n")
            outf.write('if [[ ! -d "$CardsDIR" ]]; then\n')
            outf.write('    mkdir HToZATo2L2B_ggfusion_b-associatedproduction/\n')
            outf.write('fi\n')
            outf.write('cp -r ../../../../../../example_cards HToZATo2L2B_ggfusion_b-associatedproduction/.\n')
        else:
            outf.write('ln -s -d ../../../../../../PrivateProd_run2/ .\n')
        outf.write('popd\n')
        outf.write('pushd bin/MadGraph5_aMCatNLO\n')

        if 'condor' in options.queue:
            outf.write('./submit_condor_gridpack_generation.sh\n')
        else:
            outf.write('# kEEP IN MIND : IF You are submitting from lxplus and the local directory is not on AFS \n')
            outf.write('# Automatically will switch to condor spool mode.\n')
            outf.write('# So you have to call : ./submit_condor_gridpack_generation.sh \n')
        outf.write('# Now for the real gridpack production\n')
        #for H, A in (grid['fullsim'] + grid['ellipses_rho_1']): # TODO 
        for H, A in (grid[griddata]):
            mH = float_to_mass(H)
            mA = float_to_mass(A)
            #FIXME : I DON'T SEE A RASON FOR SKIPPING THESE POINTS
            #if mH < 125.:
            #    s = '# skipping point (mH, mA) = ({}, {})'.format(mH, mA)
            #    print (s)
            #    outf.write(s + '\n')
            #    continue
            for tb in tb_list:
                wH, wA, l2, l3, lR7, sinbma, tb = compute_widths_BR_and_lambdas(mH, mA, mh, tb)
                prepare_cards(mH, mA, mh, wH, wA, l2, l3, lR7, sinbma, tb)
                
                name = "HToZATo2L2B_{}_{}_{}_{}".format(mass_to_string(mH), mass_to_string(mA), mass_to_string(tb), smpdetails)
                loc = ('HToZATo2L2B_ggfusion_b-associatedproduction/example_cards' if options.test else ('PrivateProd_run2') )
                carddir ="cards/production/13TeV/{}/{}".format(loc, name)
                workqueue='{}'.format(options.queue)
                scram_arch="slc7_amd64_gcc820"
                cmssw_version="CMSSW_11_2_0_pre7"
                
                # https://github.com/cms-sw/genproductions/blob/mg27x/bin/MadGraph5_aMCatNLO/submit_condor_gridpack_generation.sh
                # bash gridpack_generation.sh ${name} ${carddir} ${workqueue} ALL ${scram_arch} ${cmssw_version}
                #outf.write( "./gridpack_generation.sh {} {} {} ALL {} {}\n".format(name, carddir, workqueue, scram_arch, cmssw_version ))
                outf.write( "./gridpack_generation.sh {} {} {} \n".format(name, carddir, workqueue))
        
        outf.write('# uncomment these lines to Add more commits by pushing to the HToZATo2L2B_run2Cards branch on kjaffel/genproductions.!\n')
        outf.write('# pushd cards/production/13TeV/\n')
        outf.write('# git checkout -b HToZATo2L2B_run2Cards\n')
        outf.write('# git add HToZATo2L2B_ggfusion_b-associatedproduction\n')
        outf.write("# git commit -m  'update HToZATo2L2B cards'\n")
        outf.write('# git push upstream HToZATo2L2B_run2Cards\n')

        outf.write('set +x\n')
    os.chmod('prepare_{}_{}_gridpacks.sh'.format(suffix, options.order.lower()), os.stat('prepare_{}_{}_gridpacks.sh'.format(suffix, options.order.lower())).st_mode | stat.S_IXUSR)
    print ('All commands prepared in ./prepare_{}_{}_gridpacks.sh'.format(suffix, options.order.lower()))

if __name__ == '__main__':
    prepare_all_MG5_cards()

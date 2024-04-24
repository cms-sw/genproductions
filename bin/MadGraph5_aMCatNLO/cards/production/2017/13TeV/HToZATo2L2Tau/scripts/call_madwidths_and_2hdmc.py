import os, os.path
import argparse, optparse
import shutil
import logging
import math
import collections
LOG_LEVEL = logging.DEBUG
stream = logging.StreamHandler()
stream.setLevel(LOG_LEVEL)
logger = logging.getLogger("ZA Cross checks")
logger.setLevel(LOG_LEVEL)
logger.addHandler(stream)

CMSSW_Calculators42HDM = os.path.join(os.environ["CMSSW_BASE"], "src", "cp3_llbb", "Calculators42HDM")
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
    print(" You can add colours to the output of Python logging module via : https://pypi.org/project/colorlog/")
    pass

def get_keys_from_value(d=None, val=None):
    return [k for k, v in d.items() if v == val]

def string_to_mass(s):
    m = float(s.replace('p', '.'))
    return m

def getcardsParams(cardname=None):
    split_cardname = cardname.split('/')
    split_cardname = split_cardname[-1]
    split_cardname = split_cardname.split('_')
    
    if 'HToZATo2L2B' in split_cardname or 'HToZATo2L2Tau' in split_cardname:
        mh2 = split_cardname[1]
        mh3 = split_cardname[2]
    else:
        mh3 = split_cardname[1]
        mh2 = split_cardname[2]
    
    mA = string_to_mass(mh3)
    mH = string_to_mass(mh2)
    if mA > mH:
        mode = 'A'
    elif mH >= mA and mH > 125.:
        mode = 'H'
    elif mH >= mA and mH <= 125.:
        mode = 'h'
    
    tb = split_cardname[3].replace('.dat','')
    return mh2, mh3, tb, mode

def getPDGID(pdgid=None):
    dicts = {'h': 25, 'H':35, 'A':36, 'H+':37, 'H-':37, 'Z':23, 'b':5, 't':6, 'c': 4, 's':3, 'e':11, 've':12, 'mu':13, 'vm':14,'ta':15, 'vt':16, 'g':21, 'ga':22, 'W+':24, 'W-':-24, }
    dicts_flipped = {v:k for k, v in dicts.items()}
    try :
        key = dicts_flipped[int(pdgid)]
    except:
        key = dicts_flipped[-int(pdgid)]
    return key

def getSushi_mbMSscheme(cardname=None):
    with open(os.path.join(CMSSW_Calculators42HDM, 'Scan/NNPDF31_nnlo_as_0118_nf_4_mc_hessian', cardname), 'r') as f:
        for line in f:
            if '# m_b for bottom Yukawa' not in line:
                continue
            mb_MSscheme_muR = line.split()[1]
    return mb_MSscheme_muR

def getTHDMprecisions(line = None, motherParticle= None, ID1= None, ID2= None, cardname = None, gettotal_width=False):
    with open(os.path.join(CMSSW_Calculators42HDM, cardname), 'r') as f:
        thdmc_BR = 0.
        thdmc_partialwidth = 0.
        thdmc_totalwidth = 0.
        mode = {'A':0, 'h': 0, 'H':0, 'H+':0}
        for line_ in f:
            if gettotal_width:
                suffix =('' if motherParticle =='H+' else (' ') )
                if 'Decay table for {}{}'.format(motherParticle, suffix) not in line_ :
                    continue
                mode[motherParticle] = 1
                nextLine = next(f)
                if 'Total width:' in nextLine and mode[motherParticle] ==1:
                    thdmc_totalwidth = float(nextLine.split()[2])
            else:
                if '{}  -> {} {}'.format(motherParticle, getPDGID(ID1), getPDGID(ID2)) not in line_ and '{}  -> {} {}'.format(motherParticle, getPDGID(ID2), getPDGID(ID1)) not in line_ and '{}  -> {}  {}'.format(motherParticle, getPDGID(ID1), getPDGID(ID2)) not in line_ and '{}  -> {}  {}'.format(motherParticle, getPDGID(ID2), getPDGID(ID1)) not in line_ and '{} -> {}  {}'.format(motherParticle, getPDGID(ID1), getPDGID(ID2)) not in line_ and '{} -> {}  {}'.format(motherParticle, getPDGID(ID2), getPDGID(ID1)) not in line_ and '{} -> {} {}'.format(motherParticle, getPDGID(ID1), getPDGID(ID2)) not in line_ and '{} -> {} {}'.format(motherParticle, getPDGID(ID2), getPDGID(ID1)) not in line_ :
                    continue
                print( 'IDS:', ID1, getPDGID(ID1), ID2, getPDGID(ID2) )
                print('thdmc:', line_)
                """
                Decay table for H
                Total width:   7.671e+01 GeV      BR
                H  -> s  s     7.945e-06      1.036e-07
                """
                thdmc_partialwidth = float(line_.split()[4])
                thdmc_BR= float(line_.split()[5])
        """"
        #      PDG        Width
        DECAY  35   2.318863e+02
        #  BR             NDA  ID1    ID2   ...
        9.095862e-01   2    36  23 # 210.920572285
        """
        pdgid = (35 if motherParticle== 'H' else( 36 if motherParticle== 'A' else (37 if motherParticle== 'H+' else( '25' if motherParticle== 'h' else(logger.error(' You are not suppose to change the decay of : {}'.format(motherParticle)))))))
        if gettotal_width:
            newline = 'DECAY  {}   {:.6e}\n'.format(pdgid, thdmc_totalwidth)
            relative_diff = abs((float(line.split()[-1]) - thdmc_totalwidth )/float(line.split()[-1]))
            if (relative_diff > 0.05 ):
                logger.critical('The LO estimate for the width of particle %s ' % pdgid)
                logger.critical('will differs from the one in the banner by %s percent' % (relative_diff*100))
        else:
            newline = '   {:.6e}   2    {}  {} # {:.11e}\n'.format(thdmc_BR, ID1, ID2, thdmc_partialwidth)
            if thdmc_BR == 0. or thdmc_partialwidth == 0. :
                logger.warning( '2HDMCalculator does not include {} -> {} {} decay , the one from MadWidth will be kept'.format(motherParticle, ID1, ID2))
                logger.warning( line )
                newline =line
        print( 'MadWidth:', line)
        print( '2HDMCalculator:', newline )
    return newline

def set_ymb_to_MBOnshell(param_card1=None, param_card2=None, interference=False):
    
    param_card = param_card1.split('.decay')[0]
    mh2, mh3, tb, mode = getcardsParams(param_card)
    cardname="madgraphInputs_mH-{}_mA-{}_tb-{}_mode{}.log".format(mh2, mh3, tb, mode)
    process =( 'bbH' if tb =='20p00' else('ggH'))
    
    decaychains = {'A': {'35': (5 , -5), '36': (23, 35) },
                   'H': {'35': (23, 36), '36': (5 , -5) },
                   'h': {'35': (23, 36), '36': (5 , -5) }
                   }
    aEWM1= 127.9
    aEW = 1./aEWM1
    Gf = 1.166390e-05

    mb = 4.92 # mb(OS) pole mass
    mb__tilde__ = 4.92 # mb~
    mt = 1.725000e+02
    MZ= 9.118760e+01
    MW= math.sqrt(MZ**2/2. + math.sqrt(MZ**4/4. - (aEW*math.pi*MZ**2)/(Gf*math.sqrt(2))))
    
    ee = 2.*math.sqrt(aEW)*math.sqrt(math.pi)
    sw2 = 1. - MW**2/MZ**2
    sw = math.sqrt(sw2)
    vev = (2.*MW*sw)/ee
    CF = 4./3.
    alpha_s = 1.180000e-01 # FIXME alphas_muR
    
    ymt = 1.725000e+02
    ymb = 4.18 #m_b(m_b), MSbar FIXME m_b(mu_R), MSbar
    
    if process == 'bbH':
        mu_R = (string_to_mass(mh3) + MZ + mb + mb__tilde__)/4.
    elif process == 'ggH': # page 40 ; https://arxiv.org/pdf/1610.07922.pdf
        mu_R = string_to_mass(mh2)/2.
    
    if os.path.exists( param_card):
        os.remove(param_card)
    if os.path.exists( param_card1) and os.path.exists( param_card2): 
        branching_ratios = collections.defaultdict(dict)
        total_widths = collections.defaultdict(dict)
        partial_widths = collections.defaultdict(dict)

        with open(param_card2, 'r') as inf2:
            with open(param_card, 'w+') as outf:
                ID1 =None
                ID2 =None
                inf2h1_mode = 0
                inf2h2_mode = 0
                inf2h3_mode = 0
                inf2hc_mode = 0
                for line2 in inf2:
                    if "DECAY  25" in line2:
                        inf2h1_mode = 1
                        inf2h2_mode = 0
                        inf2h3_mode = 0
                        inf2hc_mode = 0
                        if interference:
                            line2_modf =getTHDMprecisions(line=line2, motherParticle='h', ID1=None, ID2=None, cardname= cardname, gettotal_width=True)
                            total_widths['25'] = float(line2_modf.split()[-1])
                            outf.write(line2_modf)
                        else:
                            outf.write(line2)
                    elif "DECAY  35" in line2:
                        inf2h1_mode = 0
                        inf2h2_mode = 1
                        inf2h3_mode = 0
                        inf2hc_mode = 0
                    elif "DECAY  36" in line2:
                        inf2h1_mode = 0
                        inf2h2_mode = 0
                        inf2h3_mode = 1
                        inf2hc_mode = 0
                        line2_modf =getTHDMprecisions(line=line2, motherParticle='A', ID1=None, ID2=None, cardname= cardname, gettotal_width=True)
                        total_widths['36'] = float(line2_modf.split()[-1])
                        outf.write(line2_modf)
                    elif "DECAY  37" in line2:
                        inf2h1_mode = 0
                        inf2h2_mode = 0
                        inf2h3_mode = 0
                        inf2hc_mode = 1
                        line2_modf =getTHDMprecisions(line=line2, motherParticle='H+', ID1=None, ID2=None, cardname= cardname, gettotal_width=True)
                        total_widths['37'] = float(line2_modf.split()[-1])
                        outf.write(line2_modf)

                    if "ymb" in line2:
                        # FIXME YOU HAVE TO DO IT RIGHT FOR THIS PROCESS , Yukawa coupling corrections CAN'T BE THIS WAY
                        #if process == 'bbH': # pp > h2 > h3 Z b b~ 
                            # https://arxiv.org/pdf/1808.01660.pdf eq 2.5 
                            # https://arxiv.org/pdf/1610.07922.pdf page 523 
                        #    yb_SM= math.sqrt(2) *ymb /vev
                        #    yt = math.sqrt(2) *ymt /vev
                        #    yb_HEFT = yb_SM + yt* (pow(alpha_s,2)/pow(math.pi,2) )* (mb/mt)*CF*((5./24.) - (1./4.)*math.log(pow(mu_R,2)/pow(mt,2)))
                        #    ymb_HEFT = (yb_HEFT * vev )/math.sqrt(2)
                        #    outf.write('    5 {}   # ymb MSbar(muR)\n'.format(ymb_HEFT))
                        #    print( ymb_HEFT)
                        #else:
                        outf.write('    5 {:.8e}   # ymb\n'.format(ymb))
                    elif inf2h1_mode ==1:
                        if "DECAY  25" not in line2:
                            try:
                                ID1=line2.split()[2]
                                ID2=line2.split()[3]
                                line2_modf =getTHDMprecisions(line=line2, motherParticle='h', ID1=ID1, ID2=ID2, cardname= cardname, gettotal_width=False)
                                branching_ratios['25']['{}  {}'.format(ID1,ID2)]=float(line2_modf.split()[0])          
                                partial_widths['25']['{}  {}'.format(ID1,ID2)]=float(line2_modf.split()[-1])          
                                print( '--'*60)
                                outf.write(line2_modf)
                            except:
                                outf.write(line2)
                    elif inf2h3_mode ==1:
                        if "DECAY  36" not in line2:
                            try:
                                ID1=line2.split()[2]
                                ID2=line2.split()[3]
                                line2_modf =getTHDMprecisions(line=line2, motherParticle='A', ID1=ID1, ID2=ID2, cardname= cardname, gettotal_width=False)
                                branching_ratios['36']['{}  {}'.format(ID1,ID2)]=float(line2_modf.split()[0])          
                                partial_widths['36']['{}  {}'.format(ID1,ID2)]=float(line2_modf.split()[-1])          
                                print( '--'*60)
                                outf.write(line2_modf)
                            except:
                                outf.write(line2)
                    elif inf2hc_mode ==1:
                        if "DECAY  37" not in line2:
                            try:
                                ID1=line2.split()[2]
                                ID2=line2.split()[3]
                                line2_modf =getTHDMprecisions(line=line2, motherParticle='H+', ID1=ID1, ID2=ID2, cardname= cardname, gettotal_width=False)
                                branching_ratios['37']['{}  {}'.format(ID1,ID2)]=float(line2_modf.split()[0])          
                                partial_widths['37']['{}  {}'.format(ID1,ID2)]=float(line2_modf.split()[-1])          
                                print( '--'*60)
                                outf.write(line2_modf)
                            except:
                                outf.write(line2)
             
                    
                    elif "DECAY  35" in line2:
                        with open(param_card1, 'r') as inf1:
                            ID1_ =None
                            ID2_ =None
                            inf2h1_mode = 0
                            inf1h2_mode = 0
                            inf1h3_mode = 0
                            inf1hc_mode = 0
                            for line1 in inf1:
                                if "DECAY  35" in line1:
                                    inf1h2_mode = 1
                                    inf2h1_mode = 0
                                    inf1h3_mode = 0
                                    inf1hc_mode = 0
                                    line1_modf =getTHDMprecisions(line=line1, motherParticle='H', ID1=None, ID2=None, cardname=cardname, gettotal_width=True) 
                                    total_widths['35'] = float(line1_modf.split()[-1])
                                    outf.write(line1_modf)
                                elif "DECAY  36" in line1:
                                    inf2h1_mode = 0
                                    inf1h2_mode = 0
                                    inf1h3_mode = 1
                                    inf1hc_mode = 0
                                
                                if inf1h2_mode == 1:
                                    if "DECAY  35" not in line1:
                                        try:
                                            ID1_=line1.split()[2]
                                            ID2_=line1.split()[3]
                                            line1_modf =getTHDMprecisions(line=line1, motherParticle='H', ID1=ID1_, ID2=ID2_, cardname=cardname, gettotal_width=False) 
                                            branching_ratios['35']['{}  {}'.format(ID1_,ID2_)]=float(line1_modf.split()[0])       
                                            partial_widths['35']['{}  {}'.format(ID1_,ID2_)]=float(line1_modf.split()[-1])          
                                            print( '--'*60)
                                            outf.write(line1_modf)
                                        except:
                                            outf.write(line1)

                    else:
                        outf.write(line2)
        
        #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        # DEBUG 
        for id in ['35', '36']:
            channel_with_max_BR = max(branching_ratios[id], key=branching_ratios[id].get)
            val = sorted(list(branching_ratios[id].values()))
            alloweddecays = ['{}  {}'.format(decaychains[mode][id][0], decaychains[mode][id][1]), '{}  {}'.format(decaychains[mode][id][1], decaychains[mode][id][0])]
            
            if channel_with_max_BR not in alloweddecays:
                logger.critical('** You need to be careful with : {}'.format(param_card))
                logger.critical('   seems to be {} is the main channel that contribute to the total width of : {} ** '.format(channel_with_max_BR, id))
                logger.critical('   BR:  {} -> {} = {}'.format( id, channel_with_max_BR, branching_ratios[id][channel_with_max_BR]))
                    
            idx = -2
            openchannel = channel_with_max_BR
            while (openchannel not in alloweddecays) and (abs(idx) in range(len(val))):    
                res = val[idx]
                pdgids = get_keys_from_value(branching_ratios[id], res)[0].split()
                openchannel = '{}  {}'.format(pdgids[0], pdgids[1])
                logger.critical(".{}.contribution. BR:  {} -> {}  = {} ".format(abs(idx), id, openchannel,res))
                idx -=1 
            
            for channel in alloweddecays:
                if channel in branching_ratios[id].keys():
                    BR = partial_widths[id][channel] / total_widths[id]
                    if BR> 1.:
                        logger.error(' Branching ratio larger than one for {}'.format(id))
                        logger.error(' Auto BR of {} = {}'.format(id, BR) )
                        
        #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        print ( "{} successfully overwritten with 35 36 37 and 23 decay widths and branching ratios!".format(param_card) ) 
        # there will be no need for these cards 
        os.remove(param_card1)
        os.remove(param_card2)
    else:
        logger.error(" XXX_param_card.dat with h2 decay OR XXX_param_card.dat with h3 and Z decay is missing, please run prepare_MG5_cards.py and then ./bin/mg5_aMC run_madwidths.sh from MG5_aMC_vX_X_X first !")
    return

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='', formatter_class=argparse.RawTextHelpFormatter)
    parser.add_argument('--param_card1', required=True, help='')
    parser.add_argument('--param_card2', required=True, help='')
    parser.add_argument('--interference', action='store_true', default= False, help='add h1 interference')
    options = parser.parse_args()
    
    set_ymb_to_MBOnshell(param_card1=options.param_card1, param_card2=options.param_card2, interference=options.interference)

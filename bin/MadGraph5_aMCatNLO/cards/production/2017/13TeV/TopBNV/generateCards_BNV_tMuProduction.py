import os
import random

#see Eq.2 in https://arxiv.org/pdf/1107.3805.pdf

#S-ch = tDUE
#T-ch = tEUD

MG = [
['define p =  u d u~ d~', 'generate p p > t~ mu+', 'add process p p > t mu-'],
['define p =  c d c~ d~', 'generate p p > t~ mu+', 'add process p p > t mu-'],
['define p =  u s u~ s~', 'generate p p > t~ mu+', 'add process p p > t mu-'], 
['define p =  c s c~ s~', 'generate p p > t~ mu+', 'add process p p > t mu-'], 
['define p =  u b u~ b~', 'generate p p > t~ mu+', 'add process p p > t mu-'], 
['define p =  c b c~ b~', 'generate p p > t~ mu+', 'add process p p > t mu-'], 
]

processName=[
'TDUMu',
'TDCMu',
'TSUMu',
'TSCMu',
'TBUMu',
'TBCMu',
]

couplingsName = [
['cS','cT'],
['cS','cT'],
['cS','cT'],
['cS','cT'],
['cS','cT'],
['cS','cT']
]


couplings =[
[['aaa3x1','bbb3x1','ccc1x2','ddd1x2'],['aaaprime3x2','bbbprime3x2','cccprime1x1','dddprime1x1']],
[['aaa3x1','bbb3x1','ccc2x2','ddd2x2'],['aaaprime3x2','bbbprime3x2','cccprime2x1','dddprime2x1']],
[['aaa3x2','bbb3x2','ccc1x2','ddd1x2'],['aaaprime3x2','bbbprime3x2','cccprime1x2','dddprime1x2']],
[['aaa3x2','bbb3x2','ccc2x2','ddd2x2'],['aaaprime3x2','bbbprime3x2','cccprime2x2','dddprime2x2']],
[['aaa3x3','bbb3x3','ccc1x2','ddd1x2'],['aaaprime3x2','bbbprime3x2','cccprime1x3','dddprime1x3']],
[['aaa3x3','bbb3x3','ccc2x2','ddd2x2'],['aaaprime3x2','bbbprime3x2','cccprime2x3','dddprime2x3']]
]

scanValues = 20

for num, name in enumerate(processName):
    os.system('rm -rf BNV_ST_' + name)
    os.system('mkdir BNV_ST_' + name)
    customizecards = ''
    customizecards = customizecards + 'set param_card mass   6  172.5\n'
    customizecards = customizecards + 'set param_card yukawa 6  172.5\n'
    customizecards = customizecards + 'set param_card mass   25 125.0\n'
    customizecards = customizecards + 'set dynamical_scale_choice 3\n'
    for gWC in couplings[num]:
        for WC in gWC:
            customizecards = customizecards + 'set param_card '+WC+ ' ' + str(1) + '\n'     
    open('BNV_ST_' + name + '/BNV_ST_' + name + '_customizecards.dat', 'wt').write(customizecards)
    n=-1
    rwgtCards = ''
    rwgtCards = rwgtCards + 'change rwgt_dir rwgt'+ '\n'+ '\n'
    for v in range(scanValues):
        randomWC = []
        for WC1 in couplingsName[num]:
            r = random.uniform(0,10)
            randomWC.append(round(r,2))
        n  = n+1
        rwgtCards = rwgtCards + '\n'
        rwgtCards = rwgtCards + 'launch --rwgt_name=EFTrwgt' + str(n) + '_'
        for WC1 in couplingsName[num]:
            rwgtCards = rwgtCards + WC1 + '_' + str(randomWC[couplingsName[num].index(WC1)]) + '_'
        rwgtCards = rwgtCards[:-1]
        rwgtCards = rwgtCards + '\n'
        for WC1 in couplingsName[num]:
            for wcIndex in couplings[num][couplingsName[num].index(WC1)]:
                rwgtCards = rwgtCards +'    set param_card ' + wcIndex + ' ' + str(randomWC[couplingsName[num].index(WC1)])  + '\n'
    open('BNV_ST_' + name + '/BNV_ST_' + name + '_reweight_card.dat', 'wt').write(rwgtCards)
    
    process = ''
    process = process + 'import model bnv_mediator_ufo' + '\n'
    process = process + MG[num][0] + '\n'
    process = process + MG[num][1] + '\n'
    process = process + MG[num][2] + '\n'
    process = process + 'output BNV_ST_' + name + ' -f -nojpeg'
    open('BNV_ST_' + name + '/BNV_ST_' + name + '_proc_card.dat', 'wt').write(process)
    os.system('cp BNV_run_card.dat BNV_ST_' + name + '/BNV_ST_' + name + '_run_card.dat')


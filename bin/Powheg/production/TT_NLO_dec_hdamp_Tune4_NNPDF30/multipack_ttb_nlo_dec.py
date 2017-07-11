#! /usr/bin/env python
import sys
import optparse
import os
import random
import re

def main():

    #configuration
    usage = 'usage: %prog [options]'
    parser = optparse.OptionParser(usage)
    parser.add_option('--nevents', dest='nevents', help='Number of events [%default]', default=10,  type=int)
    parser.add_option('--seed', dest='seed', help='Random seed [%default]', default=1,  type=int)
    (opt, args) = parser.parse_args()
    
    #branching ratios
    lep = 0.108
    had = 2*0.337

    samples = {}
    
    gridpackpath = '/cvmfs/cms.cern.ch/phys_generator/gridpacks/slc6_amd64_gcc481/13TeV/powheg/V2/TT_NLO_dec_hdamp_Tune4_NNPDF30/'
    samples['ttb_NLO_dec_e_e_NNPDF30_13TeV_ttb_NLO_dec.tgz']     = { 'br': lep*lep, 'enhance': 2.0 }
    samples['ttb_NLO_dec_e_mu_NNPDF30_13TeV_ttb_NLO_dec.tgz']    = { 'br': lep*lep, 'enhance': 2.0 }
    samples['ttb_NLO_dec_e_tau_NNPDF30_13TeV_ttb_NLO_dec.tgz']   = { 'br': lep*lep, 'enhance': 1.0 }
    samples['ttb_NLO_dec_e_had_NNPDF30_13TeV_ttb_NLO_dec.tgz']   = { 'br': lep*had, 'enhance': 1.0 }
    samples['ttb_NLO_dec_mu_e_NNPDF30_13TeV_ttb_NLO_dec.tgz']    = { 'br': lep*lep, 'enhance': 2.0 }
    samples['ttb_NLO_dec_mu_mu_NNPDF30_13TeV_ttb_NLO_dec.tgz']   = { 'br': lep*lep, 'enhance': 2.0 }
    samples['ttb_NLO_dec_mu_tau_NNPDF30_13TeV_ttb_NLO_dec.tgz']  = { 'br': lep*lep, 'enhance': 1.0 }
    samples['ttb_NLO_dec_mu_had_NNPDF30_13TeV_ttb_NLO_dec.tgz']  = { 'br': lep*had, 'enhance': 1.0 }
    samples['ttb_NLO_dec_tau_e_NNPDF30_13TeV_ttb_NLO_dec.tgz']   = { 'br': lep*lep, 'enhance': 1.0 }
    samples['ttb_NLO_dec_tau_mu_NNPDF30_13TeV_ttb_NLO_dec.tgz']  = { 'br': lep*lep, 'enhance': 1.0 }
    samples['ttb_NLO_dec_tau_tau_NNPDF30_13TeV_ttb_NLO_dec.tgz'] = { 'br': lep*lep, 'enhance': 1.0 }
    samples['ttb_NLO_dec_tau_had_NNPDF30_13TeV_ttb_NLO_dec.tgz'] = { 'br': lep*had, 'enhance': 1.0 }
    samples['ttb_NLO_dec_had_e_NNPDF30_13TeV_ttb_NLO_dec.tgz']   = { 'br': had*lep, 'enhance': 1.0 }
    samples['ttb_NLO_dec_had_mu_NNPDF30_13TeV_ttb_NLO_dec.tgz']  = { 'br': had*lep, 'enhance': 1.0 }
    samples['ttb_NLO_dec_had_tau_NNPDF30_13TeV_ttb_NLO_dec.tgz'] = { 'br': had*lep, 'enhance': 1.0 }
    samples['ttb_NLO_dec_had_had_NNPDF30_13TeV_ttb_NLO_dec.tgz'] = { 'br': had*had, 'enhance': 1.0 }
    
    sumbr = 0.
    for sample,data in samples.iteritems():
        sumbr += data['br'] * data['enhance']
        data['nevt'] = 0
    
    random.seed(opt.seed)
    for i in range(opt.nevents):
        pick = random.uniform(0.,sumbr)
        test = 0
        for sample,data in samples.iteritems():
            test += data['br'] * data['enhance']
            if pick < test:
                data['nevt'] += 1
                break
    
    print(samples)
    
    events = []
    
    with open('merged.lhe', 'w') as merged:
        firstSample = True
        for sample,data in samples.iteritems():
            if (data['nevt'] == 0): continue
            os.system('sh run_generic_tarball_cvmfs.sh %s/%s %s %s'%(gridpackpath, sample, str(data['nevt']), str(opt.seed)))
            os.system('sed -i \'/<\/LesHouchesEvents>/d\' cmsgrid_final.lhe')
            if firstSample:
                firstSample = False
            else: #remove lhe header
                os.system('sed -i \'1,/<\/init>/d\' cmsgrid_final.lhe')
            with open('cmsgrid_final.lhe', 'r') as chunk:
                hasWeights = False
                event = []
                for line in chunk:
                    if '</event>' in line:
                        if hasWeights == False:
                            event.append('<rwgt>\n')
                            event.append('<wgt id=\'1001\'>%s</wgt>\n'%(str(1./data['enhance'])))
                            event.append('</rwgt>\n</event>\n')
                        event.append(line)
                        events.append(event)
                        event = []
                    elif '<event>' in line or len(event) > 0:
                        m = re.match('<wgt id=\'(\d*)\'>(.*)<\/wgt>', line)
                        if m: #modify weight with 1/(enhancement factor)
                            weight = float(m.group(2))/data['enhance']
                            event.append('<wgt id=\'%s\'>%s</wgt>\n'%(m.group(1), str(weight)))
                            hasWeights = True
                        else:
                            event.append(line)
                    else:
                        merged.write(line)
        random.shuffle(events)
        for event in events:
            for line in event:
                merged.write(line)
    os.system('echo "</LesHouchesEvents>" >> merged.lhe')
    os.system('mv merged.lhe cmsgrid_final.lhe')

"""
for execution from another script
"""
if __name__ == "__main__":
    sys.exit(main())

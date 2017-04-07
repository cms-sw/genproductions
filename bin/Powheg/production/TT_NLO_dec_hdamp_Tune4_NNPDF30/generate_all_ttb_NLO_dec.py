#! /usr/bin/env python
import sys
import os

def main():

    modes = {'e': 11, 'mu': 13, 'tau': 15, 'had': 1}
    
    for topmode in modes:
        for tbarmode in modes:
            with open('ttb_NLO_dec_%s_%s_NNPDF30_13TeV.input'%(topmode,tbarmode), 'w') as f:
                with open('ttb_NLO_dec_template_NNPDF30_13TeV.input', 'r') as template:
                    for line in template:
                        if 'topdec' in line:
                            f.write('topdec ' + str(modes[topmode]) + ' ! 11, 13 and 15 for e+,mu+,tau+, 1 for hadrons\n')
                        elif 'tbardec' in line:
                            f.write('tbardec ' + str(modes[tbarmode]) + ' ! 11, 13 and 15 for e+,mu+,tau+, 1 for hadrons\n')
                        else: f.write(line)
            cmd = 'python ./run_pwg.py -p f -i ttb_NLO_dec_%s_%s_NNPDF30_13TeV.input -f ttb_NLO_dec_%s_%s_NNPDF30_13TeV -m ttb_NLO_dec -n 1000'%(topmode,tbarmode,topmode,tbarmode)
            print(cmd)
            if os.path.isfile('ttb_NLO_dec_%s_%s_NNPDF30_13TeV/pwgevents.lhe'%(topmode,tbarmode)):
                print('Skipping as pwgevent.lhe is present')
            else:
                os.system(cmd)


"""
for execution from another script
"""
if __name__ == "__main__":
    sys.exit(main())

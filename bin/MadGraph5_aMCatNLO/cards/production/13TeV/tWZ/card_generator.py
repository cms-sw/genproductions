from string import Template
import os
import sys
decay = {'Lep' : ['l+ vl', 'l- vl~'],'Had' : ['had had', 'had had']}
boosted = {'Boosted' : '6:250', 'NotBoosted' : ''}
DRS = {'DR1' : '1', 'DR2' :'2', 'DS' : '5'}

sample_list = [
                'tHadWLepZLepDR1Boosted',
                'tHadWLepZLepDR2',
                'tHadWLepZLepDR2Boosted',
                'tHadWLepZLepDS',
                'tHadWLepZLepDSBoosted',
                'tLepWHadZLepDR1Boosted',
                'tLepWHadZLepDR2',
                'tLepWHadZLepDR2Boosted',
                'tLepWHadZLepDS',
                'tLepWHadZLepDSBoosted',
                'tLepWLepZLepDR1Boosted',
                'tLepWLepZLepDR2',
                'tLepWLepZLepDR2Boosted',
                'tLepWLepZLepDS',
                'tLepWLepZLepDSBoosted'
]

for sample in sample_list:
    dir_name = '{}/{}'.format(sys.path[0], sample)
    if not os.path.exists(dir_name): 
        os.mkdir(dir_name)

    top_decay = sample[1:4]
    W_decay = sample[5:8]
    removal_scheme = ''
    for scheme in DRS:
        if scheme in sample:
            removal_scheme = scheme

    boosted_string = ''
    if 'Boosted' in sample: boosted_string = 'Boosted'
    else : boosted_string = 'NotBoosted'

    madspin_fill = {
            'TOPDECAY' : decay[top_decay][0],
            'TBARDECAY' : decay[top_decay][1],
            'WPLUSDECAY' : decay[W_decay][0],
            'WMINUSDECAY' : decay[W_decay][1]
    }

    proc_fill = {'SAMPLENAME' : sample}

    run_fill = {'DRS' : DRS[removal_scheme],
                'BOOSTED' : boosted[boosted_string]    
            }

    with open('template_customizecards.dat','r') as customize_template:
        file = customize_template.read()
        with open('{}/{}_customizecards.dat'.format(dir_name,sample),'w') as customize_dump:
            customize_dump.write(file)

    with open('template_madspin_card.dat','r') as madspin_template:
        file = Template(madspin_template.read())
        filled_template = file.substitute(madspin_fill)
        with open('{}/{}_madspin_card.dat'.format(dir_name,sample),'w') as madspin_dump:
            madspin_dump.write(filled_template)

    
    with open('template_proc_card.dat','r') as proc_template:
        file = Template(proc_template.read())
        filled_template = file.substitute(proc_fill)
        with open('{}/{}_proc_card.dat'.format(dir_name,sample),'w') as proc_dump:
            proc_dump.write(filled_template)

    with open('template_run_card.dat','r') as run_template:
        file = Template(run_template.read())
        filled_template = file.substitute(run_fill)
        with open('{}/{}_run_card.dat'.format(dir_name,sample),'w') as run_dump:
            run_dump.write(filled_template)

    
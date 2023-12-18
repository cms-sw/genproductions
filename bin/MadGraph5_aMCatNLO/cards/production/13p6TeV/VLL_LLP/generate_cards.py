import os
pjoin=os.path.join

sigmodel = 'singlet'
flavors= ['ele', 'muon']

VLLMass=[450,600,750,900]
VLLDecay=[1e-13,1e-14,1e-15,1e-16]

model = 'VLLS' if sigmodel == 'singlet' else 'VLLD'
templates = 'templates'

##template cards
extramodels     = 'VLLS_extramodels.dat'
run_card        = 'VLLS_run_card.dat'
customize_card  = 'VLLS_customizecards.dat'
proc_card       = 'VLLS_proc_card.dat'

for flavor in flavors:
    for mass in VLLMass:
        for decay in VLLDecay:

            massstr  = f'M{mass}'
            decaystr = f'D{decay}'
            genstr = '13p6TeV-madgraph'

            prefix   = '_'.join([model, flavor, massstr, decaystr, genstr])

            os.system(f"mkdir {prefix}")

            #copy template cards
            os.system(f'cp {templates}/{extramodels}    {prefix}/{prefix}_extramodels.dat')
            os.system(f'cp {templates}/{run_card}       {prefix}/{prefix}_run_card.dat')
            os.system(f'cp {templates}/{customize_card} {prefix}/{prefix}_customizecards.dat')
            os.system(f'cp {templates}/{proc_card}      {prefix}/{prefix}_proc_card.dat')

            ## change content using sed
            os.system(f"sed -i 's/massholder/{mass}/g'   {prefix}/{prefix}_customizecards.dat")
            os.system(f"sed -i 's/decayholder/{decay}/g' {prefix}/{prefix}_customizecards.dat")
            os.system(f"sed -i 's/flavorholder/{flavor}/g'     {prefix}/{prefix}_proc_card.dat")
            os.system(f"sed -i 's/prefixholder/{prefix}/g'     {prefix}/{prefix}_proc_card.dat")

            print(f"\nCreated cards in {prefix}:")
            print(os.listdir(prefix))

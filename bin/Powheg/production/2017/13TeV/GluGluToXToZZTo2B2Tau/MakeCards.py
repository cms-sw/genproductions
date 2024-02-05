import os

FileTemplate = 'gg_X_ZZbbtautau_quark-mass-effects_NNPDF31_13TeV_template.input'

MassPoints = [200,300,400,500,600,700,800,900,1000,1100,1200,1300,1400,1500,2000,3000]

for Mass in MassPoints:

    # copy template fragment
    os.system(f'cp {FileTemplate} gg_X_ZZbbtautau_quark-mass-effects_NNPDF31_13TeV_M{Mass}.input')

    # change content using sed
    print(f'Mass: {Mass}')
    os.system(f"sed -i 's/MASS/{Mass}/g' gg_X_ZZbbtautau_quark-mass-effects_NNPDF31_13TeV_M{Mass}.input")

print(f"\nCreated cards")
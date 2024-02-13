import os

FileTemplate = 'gg_X_ZZbbtautau_quark-mass-effects_NNPDF31_13TeV_template.input'

MassPoints = [ 200, 210, 220, 230, 240, 250, 260, 270, 280, 300, 320, 350, 360, 400, 450, 500, 550,
               600, 650, 700, 750, 800, 850, 900, 1000, 1100, 1200, 1300, 1400, 1500, 1600, 1700,
               1800, 1900, 2000, 2200, 2400, 2500, 2600, 2800, 3000, 3500, 4000, 4500, 5000] 

for Mass in MassPoints:

    # copy template fragment
    os.system(f'cp {FileTemplate} gg_X_ZZbbtautau_quark-mass-effects_NNPDF31_13TeV_M{Mass}.input')

    # change content using sed
    print(f'Mass: {Mass}')
    os.system(f"sed -i 's/MASS/{Mass}/g' gg_X_ZZbbtautau_quark-mass-effects_NNPDF31_13TeV_M{Mass}.input")

print(f"\nCreated cards")
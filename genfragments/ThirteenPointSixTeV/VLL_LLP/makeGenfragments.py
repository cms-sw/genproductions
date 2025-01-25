import os
pjoin=os.path.join

sigmodel = 'singlet'
flavors= ['ele', 'muon']

VLLMass=[450,600,750,900]
VLLDecay=[1e-13,1e-14,1e-15,1e-16]

model = 'VLLS' if sigmodel == 'singlet' else 'VLLD'
templates = 'templates'

for flavor in flavors:
    for mass in VLLMass:
        for decay in VLLDecay:

            massstr  = f'M{mass}'
            decaystr = f'D{decay}'
            genstr = '13p6TeV-madgraph'
            arch = 'el9_amd64_gcc11_CMSSW_13_2_9'
            suffix = 'tarball.tar.xz'

            prefix   = '_'.join([model, flavor, massstr, decaystr, genstr])
            gridpack_fname = '_'.join([prefix, arch, suffix])

            #copy template card
            os.system(f'cp {templates}/VLLS_fragment_template_cfi.py {prefix}_cfi.py')

            ## change content using sed
            print(f'Gridpack filename: {gridpack_fname}')
            os.system(f"sed -i 's/gridpackfilenameholder/{gridpack_fname}/g' {prefix}_cfi.py")

print(f"\nCreated fragments")
print(os.listdir('.'))

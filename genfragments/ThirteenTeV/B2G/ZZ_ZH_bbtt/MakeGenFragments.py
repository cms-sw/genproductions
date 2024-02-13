import os

FileTemplate = 'ZZ_ZH_bbtt-fragment_template_cfi.py'

# Produce fragments for GluGluToXToZZTo2B2Tau
MassPoints_ZZ = [ 200, 210, 220, 230, 240, 250, 260, 270, 280, 300, 320, 350, 360, 400, 450, 500, 550,
                  600, 650, 700, 750, 800, 850, 900, 1000, 1100, 1200, 1300, 1400, 1500, 1600, 1700,
                  1800, 1900, 2000, 2200, 2400, 2500, 2600, 2800, 3000, 3500, 4000, 4500, 5000] 
for Mass in MassPoints_ZZ:

    # copy template fragment
    os.system(f'cp {FileTemplate} GluGluToXToZZTo2B2Tau_M{Mass}-fragment_cfi.py')

    # change content using sed
    GridpackName = f'gg_H_quark-mass-effects_slc7_amd64_gcc700_CMSSW_10_6_37_my_gg_X_ZZbbtautau_quark-mass-effects_NNPDF31_13TeV_M{Mass}.tgz'
    print(f'Gridpack filename: {GridpackName}')
    os.system(f"sed -i 's/gridpack_name/{GridpackName}/g' GluGluToXToZZTo2B2Tau_M{Mass}-fragment_cfi.py")

# Produce fragments for DY_ZprimeToZH_ZbbHtautau
MassPoints_ZH = [600,800,1000,1200,1400,1600,1800,2000,2500,3000,3500,4000,4500,5000,5500,6000,6500,7000,7500,8000]
for Mass in MassPoints_ZH:

    # copy template fragment
    os.system(f'cp {FileTemplate} DY_ZprimeToZH_ZbbHtautau_M{Mass}-fragment_cfi.py')

    # change content using sed
    GridpackName = f'Zprime_Zh_Zbbhtautau_narrow_M{Mass}_slc7_amd64_gcc10_CMSSW_12_4_8_tarball.tar.xz'
    print(f'Gridpack filename: {GridpackName}')
    os.system(f"sed -i 's/gridpack_name/{GridpackName}/g' DY_ZprimeToZH_ZbbHtautau_M{Mass}-fragment_cfi.py")

# Produce fragments for DY_ZprimeToZH_ZtautauHbb
MassPoints_ZH = [600,800,1000,1200,1400,1600,1800,2000,2500,3000,3500,4000,4500,5000,5500,6000,6500,7000,7500,8000]
for Mass in MassPoints_ZH:

    # copy template fragment
    os.system(f'cp {FileTemplate} DY_ZprimeToZH_ZtautauHbb_M{Mass}-fragment_cfi.py')

    # change content using sed
    GridpackName = f'Zprime_Zh_Ztautauhbb_narrow_M{Mass}_slc7_amd64_gcc10_CMSSW_12_4_8_tarball.tar.xz'
    print(f'Gridpack filename: {GridpackName}')
    os.system(f"sed -i 's/gridpack_name/{GridpackName}/g' DY_ZprimeToZH_ZtautauHbb_M{Mass}-fragment_cfi.py")

print(f"\nCreated fragments")
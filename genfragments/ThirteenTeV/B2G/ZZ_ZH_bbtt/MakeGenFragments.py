import os

FileTemplate = 'ZZ_ZH_bbtt-fragment_template_cfi.py'

# Produce fragments for GluGluToXToZZTo2B2Tau
MassPoints_ZZ = [200,300,400,500,600,700,800,900,1000,1100,1200,1300,1400,1500,2000,3000]
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
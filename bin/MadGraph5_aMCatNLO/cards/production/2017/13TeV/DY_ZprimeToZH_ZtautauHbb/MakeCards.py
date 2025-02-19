import os

FolderTemplate = 'Zprime_Zh_Ztautauhbb_narrow_template'

MassPoints = [600,800,1000,1200,1400,1600,1800,2000,2500,3000,3500,4000,4500,5000,5500,6000,6500,7000,7500,8000]

for Mass in MassPoints:

    # copy template fragment
    os.system(f'mkdir Zprime_Zh_Ztautauhbb_narrow_M{Mass}')
    os.system(f'cp {FolderTemplate}/Zprime_Zh_Ztautauhbb_narrow_customizecards.dat Zprime_Zh_Ztautauhbb_narrow_M{Mass}/Zprime_Zh_Ztautauhbb_narrow_M{Mass}_customizecards.dat')
    os.system(f'cp {FolderTemplate}/Zprime_Zh_Ztautauhbb_narrow_extramodels.dat Zprime_Zh_Ztautauhbb_narrow_M{Mass}/Zprime_Zh_Ztautauhbb_narrow_M{Mass}_extramodels.dat')
    os.system(f'cp {FolderTemplate}/Zprime_Zh_Ztautauhbb_narrow_proc_card.dat Zprime_Zh_Ztautauhbb_narrow_M{Mass}/Zprime_Zh_Ztautauhbb_narrow_M{Mass}_proc_card.dat')
    os.system(f'cp {FolderTemplate}/Zprime_Zh_Ztautauhbb_narrow_run_card.dat Zprime_Zh_Ztautauhbb_narrow_M{Mass}/Zprime_Zh_Ztautauhbb_narrow_M{Mass}_run_card.dat')

    # change content using sed
    print(f'Mass: {Mass}')
    os.system(f"sed -i 's/MASS/{Mass}/g' Zprime_Zh_Ztautauhbb_narrow_M{Mass}/Zprime_Zh_Ztautauhbb_narrow_M{Mass}_customizecards.dat")
    os.system(f"sed -i 's/MASS/{Mass}/g' Zprime_Zh_Ztautauhbb_narrow_M{Mass}/Zprime_Zh_Ztautauhbb_narrow_M{Mass}_proc_card.dat")

print(f"\nCreated cards")
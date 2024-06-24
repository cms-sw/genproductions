import os

template = "WRtoNMutoMuMuJJ_MWR-wrmass_MN-nmass_13p6TeV"

wrmasses = [2000*i for i in range(1, 5)]

nmass = 100
for wrmass in wrmasses:
    while(nmass < wrmass):
        grid = template.replace("wrmass", str(wrmass)).replace("nmass", str(nmass))
        os.system(f"mkdir -p {grid}")
        os.system(f"cp base/base_proc_card.dat {grid}/{grid}_proc_card.dat")
        os.system(f"cp base/base_run_card.dat {grid}/{grid}_run_card.dat")
        os.system(f"cp base/base_customizecards.dat {grid}/{grid}_customizecards.dat")
        os.system(f"cp base/base_extramodels.dat {grid}/{grid}_extramodels.dat")
        os.system(f"sed -i 's|@@base@@|{grid}|g' {grid}/{grid}_proc_card.dat")
        os.system(f"sed -i 's|@@nmass@@|{nmass}|g' {grid}/{grid}_customizecards.dat")
        os.system(f"sed -i 's|@@wrmass@@|{wrmass}|g' {grid}/{grid}_customizecards.dat")
        print (f"./gridpack_generation.sh {grid} cards/LRSM/{grid}")
        nmass += 200
    if (nmass >= wrmass):
        nmass = 100

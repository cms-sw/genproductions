import sys
import os
# import numpy as np

verbose = False
#cardsPath = sys.argv[1]
# model = sys.argv[1] # either 2hdm or zpb

bbProcess=False
ggProcess=True

# def production():
#     print ("entered")
if bbProcess:
    cardsPath = 'bbProduction/'
    prefix    = '2HDMa_bb_sinp_theta_tanb_beta_mXd_10_MH3_YY_MH4_XX_MH2_YY_MHC_YY'
elif ggProcess:
    cardsPath = 'ggProduction/'
    prefix    = '2HDMa_gg_sinp_theta_tanb_beta_mXd_10_MH3_YY_MH4_XX_MH2_YY_MHC_YY'
else:
    print ("Please provide corect production")
    sys.exit(0)

d_cardspath = os.path.join(cardsPath, prefix)


if verbose: print (d_cardspath)

d_run_card       = os.path.join(d_cardspath, prefix +'_run_card.dat')
d_proc_card      = os.path.join(d_cardspath, prefix +'_proc_card.dat')
d_extramodels    = os.path.join(d_cardspath, prefix +'_extramodels.dat')
d_customizecards = os.path.join(d_cardspath, prefix +'_customizecards.dat')
# d_cutcards       = os.path.join(d_cardspath, prefix +'_cuts.f')

# print ()

#
if verbose: print (d_run_card, d_proc_card, d_extramodels, d_customizecards)
    # return




def change_cards(d_cardname, cardname, MH4, MH3,tb,st):
    f    = open(d_cardname, 'r')
    fout = open (cardname, 'w')
    tb_1=tb.split('.')[0]
    if len(tb.split('.')) > 1 :
        tb_2=tb.split('.')[1]
    else:tb_2='0'
    st_1=st.split('.')[0]
    if len(st.split('.')) > 1:
        st_2=st.split('.')[1]
    else:st_2='0'

    for line in f:
        line = line.replace('XX', str(MH4))
        line = line.replace('YY', str(MH3))
        if 'proc_card' in cardname:
            line = line.replace("beta",str(tb_1)+"p"+str(tb_2))
            line = line.replace("theta",str(st_1)+"p"+str(st_2))
        else:
            line = line.replace('beta', str(tb))
            line = line.replace('theta', str(st))

        fout.write(line)
    fout.close()
    print ("Cardname",cardname)



#mA,ma,tb,st
def submitgrid(MH3,MH4,tb,st):
    tb_1=tb.split('.')[0]
    if len(tb.split('.')) > 1 :
        tb_2=tb.split('.')[1]
    else:tb_2='0'
    st_1=st.split('.')[0]
    if len(st.split('.')) > 1:
        st_2=st.split('.')[1]
    else:st_2='0'

    cardspath = d_cardspath.replace("XX",str(MH4)).replace("YY",str(MH3)).replace("theta",str(st_1)+"p"+str(st_2)).replace("beta",str(tb_1)+"p"+str(tb_2))
    print ("cardpath",cardspath)
    os.system('mkdir '+cardspath)

    run_card       = d_run_card.replace("XX",str(MH4)).replace("YY",str(MH3)).replace("theta",str(st_1)+"p"+str(st_2)).replace("beta",str(tb_1)+"p"+str(tb_2))
    proc_card      = d_proc_card.replace("XX",str(MH4)).replace("YY",str(MH3)).replace("theta",str(st_1)+"p"+str(st_2)).replace("beta",str(tb_1)+"p"+str(tb_2))
    extramodels    = d_extramodels.replace("XX",str(MH4)).replace("YY",str(MH3)).replace("theta",str(st_1)+"p"+str(st_2)).replace("beta",str(tb_1)+"p"+str(tb_2))
    customizecards = d_customizecards.replace("XX",str(MH4)).replace("YY",str(MH3)).replace("theta",str(st_1)+"p"+str(st_2)).replace("beta",str(tb_1)+"p"+str(tb_2))
    # cutcards = d_cutcards.replace("XX",str(MH4)).replace("YY",str(Mchi))

    change_cards(d_run_card, run_card, MH4, MH3,tb,st)
    change_cards(d_proc_card, proc_card, MH4, MH3,tb,st)
    change_cards(d_extramodels, extramodels, MH4, MH3,tb,st)
    change_cards(d_customizecards, customizecards, MH4, MH3,tb,st)
    # change_cards(d_cutcards, cutcards, MH4, Mchi)

    outdir = prefix.replace("XX",str(MH4)).replace("YY",str(MH3)).replace("theta",str(st_1)+"p"+str(st_2)).replace("beta",str(tb_1)+"p"+str(tb_2))
    print ("output dir",outdir)
    os.system('nohup ./submit_cmsconnect_gridpack_generation.sh  '+ outdir +' '+ cardspath +'  4 "4 Gb" > mysubmit_'+outdir+'.debug 2>&1 &')
    #print ("12000 12000 2nw  outdir  cardspath   8nh")


# MH4=[50,100,350,400,500,900]
# Mchi=[1,10]
#
# for i in Mchi:
#     for j in MH4:
#         mh4=j
#         mchi=i
#         print ("Mchi= ", mchi, "MH4= ",mh4)
#         submitgrid(mh4, mchi)

f=open('par_scans_2HDMa.txt','r')

for line in f.readlines()[1:]:
    mA=line.split()[0]
    ma=line.split()[1]
    tb=line.split()[3]
    st=line.split()[4]
    prod=line.split()[5]
    if (prod=='gg') and ggProcess:
        print (prod)
        submitgrid(mA,ma,tb,st)
    elif (prod=='bb') and bbProcess:
        print (prod)
        submitgrid(mA,ma,tb,st)
    else:
        print ("process not found or switch on the process")

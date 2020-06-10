#!/usr/bin/python

import os
import re

#5000000 s0
#5000001 a0
particle = "s0"
#frblock --> rtu,rtc,rtt
rtu = 0.0
rtc = 0.4
rtt = 0.0
mass = [200, 250, 300, 350, 400, 450, 500, 550, 600, 650, 700, 750, 800, 850, 900, 950, 1000]
#these values are for rtu,rtc,rtt=0.0,0.4,0.0
decay_width_a0_rtc = [
0.1293450862158179,
0.6620367149982869,
1.2908503997418217,
1.922771826658552,
2.537695807588128,
3.1332454135298784,
3.7114205206577124,
4.274993296127559,
4.826529926450234,
5.368176477386871,
5.901667474368654,
6.428390460485142,
6.949455137894594,
7.465752857356995,
7.978004202103315,
8.486795980248855,
8.992609692246806
]

decay_width_s0_rtc = [
0.12934508621581797, #200
0.6620367149982869, #250
1.2908503997418217, #300
1.922771826658552, #350
2.537695807588128, #400
3.1332454135298784, #450
3.7114205206577124, #500
4.274993296127559, #550
4.826529926450234, #600
5.368176477386871, #650
5.901667474368654, #700
6.428390460485142, #750
6.949455137894594, #800
7.465752857356995, #850
7.978004202103315, #900
8.486795980248855, #950
8.992609692246806 #1000
]

#these values are for rtu,rtc,rtt=0.1,0.0,0.0
decay_width_a0_rtu = [
0.0080840678884886,
0.0413772946873929,
0.0806781499838639,
0.1201732391661595,
0.1586059879742580,
0.1958278383456174,
0.2319637825411070,
0.2671870810079724,
0.3016581204031396,
0.3355110298366795,
0.3688542171480409,
0.4017744037803214,
0.4343409461184121,
0.4666095535848122,
0.4986252626314572,
0.5304247487655535,
0.5620381057654253
]

decay_with_s0_rtu = [
0.00809400285662884,
0.04138524022248766,
0.08068477117696429,
0.12017891447110392,
0.15861095386980853,
0.19583225247851266,
0.23196775526333500,
0.26719069257552075,
0.30166143100808870,
0.33551408578070920,
0.36885705481110050,
0.40177705226641014,
0.43434342907455786,
0.46661189048505650,
0.49862746970418190,
0.53042683967677420,
0.56204009213126250
]

temp_run_card = 'ttc/g2HDM_ttc_a0_run_card.dat'
temp_proc_card = 'ttc/g2HDM_ttc_a0_proc_card.dat'
temp_extramodels = 'ttc/g2HDM_ttc_a0_extramodels.dat'
temp_customizecards= 'ttc/g2HDM_ttc_a0_customizecards.dat'

with open(temp_customizecards) as f_cust:
    cust = f_cust.read().splitlines()

for ind, m in enumerate(mass):
    if rtu > 0.000001 and rtc < 0.000001:
	width_s0 = decay_width_s0_rtu[ind]
	width_a0 = decay_width_a0_rtu[ind]
    if rtu < 0.000001 and rtc > 0.000001:
	width_s0 = decay_width_s0_rtc[ind]
	width_a0 = decay_width_a0_rtc[ind]
    print str(ind)+" "+str(m)+" "+str(width_s0)+" "+str(width_a0)
    f_name = "ttc_"+particle+"_M"+str(m)+"_rhotu"+str(rtu).replace('.','') \
		 +"_rhotc"+str(rtc).replace('.','')+"_rhott"+str(rtt).replace('.','')

    print "creating folder: "+f_name
    os.system('mkdir -p %s' %f_name)
    os.system('cp ttc/g2HDM_ttc_a0_extramodels.dat '+f_name+'/g2HDM_'+\
		f_name+'_extramodels.dat')
    os.system('cp ttc/g2HDM_ttc_a0_run_card.dat '+f_name+'/g2HDM_'+\
		f_name+'_run_card.dat')
    print str(cust[0].split(' ')[4])
    cust[0]=cust[0].replace(str(cust[0].split(' ')[4]),str(rtu))
    cust[1]=cust[1].replace(str(cust[1].split(' ')[4]),str(rtc))
    cust[2]=cust[2].replace(str(cust[2].split(' ')[4]),str(rtt))
    if "s0" in particle:
        old = str(cust[3].split(' ')[4])
        temp = str(m).join(str(cust[3]).rsplit(old,1))
        cust[3] = temp
        cust[4]=cust[4].replace(str(cust[4].split(' ')[4]),str(0.0))
        cust[5]=cust[5].replace(str(cust[5].split(' ')[4]),str(width_s0))
        cust[6]=cust[6].replace(str(cust[6].split(' ')[4]),str(0.0))
    if "a0" in particle:
        cust[3]=cust[3].replace(str(cust[3].split(' ')[4]),str(0.0))
        old = str(cust[4].split(' ')[4])
        temp = str(m).join(str(cust[4]).rsplit(old,1))
        cust[4] = temp
        cust[5]=cust[5].replace(str(cust[5].split(' ')[4]),str(0.0))
        cust[6]=cust[6].replace(str(cust[6].split(' ')[4]),str(width_a0))
    print cust

    proc = open(temp_proc_card,'r')
    f_name_proc = f_name+'/g2HDM_'+f_name+'_proc_card.dat'
    f_name_cust = f_name+'/g2HDM_'+f_name+'_customizecards.dat'
    with open(f_name_proc,'w') as proc_file:
        for line in proc:
            if "s0" in particle:
                linep = line.replace('/ s0','/ a0').replace('_a0','_s0').replace('g2HDM_ttc_'+particle,'g2HDM_'+f_name)
            else:
                linep = line.replace('g2HDM_ttc_'+particle,'g2HDM_'+f_name)
            proc_file.write(linep)
    with open(f_name_cust,'w') as cust_file:
	for el in cust:
	    print >> cust_file, el

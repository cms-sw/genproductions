#!/usr/bin/python

import os
import re
import sys
import collections

n = len(sys.argv)
if n < 2:
    print("enter (a0 or s0) and (rtu or rtc) and rho value")
    sys.exit()

particle = sys.argv[1]
rho = sys.argv[2]
rho_v = sys.argv[3]

#5000000 s0
#5000001 a0
#particle = "a0"
#frblock --> rtu,rtc,rtt
rtu = 0.0
rtc = 0.0
rtt = 0.0

if rho == "rtu":
    rtu = rho_v
if rho == "rtc":
    rtc = rho_v

mass = [200, 250, 300, 350, 400, 450, 500, 550, 600, 650, 700, 750, 800, 850, 900, 950, 1000]
#decay widths a0=s0:
rtc01=[
0.00808407,
0.0413773, 
0.0806781, 
0.120173, 
0.158606,
0.195828,
0.231964,
0.267187,
0.301658,
0.335511,
0.368854,
0.401774,
0.434341,
0.46661, 
0.498625,
0.530425,
0.562038
]

rtc04 = [
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


rtc08 = [
0.51738,
2.64815,
5.1634,
7.69109,
10.1508,
12.533,
14.8457,
17.1,
19.3061,
21.4727,
23.6067,
25.7136,
27.7978,
29.863,
31.912,
33.9472,
35.9704
]

rtc10= [
0.808407,
4.13773,
8.06781,
12.0173,
15.8606,
19.5828,
23.1964,
26.7187,
30.1658,
33.5511,
36.8854,
40.1774,
43.4341,
46.661,
49.8625,
53.0425,
56.2038
]

rtu01 = [
0.008094,
0.0413852,
0.0806848,
0.120179,
0.158611,
0.195832,
0.231968,
0.267191,
0.301661,
0.335514,
0.368857,
0.401777,
0.434343,
0.466612,
0.498627,
0.530427,
0.56204
]

rtu04 = [
0.129504,
0.662164,
1.29096,
1.92286,
2.53778,
3.13332,
3.71148,
4.27505,
4.82658,
5.36823,
5.90171,
6.42843,
6.94949,
7.46579,
7.97804,
8.48683,
8.99264
]

rtu08 = [
0.518016,
2.64866,
5.16383,
7.69145,
10.1511,
12.5333,
14.8459,
17.1002,
19.3063,
21.4729,
23.6069,
25.7137,
27.798,
29.8632,
31.9122,
33.9473,
35.9706
]

rtu10 = [
0.8094,
4.13852,
8.06848,
12.0179,
15.8611,
19.5832,
23.1968,
26.7191,
30.1661,
33.5514,
36.8857,
40.1777,
43.4343,
46.6612,
49.8627,
53.0427,
56.204
]


d = collections.OrderedDict({
    'rtc01': rtc01,
    'rtc04': rtc04,
    'rtc08': rtc08,
    'rtc10': rtc10,
    'rtu01': rtu01,
    'rtu04': rtu04,
    'rtu08': rtu08,
    'rtu10': rtu10
})

#for key in d:
#    print(key)

temp_run_card = 'ttc/g2HDM_ttc_interference_run_card.dat'
temp_proc_card = 'ttc/g2HDM_ttc_interference_proc_card.dat'
temp_extramodels = 'ttc/g2HDM_ttc_interference_extramodels.dat'
temp_customizecards= 'ttc/g2HDM_ttc_interference_customizecards.dat'

with open(temp_customizecards) as f_cust:
    cust = f_cust.read().splitlines()

for key in d:
    for ind, m in enumerate(mass):
        if  m < 700:
            continue
        print(key)
        width = d[key][ind]
        width_ot = d[key][ind-1]
        print str(ind)+" "+str(m)+"  "+str(width)+" "+str(width_ot)

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
            cust[5]=cust[5].replace(str(cust[5].split(' ')[4]),str(width))
            cust[6]=cust[6].replace(str(cust[6].split(' ')[4]),str(0.0))
        if "a0" in particle:
            cust[3]=cust[3].replace(str(cust[3].split(' ')[4]),str(0.0))
            old = str(cust[4].split(' ')[4])
            temp = str(m).join(str(cust[4]).rsplit(old,1))
            cust[4] = temp
            cust[5]=cust[5].replace(str(cust[5].split(' ')[4]),str(0.0))
            cust[6]=cust[6].replace(str(cust[6].split(' ')[4]),str(width))
        print(cust)

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

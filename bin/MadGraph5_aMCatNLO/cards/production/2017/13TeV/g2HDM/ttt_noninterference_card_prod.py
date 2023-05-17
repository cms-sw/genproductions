#!/usr/bin/python

import os
import re
import sys
import collections

n = len(sys.argv)
if n < 2:
    print("enter particle type (a0 or s0), rtc and rtt rho values")
    print("example: ./.... a0 0.4 1.0")
    print("example: ./.... s0 1.0 1.0")
    sys.exit()


particle = sys.argv[1]
#rho = sys.argv[2]
#rho_v = sys.argv[3]

#5000000 s0
#5000001 a0
#particle = "a0"
#frblock --> rtu,rtc,rtt
rtu = 0.0
rtc = sys.argv[2]
rtt = sys.argv[3]



#if rho == "rtu":
#    rtu = rho_v
#if rho == "rtc":
#    rtc = rho_v
#if rho == "rtt":
#    rtt = rho_v

mass = [520,550,700,1000]

d_s0 = collections.OrderedDict({
('s0,0.4,0.4',520):	6.03258,
('s0,0.4,1.0',520):	26.7094,
('s0,1.0,0.4',520):	17.0268,
('s0,1.0,1.0',520):	37.7036,
('s0,0.4,0.4',550):	6.76989,
('s0,0.4,1.0',550):	29.2136,
('s0,1.0,0.4',550):	19.8681,
('s0,1.0,1.0',550):	42.3118,
('s0,0.4,0.4',700):	10.3174,
('s0,0.4,1.0',700):	41.3011,
('s0,1.0,0.4',700):	33.4999,
('s0,1.0,1.0',700):	64.4836,
('s0,0.4,0.4',1000):	16.898 ,
('s0,0.4,1.0',1000):	64.1093,
('s0,1.0,0.4',1000):	58.4016,
('s0,1.0,1.0',1000):	105.613
})

d_a0 = collections.OrderedDict({
('a0,0.4,0.4',520):	7.66223,
('a0,0.4,1.0',520):	28.3391,
('a0,1.0,0.4',520):	27.2121,
('a0,1.0,1.0',520):	47.889 ,
('a0,0.4,0.4',550):	8.37301,
('a0,0.4,1.0',550):	30.8167,
('a0,1.0,0.4',550):	29.8876,
('a0,1.0,1.0',550):	52.3313,
('a0,0.4,0.4',700):	11.7233,
('a0,0.4,1.0',700):	42.7071,
('a0,1.0,0.4',700):	42.287 ,
('a0,1.0,1.0',700):	73.2708,
('a0,0.4,0.4',1000):	17.9591,
('a0,0.4,1.0',1000):	65.1703,
('a0,1.0,0.4',1000):	65.0332,
('a0,1.0,1.0',1000):	112.244
})

if 'a0' in particle:
    d = d_a0
elif 's0' in particle:
    d = d_s0
else:
    print("Select a0 or s0 for particle name. Quitting...")
    sys.exit()



temp_run_card = 'ttt/g2HDM_ttt_a0_run_card.dat'
temp_proc_card = 'ttt/g2HDM_ttt_a0_proc_card.dat'
temp_extramodels = 'ttt/g2HDM_ttt_a0_extramodels.dat'
temp_customizecards= 'ttt/g2HDM_ttt_a0_customizecards.dat'

with open(temp_customizecards) as f_cust:
    cust = f_cust.read().splitlines()

input_sel = particle+","+rtc+","+rtt
print(input_sel)
for ind in mass:
    width = d[input_sel,ind]
    print(input_sel,ind, width)
    f_name = "ttt_"+particle+"_M"+str(ind)+"_rhotu"+str(rtu).replace('.','') \
		 +"_rhotc"+str(rtc).replace('.','')+"_rhott"+str(rtt).replace('.','') 
    print "creating folder: "+f_name
    os.system('mkdir -p %s' %f_name)
    os.system('cp ttt/g2HDM_ttt_a0_extramodels.dat '+f_name+'/g2HDM_'+\
		f_name+'_extramodels.dat')
    os.system('cp ttt/g2HDM_ttt_a0_run_card.dat '+f_name+'/g2HDM_'+\
		f_name+'_run_card.dat')
    print str(cust[0].split(' ')[4])   
    cust[0]=cust[0].replace(str(cust[0].split(' ')[4]),str(rtu))
    cust[1]=cust[1].replace(str(cust[1].split(' ')[4]),str(rtc))
    cust[2]=cust[2].replace(str(cust[2].split(' ')[4]),str(rtt))
    if "s0" in particle:
        old = str(cust[3].split(' ')[4])
        temp = str(ind).join(str(cust[3]).rsplit(old,1))
        cust[3] = temp
        cust[4]=cust[4].replace(str(cust[4].split(' ')[4]),str(0.0))
        cust[5]=cust[5].replace(str(cust[5].split(' ')[4]),str(width))
        cust[6]=cust[6].replace(str(cust[6].split(' ')[4]),str(0.0))
    if "a0" in particle:
        cust[3]=cust[3].replace(str(cust[3].split(' ')[4]),str(0.0))
        old = str(cust[4].split(' ')[4])
        temp = str(ind).join(str(cust[4]).rsplit(old,1))
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
                linep = line.replace('a0','s0').replace('_a0','_s0').replace('g2HDM_ttt_'+particle,'g2HDM_'+f_name)
            else:
                linep = line.replace('g2HDM_ttt_'+particle,'g2HDM_'+f_name)
            proc_file.write(linep)
    with open(f_name_cust,'w') as cust_file:
        for el in cust:
            print >> cust_file, el

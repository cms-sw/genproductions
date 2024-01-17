#! /bin/env python

import os, shutil, subprocess

cgnames =["n0_20000000", "n0_04000000", "n0_02000000", "n0_01000000", "n0_00500000","n0_00250000", "n0_00125000", "n0_00025000", "0_00000000", "0_00025000","0_00125000", "0_00250000", "0_00500000", "0_01000000", "0_02000000","0_04000000", "0_20000000"]
cbtwl4names =["n7_5000", "n5_0000", "n2_5000", "n1_2500", "n0_6250","n0_3125", "n0_1042", "n0_0208", "0_0000", "0_0208","0_1042", "0_3125", "0_6250", "1_2500", "2_5000","5_0000", "7_5000"]
cgmnames =["n10_0000", "n6_6667", "n10_0000", "n2_2222", "n1_1111","n0_5556", "n0_1852", "n0_0370", "0_0000", "0_0370","0_1852", "0_5556", "1_1111", "2_2222", "4_4444","6_6667", "10_0000"]


cgs =["-0.2000000000", "-0.0400000000", "-0.0200000000", "-0.0100000000", "-0.0050000000","-0.0025000000", "-0.0012500000", "-0.0002500000", "0.0000000000", "0.0002500000","0.0012500000", "0.0025000000", "0.0050000000", "0.0100000000", "0.0200000000","0.0400000000", "0.2000000000"]
cbtwl4s =["-7.5000000000", "-5.0000000000", "-2.5000000000", "-1.2500000000", "-0.6250000000","-0.3125000000", "-0.1041700000", "-0.0208330000", "0.0000000000", "0.0208330000","0.1041700000", "0.3125000000", "0.6250000000", "1.2500000000", "2.5000000000","5.0000000000", "7.5000000000"]
cgms =["-10.0000000000", "-6.6667000000", "-10.0000000000", "-2.2222000000", "-1.1111000000","-0.5555600000", "-0.1851900000", "-0.0370370000", "0.0000000000", "0.0370370000","0.1851900000", "0.5555600000", "1.1111000000", "2.2222000000", "4.4444000000","6.6667000000", "10.0000000000"]



f=open("ZGtoLLG01j_5f_LO_mlm_nTGC_sjtu_reweight_card.dat","w")
f.write("change rwgt_dir ./rwgt\n\n")
a = 0
for cg in cgs:
	a += 1
	f.write("launch --rwgt_name=ZG_cg_%s_%s\nset cg %s\nset cgm 0\nset CBtWL4 0\nset CBWL4 0\nset CWWL4 0\nset CBBL4 0\n\n" % (str(a),str(cgnames[a-1]),str(cg)))
a = 0
for cbtwl4 in cbtwl4s:
	a += 1
	f.write("launch --rwgt_name=ZG_cbtwl4_%s_%s\nset cg 0\nset cgm 0\nset CBtWL4 %s\nset CBWL4 0\nset CWWL4 0\nset CBBL4 0\n\n" % (str(a),str(cbtwl4names[a-1]),str(cbtwl4)))
a = 0
for cgm in cgms:
        a += 1
        f.write("launch --rwgt_name=ZG_cgm_%s_%s\nset cg 0\nset cgm %s\nset CBtWL4 0\nset CBWL4 0\nset CWWL4 0\nset CBBL4 0\n\n" % (str(a),str(cgmnames[a-1]),str(cgm)))

f.close

#for para in paras:
#    sampleName = 'ZG_nTGC_new_' + str(para)
#    print sampleName
#    if os.path.isdir(sampleName):
#        shutil.rmtree(sampleName)
#    os.makedirs(sampleName)
#    # Copy cards
#    shutil.copyfile(Ref_dir + Ref_name + '_run_card.dat',sampleName+'/'+sampleName+'_run_card.dat')
#    with open("{0}/{0}_customizecards.dat".format(sampleName), "w") as f:
#        f.write("set param_card %s %s\n" % (block, str(para)))

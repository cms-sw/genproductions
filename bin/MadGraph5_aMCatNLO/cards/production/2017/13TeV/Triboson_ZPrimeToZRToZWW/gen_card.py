import os
R1=["0.12","0.2","0.3","0.4","0.5","0.6","0.7","0.9"]  # ratio: mR/mZkk for mZkk in [1500]
R2=["0.1","0.2","0.3","0.4","0.5","0.6","0.7","0.9"]   # ratio: mR/mZkk for mZkk in [2000]
R3=["0.08","0.1","0.2","0.3","0.4","0.5","0.6","0.7","0.9"] # ratio: mR/mZkk for mZkk in [2500]
R4=["0.06","0.08","0.1","0.2","0.3","0.4","0.5","0.6","0.7","0.9"] # ratio: mR/mZkk for mZkk in [3000,3500,4000,4500,5000]

for m in [1500,2000,2500,3000,3500,4000,4500,5000]:
	os.system("mkdir Triboson_M%s"%(m))
        if m==1500: R=R1
        if m==2000: R=R2
        if m==2500: R=R3
        if m in [3000,3500,4000,4500,5000]:R=R4
	for r in R:
		print "generating cards for M = %s GeV R = %s"%(m,r)
		os.system("mkdir Triboson_M%s/Triboson_M%s_R%s"%(m,m,r))
		fwidth=open('width.txt')
		for l in fwidth:
			l=l.replace("\n","").split(" ")
			if (int)(l[0])==m and l[1]==r and l[2]=="width_zkk":width_zkk=l[3]
			if (int)(l[0])==m and l[1]==r and l[2]=="width_r":width_r=l[3];break
		mR=m*(float)(r)
		for postfix in ["_run_card.dat","_customizecards.dat","_proc_card.dat","_extramodels.dat"]:	
			os.system("cp Triboson_M_R/Triboson_M_R%s Triboson_M%s/Triboson_M%s_R%s/Triboson_M%s_R%s%s"%(postfix,m,m,r,m,r,postfix))
			if postfix =="_proc_card.dat":
				os.system("sed -i 's/<MASS>/%s/g' Triboson_M%s/Triboson_M%s_R%s/Triboson_M%s_R%s%s"%(m,m,m,r,m,r,postfix))
				os.system("sed -i 's/<R>/%s/g' Triboson_M%s/Triboson_M%s_R%s/Triboson_M%s_R%s%s"%(r,m,m,r,m,r,postfix))
			if postfix =="_customizecards.dat":
				os.system("sed -i 's/<MASS>/%s/g' Triboson_M%s/Triboson_M%s_R%s/Triboson_M%s_R%s%s"%(m,m,m,r,m,r,postfix))
				os.system("sed -i 's/<MASS_R>/%s/g' Triboson_M%s/Triboson_M%s_R%s/Triboson_M%s_R%s%s"%(mR,m,m,r,m,r,postfix))
				os.system("sed -i 's/<WIDTH>/%s/g' Triboson_M%s/Triboson_M%s_R%s/Triboson_M%s_R%s%s"%(width_zkk,m,m,r,m,r,postfix))
				os.system("sed -i 's/<WIDTH_R>/%s/g' Triboson_M%s/Triboson_M%s_R%s/Triboson_M%s_R%s%s"%(width_r,m,m,r,m,r,postfix))
		fwidth.close()

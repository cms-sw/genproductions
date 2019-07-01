###
from shutil import copyfile
import os
import csv

#Xmasses = [500, 600, 700, 800, 1000, 1200, 1500, 1800, 2000, 2500]
#amasses = [25, 35, 50, 65, 75]

Xmasses = [1000]
amasses = [50]

def Template_Replace(F, O, R):
	with open(F, 'r') as file :
		filedata = file.read()
	filedata = filedata.replace(O, R)
	with open(F, 'w') as file:
		file.write(filedata)

def getWidths(X,a):
	with open("ParticleWidths.csv") as csvfile:
		W = csv.reader(csvfile)
		for r in W:
			if (X == int(r[0]) and a == int(r[1])):
				XW = r[2]
				aW = r[3]
	Widths = ['%e'%float(XW), '%e'%float(aW), XW, aW]
	return Widths
	

def main():
	for X in Xmasses:
		for a in amasses:
			folder = "XtoAAto4b_X"+str(X)+"A"+str(a)
			try:
				os.stat("../"+folder)
			except:
    				os.mkdir("../"+folder) 
			filename = "XtoAAto4b_X"+str(X)+"A"+str(a)
			param_Xmass = '%e'%X
			param_amass = '%e'%a
			W = getWidths(X,a)
			param_Xwidth = W[0]
			param_awidth = W[1]
			param_Xwidth_ns = W[2]
			param_awidth_ns = W[3]
			copyfile("run_card_template.dat", "../"+folder+"/"+filen
ame+"_run_card.dat")
			copyfile("extramodels_template.dat", "../"+folder+"/"+fi
lename+"_extramodels.dat")
			copyfile("proc_card_template.dat", "../"+folder+"/"+file
name+"_proc_card.dat")
			copyfile("param_card_template.dat", "../"+folder+"/"+fil
ename+"_param_card.dat")
			copyfile("madspin_card_template.dat", "../"+folder+"/"+f
ilename+"_madspin_card.dat")
			Template_Replace("../"+folder+"/"+filename+"_proc_card.d
at", "%PROC_NAME%", folder+"/"+filename)
			Template_Replace("../"+folder+"/"+filename+"_param_card.
dat", "%X_MASS%", param_Xmass)
			Template_Replace("../"+folder+"/"+filename+"_param_card.
dat", "%A_MASS%", param_amass)
			Template_Replace("../"+folder+"/"+filename+"_param_card.
dat", "%X_WIDTH%", param_Xwidth)
			Template_Replace("../"+folder+"/"+filename+"_param_card.
dat", "%A_WIDTH%", param_awidth)
			Template_Replace("../"+folder+"/"+filename+"_param_card.
dat", "%X_WIDTH_NS%", param_Xwidth_ns)
			Template_Replace("../"+folder+"/"+filename+"_param_card.
dat", "%A_WIDTH_NS%", param_awidth_ns)
			


if __name__ == "__main__":
	main()

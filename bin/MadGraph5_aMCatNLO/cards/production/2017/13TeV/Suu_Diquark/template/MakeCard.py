########################################################################################
## python MakeCard.py
########################################################################################
from shutil import copyfile
import os
import csv

def Template_Replace(F, O, R):
	with open(F, 'r') as file :
		filedata = file.read()
	filedata = filedata.replace(O, R)
	with open(F, 'w') as file:
		file.write(filedata)

with open("MassWidthVals.csv") as csvfile:
	W = csv.reader(csvfile)
	for r in W:
		R = r[0].split()
		N = "Suu_Diquark_S"+R[0]+"_chi"+R[1]
		print "Creating: " + N
		try:
			os.stat("./"+N)
		except:
			os.mkdir("./"+N)

		copyfile("SuuDQTemplate_customizecards.dat", "./"+N+"/"+N+"_customizecards.dat")
		copyfile("SuuDQTemplate_extramodels.dat", "./"+N+"/"+N+"_extramodels.dat")
		copyfile("SuuDQTemplate_proc_card.dat", "./"+N+"/"+N+"_proc_card.dat")
		copyfile("SuuDQTemplate_madspin_card.dat", "./"+N+"/"+N+"_madspin_card.dat")
		copyfile("SuuDQTemplate_run_card.dat", "./"+N+"/"+N+"_run_card.dat")
		Template_Replace("./"+N+"/"+N+"_proc_card.dat", "$NAME$", N)
		Template_Replace("./"+N+"/"+N+"_customizecards.dat", "$SUUMASS$", '%e'%float(R[0]))
		Template_Replace("./"+N+"/"+N+"_customizecards.dat", "$SUUWIDTH$", '%e'%float(R[2]))
		Template_Replace("./"+N+"/"+N+"_customizecards.dat", "$DQMASS$", '%e'%float(R[1]))
		Template_Replace("./"+N+"/"+N+"_customizecards.dat", "$DQWIDTH$", '%e'%float(R[3]))

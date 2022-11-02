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

with open("MassVals.csv") as csvfile:
	W = csv.reader(csvfile)
	for r in W:
		R = r[0].split()
		N = "st_jj_rpv_LO_M1-"+R[0]
		print "Creating: " + N
		try:
			os.stat("./"+N)
		except:
			os.mkdir("./"+N)

		copyfile("st_jj_rpvTemplate_customizecards.dat", "./"+N+"/"+N+"_customizecards.dat")
		copyfile("st_jj_rpvTemplate_extramodels.dat", "./"+N+"/"+N+"_extramodels.dat")
		copyfile("st_jj_rpvTemplate_proc_card.dat", "./"+N+"/"+N+"_proc_card.dat")
		copyfile("st_jj_rpvTemplate_run_card.dat", "./"+N+"/"+N+"_run_card.dat")
		Template_Replace("./"+N+"/"+N+"_proc_card.dat", "$NAME$", N)
		Template_Replace("./"+N+"/"+N+"_customizecards.dat", "$STMASS$", '%e'%int(R[0]))

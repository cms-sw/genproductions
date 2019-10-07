from shutil import copyfile
import os
import csv

def Template_Replace(F, O, R):
	with open(F, 'r') as file :
		filedata = file.read()
	filedata = filedata.replace(O, R)
	with open(F, 'w') as file:
		file.write(filedata)

with open("ParticleWidths.csv") as csvfile:
	W = csv.reader(csvfile)
	for R in W:
		print R
		N = "XtoAAto4b_X"+R[0]+"A"+R[1]
		print "Creating: " + N
		try:
			os.stat("./"+N)
		except:
			os.mkdir("./"+N)

		copyfile("customizecards_template.dat", "./"+N+"/"+N+"_customizecards.dat")
		copyfile("extramodels_template.dat", "./"+N+"/"+N+"_extramodels.dat")
		copyfile("proc_card_template.dat", "./"+N+"/"+N+"_proc_card.dat")
		copyfile("madspin_card_template.dat", "./"+N+"/"+N+"_madspin_card.dat")
		copyfile("run_card_template.dat", "./"+N+"/"+N+"_run_card.dat")
		Template_Replace("./"+N+"/"+N+"_proc_card.dat", "$NAME$", N)
		Template_Replace("./"+N+"/"+N+"_costumizecards.dat", "$XMASS$", '%e'%float(R[0]))
		Template_Replace("./"+N+"/"+N+"_costumizecards.dat", "$XWIDTH$", '%e'%float(R[2]))
		Template_Replace("./"+N+"/"+N+"_costumizecards.dat", "$AMASS$", '%e'%float(R[1]))
		Template_Replace("./"+N+"/"+N+"_costumizecards.dat", "$AWIDTH$", '%e'%float(R[3]))
#!/usr/bin/env python
import sys, os, pwd, commands
from subprocess import *
import optparse, shlex, re
import math
import time
from decimal import *
import json
### Define function for processing of os command
def processCmd(cmd, quiet = 0):
    #print cmd
    #status, output = commands.getstatusoutput(cmd)
    #output = subprocess.check_output(cmd, shell=True)

    output = '\n'
    p = Popen(cmd, shell=True, stdout=PIPE, stderr=STDOUT,bufsize=-1)
    #p = subprocess.Popen(cmd, stderr=subprocess.STDOUT, stdout=subprocess.PIPE)

    for line in iter(p.stdout.readline, ''):
        output=output+str(line)
        print line,
    p.stdout.close()
    if p.wait() != 0:
        raise RuntimeError("%r failed, exit status: %d" % (cmd, p.returncode))

    if (not quiet):
        print 'Output:\n   ['+output+'] \n'
    return output

mass_points = [1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100,110,120,130,140,150,160,170,180,190,200,250,300]
#mass_points = [80,85,90,95,100,110,120,130,140,150,160,170,180,190,200,250,300]
file_to_copy = ["Zp_to_3mu_Mass_customizecards.dat","Zp_to_3mu_Mass_extramodels.dat","Zp_to_3mu_Mass_proc_card.dat","Zp_to_3mu_Mass_run_card.dat"]
current_directory = os.getcwd()
print "current path:", current_directory
for mass in mass_points:
	cmd = "mkdir -p "+str(current_directory)+"/Zp_to_3mu_M"+str(mass)
        processCmd(cmd,1)
        for file_name in file_to_copy:
        	cmd = "cp "+ str(current_directory)+"/Zp_to_3mu_template"+"/"+str(file_name)+" "+str(current_directory)+"/Zp_to_3mu_M"+str(mass)+"/"
                processCmd(cmd,1)
	for file_name in file_to_copy:
		if ("extramodels" in file_name):
                	cmd = "mv "+ str(current_directory)+"/Zp_to_3mu_M"+str(mass)+"/"+"Zp_to_3mu_Mass_extramodels.dat"+ " " +str(current_directory)+"/Zp_to_3mu_M"+str(mass)+"/"+"Zp_to_3mu_M"+str(mass)+"_extramodels.dat"                 
                        processCmd(cmd,1)       
		if ("customizecards" in file_name):
                        cmd = "mv "+ str(current_directory)+"/Zp_to_3mu_M"+str(mass)+"/"+"Zp_to_3mu_Mass_customizecards.dat"+ " " +str(current_directory)+"/Zp_to_3mu_M"+str(mass)+"/"+"Zp_to_3mu_M"+str(mass)+"_customizecards.dat"
                        processCmd(cmd,1)
			os.system("sed -i s~Mass_point~"+str(mass)+"~g  "+str(current_directory)+"/Zp_to_3mu_M"+str(mass)+"/"+"Zp_to_3mu_M"+str(mass)+"_customizecards.dat")
                if ("proc_card" in file_name):
                        cmd = "mv "+ str(current_directory)+"/Zp_to_3mu_M"+str(mass)+"/"+"Zp_to_3mu_Mass_proc_card.dat"+ " " +str(current_directory)+"/Zp_to_3mu_M"+str(mass)+"/"+"Zp_to_3mu_M"+str(mass)+"_proc_card.dat"
                        processCmd(cmd,1)
                        os.system("sed -i s~Mass_point~"+str(mass)+"~g "+str(current_directory)+"/Zp_to_3mu_M"+str(mass)+"/"+"Zp_to_3mu_M"+str(mass)+"_proc_card.dat")
                if ("run_card" in file_name):
                        cmd = "mv "+ str(current_directory)+"/Zp_to_3mu_M"+str(mass)+"/"+"Zp_to_3mu_Mass_run_card.dat"+ " " +str(current_directory)+"/Zp_to_3mu_M"+str(mass)+"/"+"Zp_to_3mu_M"+str(mass)+"_run_card.dat"
                        processCmd(cmd,1)



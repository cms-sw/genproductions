#!usr/bin/python
import os
import optparse 
import sys 
import subprocess
import datetime 
import re

basedir_path = os.path.dirname(os.path.realpath(__file__))
#print basedir_path

usage = ""
parser = optparse.OptionParser(usage='\nExample: python %prog -i templates -n st_jj_rpv_LO')
parser.add_option("-i","--inputCardDir",action="store",type="string",dest="INPUTCARDDIR",default="")
parser.add_option("-n","--genProcessName",action="store",type="string",dest="GENPROCESSNAME",default="")

(options, args) = parser.parse_args()
INPUTCARDDIR = options.INPUTCARDDIR
GENPROCESSNAME = options.GENPROCESSNAME

if not options.INPUTCARDDIR:   
    parser.error('ERROR: Input card directory is not given')
if not options.GENPROCESSNAME:   
    parser.error('ERROR: Gen process name is not given')

# Create outputgridpack dir
current_time = datetime.datetime.now()
OUTPROCESSDIR = basedir_path+"/"
print ("mkdir -p %s" % OUTPROCESSDIR)
os.system("mkdir -p %s" % OUTPROCESSDIR)

print "The input card directory is:", INPUTCARDDIR
print "The output gridpacks directory is:", OUTPROCESSDIR
print "The gen process name is:", GENPROCESSNAME

# Create cards and run gridpacks

STMASSValues = [25, 50, 75, 100, 125, 150, 175, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1250, 1500, 1750, 2000, 2500, 3000]

for STMASS in STMASSValues:
    for R in [1]:
        print STMASS

        Rmod = str(R)   
        Rmod = re.sub("\.","p",Rmod)
        
        CURRENTPROCESS = GENPROCESSNAME+"_"+"M1-"+str(STMASS)
        print ("mkdir -p %s/%s" % (OUTPROCESSDIR,CURRENTPROCESS))
        os.system("mkdir -p %s/%s" % (OUTPROCESSDIR,CURRENTPROCESS))

        ##        
        RUNCARD = OUTPROCESSDIR+"/"+CURRENTPROCESS+"/"+CURRENTPROCESS+"_run_card.dat"
        template_runcard = INPUTCARDDIR+"/"+"PROCESSNAME_run_card.dat"
        with open(RUNCARD, "wt") as fout:            
            with open(template_runcard, "rt") as fin:
                for line in fin:
                    ## EDIT CARD
                    ##
                    fout.write(line)

        ##   
        PROCCARD = OUTPROCESSDIR+"/"+CURRENTPROCESS+"/"+CURRENTPROCESS+"_proc_card.dat"
        template_proccard = INPUTCARDDIR+"/"+"PROCESSNAME_proc_card.dat"
        with open(PROCCARD, "wt") as fout:            
            with open(template_proccard, "rt") as fin:
                for line in fin:
                    ## EDIT CARD
                    line = re.sub("PROCESSNAME",CURRENTPROCESS,line)                
                    ##
                    fout.write(line)

        ##
        EXTRAMODCARD = OUTPROCESSDIR+"/"+CURRENTPROCESS+"/"+CURRENTPROCESS+"_extramodels.dat"
        template_extramod = INPUTCARDDIR+"/"+"PROCESSNAME_extramodels.dat"
        with open(EXTRAMODCARD, "wt") as fout:            
            with open(template_extramod, "rt") as fin:
                for line in fin:
                    ## EDIT CARD
                    ##
                    fout.write(line)

        ##
        CUSTOMCARD = OUTPROCESSDIR+"/"+CURRENTPROCESS+"/"+CURRENTPROCESS+"_customizecards.dat"
        template_custom = INPUTCARDDIR+"/"+"PROCESSNAME_customizecards.dat"
        with open(CUSTOMCARD, "wt") as fout:            
            with open(template_custom, "rt") as fin:
                for line in fin:
                    ## EDIT CARD
                    line = re.sub("STMASS",str(STMASS),line) 
                    ##
                    fout.write(line)




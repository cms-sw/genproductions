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
parser = optparse.OptionParser(usage='\nExample: python %prog -i template_cards -n Res1ToRes2GluTo3Glu')
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

MGKKValues = [500, 750, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000]
RValues = [0.1, 0.2, 0.3, 0.5, 0.7, 0.9]

for MGKK in MGKKValues:
    for R in RValues:
        MR = int(MGKK*R)
        print MGKK, R

        Rmod = str(R)   
        Rmod = re.sub("\.","p",Rmod)
        
        CURRENTPROCESS = GENPROCESSNAME+"_"+"M1-"+str(MGKK)+"_R-"+Rmod
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
                    line = re.sub("GKKMASS",str(MGKK),line) 
                    line = re.sub("RMASS",str(MR),line) 
                    ##
                    fout.write(line)




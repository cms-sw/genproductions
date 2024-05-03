#!usr/bin/python
import os
import optparse 
import sys 
import subprocess
import datetime 
import re

basedir_path = os.path.dirname(os.path.realpath(__file__))
#print basedir_path

#madgraph_dir = "genproductions/bin/MadGraph5_aMCatNLO"
madgraph_dir = ""
card_dir = "cards/production/13TeV"
prod_dirname = madgraph_dir+"/"+card_dir

usage = ""
parser = optparse.OptionParser(usage='\nExample: python %prog -i template_cards/TrijetRes_g_ggg_BP1 -n TrijetRes_g_ggg_BP1 \n1) Create template_cards/TrijetRes_g_ggg_BP1 directory containing config files for madgraph (i.e. PROCESSNAME_customizecards.dat, PROCESSNAME_proc_card.dat, PROCESSNAME_extramodels.dat, PROCESSNAME_run_card.dat) \n2) Edit %prog to specify the parameters of the samples to be produced (i.e. MGKKValues and RValues) \n3) Launch the script %prog')
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
#GENPROCESSDIRNAME = GENPROCESSNAME+"_%04d%02d%02d_%02d%02d%02d" % (current_time.year,current_time.month,current_time.day,current_time.hour,current_time.minute,current_time.second)
#OUTPROCESSDIR = basedir_path+"/"+prod_dirname+"/"+GENPROCESSDIRNAME
OUTPROCESSDIR = basedir_path+"/"+prod_dirname+"/"
print ("mkdir -p %s" % OUTPROCESSDIR)
os.system("mkdir -p %s" % OUTPROCESSDIR)

print "The input card directory is:", INPUTCARDDIR
print "The output gridpacks directory is:", OUTPROCESSDIR
print "The gen process name is:", GENPROCESSNAME

# Create cards and run gridpacks

MZPValues = [1,2,3,4,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80]
WCValues = ["m","p"]

#MZPValues = [8,12,15,20,30,45,60]
#MZPValues = [30]
#WCValues = ["m"]

for MZP in MZPValues:
    for WC in WCValues:

        Mmod = str(MZP)
        Mmod = re.sub("\.","p",Mmod)

        CURRENTPROCESS = "W"+WC+"To"+GENPROCESSNAME+"_"+"M"+Mmod
        #print ("mkdir -p %s/%s" % (OUTPROCESSDIR,CURRENTPROCESS))
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
                    if WC=="p":
                        line = re.sub("PRODUCTS","mu+ vm mu+ mu-",line)
                    elif WC=="m":
                        line = re.sub("PRODUCTS","mu- vm~ mu+ mu-",line)
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
                    line = re.sub("ZPMASS",str(MZP),line) 
                    ##
                    fout.write(line)

        ## run gridpacks             
        run_dir = basedir_path+"/"+madgraph_dir

        os.chdir(run_dir)
        #print ("./gridpack_generation.sh %s %s/%s > /dev/null &" % (CURRENTPROCESS,card_dir,CURRENTPROCESS))
        #os.system("./gridpack_generation.sh %s %s/%s > /dev/null &" % (CURRENTPROCESS,card_dir,CURRENTPROCESS))
        #print ("./gridpack_generation.sh %s %s/%s" % (CURRENTPROCESS,card_dir,CURRENTPROCESS))
        #os.system("./gridpack_generation.sh %s %s/%s" % (CURRENTPROCESS,card_dir,CURRENTPROCESS))
        #os.system("./submit_gridpack_generation.sh 15000 50000 1nh %s %s/%s 1nh" % (CURRENTPROCESS,card_dir,CURRENTPROCESS))
        print("./submit_condor_gridpack_generation.sh %s %s/%s" % (CURRENTPROCESS,card_dir,CURRENTPROCESS))
        os.system("./submit_condor_gridpack_generation.sh %s %s/%s" % (CURRENTPROCESS,card_dir,CURRENTPROCESS))
        #os.system("./submit_cmsconnect_gridpack_generation.sh %s %s/%s" % (CURRENTPROCESS,card_dir,CURRENTPROCESS))
        os.chdir(basedir_path)



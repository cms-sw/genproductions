import os
import sys


directory = "./cards/2017/13TeV/VBFoffshell/"

for filename in os.listdir(directory):
    if filename.endswith(".input") and filename.startswith("BSIZZ"): 
        print(os.path.join(directory, filename))
        process_str = filename.rsplit('.',1)[0]
        cmnd = "python install.py --name "+process_str+" --card "+filename+" --link-mela --vbf-offshell"
        
        cmnd_check  = "python install.py --name "+process_str+" --card "+directory+filename+" --link-mela --vbf-offshell --check-jobs"
        
        print cmnd
        test = "echo test"
        fout = process_str+".check"
        f1=open(fout,'w+')

        print >> f1, cmnd_check
        os.system(cmnd)
        
        continue
    else:
        continue




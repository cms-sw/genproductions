import sys,os

process_list = ["HZJ","HWJ"] # add as many processes as required

print "downloading POWHEG-BOX-V2"
os.system("svn checkout --username anonymous --password anonymous svn://powhegbox.mib.infn.it/trunk/POWHEG-BOX-V2")

os.chdir("POWHEG-BOX-V2")

for process in process_list:
  
  print "downloading "+str(process)
  os.system("svn co --username anonymous --password anonymous svn://powhegbox.mib.infn.it/trunk/User-Processes-V2/"+str(process))
  os.system("tar cvzf "+str(process)+".tgz "+str(process))
  os.system("rm -rf "+str(process))
  
os.chdir("../")
print "compressing the repository"
os.system("tar cvzf POWHEG-BOX-V2.tgz POWHEG-BOX-V2")

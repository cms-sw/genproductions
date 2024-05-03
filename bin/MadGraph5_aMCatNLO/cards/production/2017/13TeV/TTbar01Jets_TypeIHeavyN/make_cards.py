import os,sys

xqcut = "20"

mix = ["Mu", "El"]
wp = ["w+ > ll vv", "w+ > em n1", "w+ > j j"]
wm = ["w- > ll vv", "w- > em n1", "w- > j j"]
n1 = ["n1 > em ll vv", "n1 > em j j"]
mass = ["10","15","20","30","40","50","60","70","75","85","90","95","100","120"]
mass = ["40"]
procs = [[],[],[],[]]

for i_wp in range(len(wp)):

  top = "t > b w+"
  top = top+", ("+wp[i_wp]
  if ("n1" in wp[i_wp]):
    top = "t > b "+wp[i_wp].replace("w+ > ","")

  for i_wm in range(len(wm)):

    atop = "t~ > b~ w-"
    atop = atop+", ("+wm[i_wm]
    if ("n1" in wm[i_wm]):
      atop = "t~ > b~ "+wm[i_wm].replace("w- > ","")

    for i_n1 in range(len(n1)):

      this_top = ""
      this_atop = ""
      if ("n1" in top) and ("n1" in atop):continue

      if ("n1" in top):
        this_top = top+", ("+n1[i_n1]+")"
        this_atop = atop+")"
      elif ("n1" in atop):
        this_top = top+")"
        this_atop = atop+", ("+n1[i_n1]+")"
      else: continue

      process = "("+this_top+"), ("+this_atop+")"
      if (process.count("ll")+process.count("em") == 2):
        procs[0].append(process)
      elif (process.count("ll")+process.count("em") == 3):
        if "n1 > em ll vv" in process:
          procs[1].append(process)
        else:
          procs[2].append(process)
      elif (process.count("ll")+process.count("em") == 4):
        procs[3].append(process)
      else:
        print "ERROR"
        sys.exit()

for i_procs in range(len(procs)):
  if i_procs == 0: channel = "2L"
  if i_procs == 1: channel = "HadSMTop_3L"
  if i_procs == 2: channel = "LepSMTop_3L"
  if i_procs == 3: channel = "4L"

  for i_mix in range(len(mix)):
    for i_mass in range(len(mass)):
      this_name = "TTbar01Jets_TypeIHeavyN-"+mix[i_mix]+"_"+channel+"_LO_MN"+mass[i_mass]+"_XQCUT"+xqcut
      os.system("mkdir -p "+this_name)
      os.system("cp skeleton/proc_card.dat "+this_name+"/"+this_name+"_proc_card.dat")
      process = []
      at_index = 0
      for j_procs in range(len(procs[i_procs])):
        process.append("p p > t t~, "+procs[i_procs][j_procs]+" @"+str(at_index))
        at_index = at_index + 1
        process.append("p p > t t~ j, "+procs[i_procs][j_procs]+" @"+str(at_index))
        at_index = at_index + 1
      process_line = ""
      for i_process in range(len(process)):
        if(i_process == 0): process_line = process_line+"generate "+process[i_process]+"\\n"
        else: process_line = process_line+"add process "+process[i_process]+"\\n"
      os.system("sed -i \'s|###PROCESSHERE|"+process_line+"|g\' "+this_name+"/"+this_name+"_proc_card.dat")
      os.system("sed -i \'s|###OUTPUTHERE|output "+this_name+" --nojpeg|g\' "+this_name+"/"+this_name+"_proc_card.dat")

      os.system("cp skeleton/extramodels.dat "+this_name+"/"+this_name+"_extramodels.dat")
      os.system("cp skeleton/run_card.dat "+this_name+"/"+this_name+"_run_card.dat")
      os.system("sed -i \'s|###XQCUT| "+xqcut+"   = xqcut|g\'  "+this_name+"/"+this_name+"_run_card.dat")
      os.system("cp skeleton/customizecards.dat "+this_name+"/"+this_name+"_customizecards.dat")
      os.system("sed -i \'s|###N1MASSHERE|set param_card mn1 "+mass[i_mass]+"|g\' "+this_name+"/"+this_name+"_customizecards.dat")
      if(mix[i_mix] == "Mu"):
        os.system("sed -i \'s|###N1MUMIXHERE|set param_card ven1 0.0|g\' "+this_name+"/"+this_name+"_customizecards.dat")
        os.system("sed -i \'s|###N1ELMIXHERE|set param_card vmun1 0.01|g\' "+this_name+"/"+this_name+"_customizecards.dat")
      else:
        os.system("sed -i \'s|###N1MUMIXHERE|set param_card ven1 0.01|g\' "+this_name+"/"+this_name+"_customizecards.dat")
        os.system("sed -i \'s|###N1ELMIXHERE|set param_card vmun1 0.0|g\' "+this_name+"/"+this_name+"_customizecards.dat")

#    print "sed -i \'s|;;;|\\n|g\' "+this_name+"/*.dat"
#    os.system("sed -i \'s|;;;|\n|g\' "+this_name+"/*.dat")


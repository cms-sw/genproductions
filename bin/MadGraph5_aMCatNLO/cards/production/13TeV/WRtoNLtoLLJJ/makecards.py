import os

WRMASSEND = 7000

WRMASS = 200
while (WRMASS <= WRMASSEND):
  NMASS = 100
  while (NMASS < WRMASS):
    if (NMASS == WRMASS): NMASS = WRMASS - 100
    DIRNAME = "WRtoNLtoLLJJ_WR"+(str)(WRMASS)+"_N"+(str)(NMASS)
    os.system("mkdir "+DIRNAME)
    os.system("cp skeletons/run_card.dat "+DIRNAME+"/"+DIRNAME+"_run_card.dat") 
    os.system("cp skeletons/extramodels.dat "+DIRNAME+"/"+DIRNAME+"_extramodels.dat")
    proclines = file("skeletons/proc_card.dat").readlines()
    custolines = file("skeletons/customizecards.dat").readlines()
    procnew = file(DIRNAME+"/"+DIRNAME+"_proc_card.dat", "wt")
    custonew = file(DIRNAME+"/"+DIRNAME+"_customizecards.dat", "wt")
    for line in proclines:
      if "###OUTPUT" in line:
	procnew.write("output "+DIRNAME+" --nojpeg\n")
      else:
        procnew.write(line)
    for line in custolines:
      if "###SETMASS9900012" in line:
        custonew.write("set param_card mass 9900012 "+(str)(NMASS)+"\n")
      elif "###SETMASS9900014" in line:
        custonew.write("set param_card mass 9900014 "+(str)(NMASS)+"\n")
      elif "###SETMASS9900016" in line:
        custonew.write("set param_card mass 9900016 999999\n")
      elif "###SETMASS34" in line:
        custonew.write("set param_card mass 34 "+(str)(WRMASS)+"\n")
      else:
        custonew.write(line)
    if (NMASS == 100): NMASS = NMASS + 100
    elif (NMASS == WRMASS - 200): NMASS = NMASS + 100
    else: NMASS = NMASS + 200

  WRMASS = WRMASS + 200

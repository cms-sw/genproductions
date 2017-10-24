import os

#HNmass = ["15", "20", "30", "40", "50", "60", "65", "70", "75", "80", "85", "90", "95", "100", "110", "150", "200", "300", "400", "500", "600", "700", "800", "900", "1000", "1100", "1200", "1300", "1400", "1500"]
#Decay = ["MuMu", "MuE", "EMu", "EE"]

HNmass = ["50", "400"]
Decay = ["MuE", "EMu"]

skel_customizecards = open("./skeleton_Schannel/Schannel_customizecards.dat").readlines()
skel_madspin_card = open("./skeleton_Schannel/Schannel_madspin_card.dat").readlines()
skel_proc_card = open("./skeleton_Schannel/Schannel_proc_card.dat").readlines()

os.mkdir("HeavyNeutrinoToDilepton_Schannel_NLO")

for i in range(0,2):
  for ii in range(0,2):
    os.mkdir("HeavyNeutrinoToDilepton_Schannel_NLO/HeavyNeutrinoTo"+Decay[ii]+"_Schannel_M"+HNmass[i]+"_NLO")
    out_customizecards = open("HeavyNeutrinoTo"+Decay[ii]+"_Schannel_M"+HNmass[i]+"_NLO_customizecards.dat", "wt")
    for line in skel_customizecards:
      if "MASS_HERE" in line:
        out_customizecards.write("set param_card mn1 "+HNmass[i]+"\n")
      elif "MIXING_HERE" in line:
        if( Decay[ii] == "MuMu"):
          out_customizecards.write("set param_card vmun1 0.1\nset param_card ven1 0.\n")
        elif( Decay[ii] == "EE"):
          out_customizecards.write("set param_card vmun1 0.\nset param_card ven1 0.1\n")
        else:
          out_customizecards.write("set param_card vmun1 0.1\nset param_card ven1 0.1\n")
      else:
        out_customizecards.write(line)
    out_customizecards.close()

    out_madspin_card = open("HeavyNeutrinoTo"+Decay[ii]+"_Schannel_M"+HNmass[i]+"_NLO_madspin_card.dat", "wt")
    for line in skel_madspin_card:
      if "SPINMODE_HERE" in line:
        if( int(HNmass[i]) < 80.4 ):
          out_madspin_card.write("set spinmode onshell\n")
        else:
          out_madspin_card.write("#\n")
      elif "DECAY_HERE" in line:
        if( int(HNmass[i]) < 80.4 ):
          if( Decay[ii] == "MuMu") or ( Decay[ii] == "EMu"):
            out_madspin_card.write("decay n1 > mu j j\n")
          else:
            out_madspin_card.write("decay n1 > e j j\n")
        else:
          if( Decay[ii] == "MuMu") or ( Decay[ii] == "EMu"):
            out_madspin_card.write("decay n1 > mu w, w > j j\n")
          else:
            out_madspin_card.write("decay n1 > e w, w > j j\n")
      else:
        out_madspin_card.write(line)
    out_madspin_card.close()

    out_proc_card = open("HeavyNeutrinoTo"+Decay[ii]+"_Schannel_M"+HNmass[i]+"_NLO_proc_card.dat", "wt")
    for line in skel_proc_card:
      if "PROCESS_HERE" in line:
        if( Decay[ii] == "MuMu") or ( Decay[ii] == "MuE"):
          out_proc_card.write("generate p p > n1 mu [QCD]\n")
        else:
          out_proc_card.write("generate p p > n1 e [QCD]\n")
      elif "OUTPUT_HERE" in line:
        out_proc_card.write("output HeavyNeutrinoTo"+Decay[ii]+"_Schannel_M"+HNmass[i]+"_NLO -nojpeg\n")
      else:
        out_proc_card.write(line)
    out_proc_card.close()

for i in range(0,2):
  for ii in range(0,2):
    os.system("mv HeavyNeutrinoTo"+Decay[ii]+"_Schannel_M"+HNmass[i]+"_NLO_customizecards.dat HeavyNeutrinoToDilepton_Schannel_NLO/HeavyNeutrinoTo"+Decay[ii]+"_Schannel_M"+HNmass[i]+"_NLO/")
    os.system("mv HeavyNeutrinoTo"+Decay[ii]+"_Schannel_M"+HNmass[i]+"_NLO_madspin_card.dat HeavyNeutrinoToDilepton_Schannel_NLO/HeavyNeutrinoTo"+Decay[ii]+"_Schannel_M"+HNmass[i]+"_NLO/")
    os.system("mv HeavyNeutrinoTo"+Decay[ii]+"_Schannel_M"+HNmass[i]+"_NLO_proc_card.dat HeavyNeutrinoToDilepton_Schannel_NLO/HeavyNeutrinoTo"+Decay[ii]+"_Schannel_M"+HNmass[i]+"_NLO/")
    os.system("cp ./skeleton_Schannel/Schannel_extramodels.dat HeavyNeutrinoToDilepton_Schannel_NLO/HeavyNeutrinoTo"+Decay[ii]+"_Schannel_M"+HNmass[i]+"_NLO/HeavyNeutrinoTo"+Decay[ii]+"_Schannel_M"+HNmass[i]+"_NLO_extramodels.dat")
    os.system("cp ./skeleton_Schannel/Schannel_run_card.dat HeavyNeutrinoToDilepton_Schannel_NLO/HeavyNeutrinoTo"+Decay[ii]+"_Schannel_M"+HNmass[i]+"_NLO/HeavyNeutrinoTo"+Decay[ii]+"_Schannel_M"+HNmass[i]+"_NLO_run_card.dat")


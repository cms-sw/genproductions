import os
ZRMASSES = ["400", "600", "800", "1000", "1200", "1400", "1600", "1800", "2000", "2200", "2400", "2600", "2800", "3000", "3200", "3400", "3600", "3800", "4000", "4200", "4400", "4600", "4800", "5000"]
NMASSES  = ["100", "200", "300", "400", "500", "600", "700", "800", "900", "1000", "1100", "1200", "1300", "1400", "1500", "1600", "1700", "1800", "1900", "2000", "2100", "2200", "2300", "2400"]

custo = open("./skeleton_mumu/skeleton_customizecards.dat").readlines()
proc = open("./skeleton_mumu/skeleton_proc_card.dat").readlines()

for ZRMASS in ZRMASSES:
  for NMASS in NMASSES:
    if (((float)(ZRMASS)/2. - 1) < ((float)(NMASS))):
      continue
    os.system("mkdir ZprimetoNN_MuMuJJJJ_Zprime"+ZRMASS+"_N"+NMASS+"_WR5000_NLO")

    os.system("cp ./skeleton_mumu/skeleton_madspin_card.dat ZprimetoNN_MuMuJJJJ_Zprime"+ZRMASS+"_N"+NMASS+"_WR5000_NLO/ZprimetoNN_MuMuJJJJ_Zprime"+ZRMASS+"_N"+NMASS+"_WR5000_NLO_madspin_card.dat")
    os.system("cp ./skeleton_mumu/skeleton_run_card.dat ZprimetoNN_MuMuJJJJ_Zprime"+ZRMASS+"_N"+NMASS+"_WR5000_NLO/ZprimetoNN_MuMuJJJJ_Zprime"+ZRMASS+"_N"+NMASS+"_WR5000_NLO_run_card.dat")
    os.system("cp ./skeleton_mumu/skeleton_extramodels.dat ZprimetoNN_MuMuJJJJ_Zprime"+ZRMASS+"_N"+NMASS+"_WR5000_NLO/ZprimetoNN_MuMuJJJJ_Zprime"+ZRMASS+"_N"+NMASS+"_WR5000_NLO_extramodels.dat")


    custo_cards = open("ZprimetoNN_MuMuJJJJ_Zprime"+ZRMASS+"_N"+NMASS+"_WR5000_NLO_customizecards.dat", "wt")
    for line in custo:
      if "N1MASS_HERE" in line:
        custo_cards.write("set param_card mn1 1.0e+12\n")
      elif "N2MASS_HERE" in line:
        custo_cards.write("set param_card mn2 "+NMASS+"\n")
      elif "N3MASS_HERE" in line:
        custo_cards.write("set param_card mn3 1.0e+12\n")
      elif "ZRMASS_HERE" in line:
	custo_cards.write("set param_card mzr "+ZRMASS+"\n")
      elif "WRMASS_HERE" in line:
	custo_cards.write("set param_card mwr 5000\n")
      elif "ZRMASSHERE" in line:
        custo_cards.write("set param_card mzr "+ZRMASS+"\n")
      else:
        custo_cards.write(line)

    proc_cards = open("ZprimetoNN_MuMuJJJJ_Zprime"+ZRMASS+"_N"+NMASS+"_WR5000_NLO_proc_card.dat", "wt")
    for line in proc:
      if "OUTPUT_HERE" in line:
	proc_cards.write("output ZprimetoNN_MuMuJJJJ_Zprime"+ZRMASS+"_N"+NMASS+"_WR5000_NLO")
      else:
	proc_cards.write(line)

    os.system("mv ZprimetoNN_MuMuJJJJ_Zprime"+ZRMASS+"_N"+NMASS+"_WR5000_NLO*dat ZprimetoNN_MuMuJJJJ_Zprime"+ZRMASS+"_N"+NMASS+"_WR5000_NLO/")

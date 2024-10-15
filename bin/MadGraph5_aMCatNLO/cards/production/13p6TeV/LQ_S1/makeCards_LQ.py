# EXAMPLES: python3 makeCards_LQ.py tau

import os
import sys


lflav  = sys.argv[1]
# Options: ["ele", "mu", "tau", "eletau", "mutau"]


if lflav != "ele" and lflav != "mu" and lflav != "tau" and lflav != "eletau" and lflav != "mutau":
    print("Invalid lepton coupling to LQ. Exiting.")
    exit(0)


custoTemplate = open("./S1LQ_customizecards.dat")
custoCard = custoTemplate.read()
custoTemplate.close()
modelTemplate = open("./S1LQ_extramodels.dat")
modelCard = modelTemplate.read()
modelTemplate.close()
runTemplate = open("./S1LQ_run_card.dat")
runCard = runTemplate.read()
runTemplate.close()

mass_list = ['500','750','1000','1250','1500','1750','2000','2250','2500','2750','3000']
yuk_list = ['0pt01']
chrl_list = ['L','R']
fs_list = ['TLTL','TLQ5L','TLQ5N']


for fs in fs_list:

    procTemplate = open("./S1LQ_"+fs+"_proc_card.dat")
    procCard = procTemplate.read()
    procTemplate.close()

    for chrl in chrl_list:

        if fs != 'TLTL' and chrl == 'R':
            continue

        if lflav == "ele":
            restrictInfo = "y"+chrl+chrl+"3x1"
            nevents = "25000"
        elif lflav == "mu":
            restrictInfo = "y"+chrl+chrl+"3x2"
            nevents = "25000"
        elif lflav == "tau":
            restrictInfo = "y"+chrl+chrl+"3x3"
            nevents = "50000"
        elif lflav == "eletau":
            restrictInfo = "y"+chrl+chrl+"3x3"+"y"+chrl+chrl+"3x1"
            nevents = "100000"
        elif lflav == "mutau":
            restrictInfo = "y"+chrl+chrl+"3x3"+"y"+chrl+chrl+"3x2"
            nevents = "100000"

        for mass in mass_list:

            for yuk in yuk_list:

                yukdec = yuk.replace("pt",".")

                print (f"Generating cards for Final State = {fs}, Chirality = {chrl}, Mass = {mass}, Yuk = {yukdec}")
                prefix = "S1LQ_"+fs+"_"+lflav+"_M-"+mass+"_yuk"+chrl+"-"+yuk                                               #Example: "S1LQ_TLTL_ele_M-500_yukL-0pt01"
                os.makedirs("./LQ_Cards/"+fs+"/"+lflav+"/"+chrl+"/"+prefix, exist_ok=True)                                 #Example: "./LQ_Cards/TLTL/ele/L/S1LQ_TLTL_ele_M-500_yukL-0pt01/"

                newprocCard = open("./LQ_Cards/"+fs+"/"+lflav+"/"+chrl+"/"+prefix+"/"+prefix+"_proc_card.dat",'w+')        #Example: "./LQ_Cards/TLTL/ele/L/S1LQ_TLTL_ele_M-500_yukL-0pt01/S1LQ_TLTL_ele_M-500_yukL-0pt01_proc_card.dat"
                newprocCard_1 = procCard.replace("LQYUK",restrictInfo).replace("YUKVALUE",yuk).replace("LEPF",lflav).replace("NUF","nu"+lflav).replace("LQOUTPUT",prefix)
                newprocCard.write(newprocCard_1)
                newprocCard.close()
            
                newcustoCard = open("./LQ_Cards/"+fs+"/"+lflav+"/"+chrl+"/"+prefix+"/"+prefix+"_customizecards.dat",'w+')  #Example: "./LQ_Cards/TLTL/ele/L/S1LQ_TLTL_ele_M-500_yukL-0pt01/S1LQ_TLTL_ele_M-500_yukL-0pt01_customizecards.dat"
                newcustoCard_1 = custoCard.replace("LQMASS",mass)
                newcustoCard.write(newcustoCard_1)
                newcustoCard.close()
            
                newmodelCard = open("./LQ_Cards/"+fs+"/"+lflav+"/"+chrl+"/"+prefix+"/"+prefix+"_extramodels.dat",'w+')     #Example: "./LQ_Cards/TLTL/ele/L/S1LQ_TLTL_ele_M-500_yukL-0pt01/S1LQ_TLTL_ele_M-500_yukL-0pt01_extramodels.dat"
                newmodelCard_1 = modelCard
                newmodelCard.write(newmodelCard_1)
                newmodelCard.close()
            
                newrunCard = open("./LQ_Cards/"+fs+"/"+lflav+"/"+chrl+"/"+prefix+"/"+prefix+"_run_card.dat",'w+')          #Example: "./LQ_Cards/TLTL/ele/L/S1LQ_TLTL_ele_M-500_yukL-0pt01/S1LQ_TLTL_ele_M-500_yukL-0pt01_run_card.dat"
                newrunCard_1 = runCard.replace("N_EVENTS",nevents)
                newrunCard.write(newrunCard_1)
                newrunCard.close()
        
exit(0)

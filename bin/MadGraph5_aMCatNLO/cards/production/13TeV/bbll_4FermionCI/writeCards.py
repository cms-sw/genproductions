import os

massRanges = [["200", "500"]] # [["200", "-1"]]
xqCuts = ["30"] # ["10", "20", "30"]
interTerms = ["const"]  # ["const", "dest"]
helicityCombs = ["LL"] # ["LL", "RR", "LR", "RL", "ALL"]
lambdaSs = ["1000"] # ["1000", "4000", "8000", "10000", "-1"] 

for XQCUT in xqCuts:

  for INTERTERM in interTerms:

    if (INTERTERM == "const"): coupling = "1.256637e+01"
    elif (INTERTERM == "dest"): coupling = "-1.256637e+01"

    for HELICITYCOMB in helicityCombs:

      for LAMBDAS in lambdaSs:

        for massRange in massRanges:


          MMLL = massRange[0]
          MMLLMAX = massRange[1]
          MASSRANGE = "M-{0}To{1}".format(MMLL,MMLLMAX.replace("-1","Inf"))
          LAMBDAS = LAMBDAS.replace("-1","10000000000")
          LAMBDA = LAMBDAS.replace("10000000000","Inf")
          if LAMBDA!="Inf": LAMBDA = str(int(int(LAMBDA)/1000)).replace("000","")+"TeV"

          datasetName =  "bbll_4FermionCI_{0}_Lambda-{1}_{3}{2}_XQCUT-{4}".format(MASSRANGE, LAMBDA, HELICITYCOMB, INTERTERM, XQCUT)

#          print datasetName
          os.system("mkdir -p {0}".format(datasetName))

          os.system("cp skeleton/proc_card.dat {0}/{1}_proc_card.dat".format(datasetName,datasetName))

          os.system("cp skeleton/run_card.dat {0}/{1}_run_card.dat".format(datasetName,datasetName))
          os.system("cp skeleton/customizecards.dat {0}/{1}_customizecards.dat".format(datasetName,datasetName))

          os.system("sed -i 's|###OUTPUT###|{0}|g' {1}/{2}_proc_card.dat".format(datasetName,datasetName,datasetName))

          os.system("sed -i 's|###XQCUT###|{0}|g' {1}/{2}_run_card.dat".format(XQCUT,datasetName,datasetName))
          os.system("sed -i 's|###MMLL###|{0}|g' {1}/{2}_run_card.dat".format(MMLL,datasetName,datasetName))
          os.system("sed -i 's|###MMLLMAX###|{0}|g' {1}/{2}_run_card.dat".format(MMLLMAX,datasetName,datasetName))

          os.system("sed -i 's|###LAMBDAS###|{0}|g' {1}/{2}_customizecards.dat".format(LAMBDAS,datasetName,datasetName))
          for writeHel in ["LL", "LR", "RL", "RR"]:
            if not (HELICITYCOMB == "ALL"):
              if (HELICITYCOMB == writeHel):
                os.system("sed -i 's|###CMUBB{0}###|{1}|g' {2}/{3}_customizecards.dat".format(writeHel,coupling,datasetName,datasetName))
                os.system("sed -i 's|###CELBB{0}###|{1}|g' {2}/{3}_customizecards.dat".format(writeHel,coupling,datasetName,datasetName))
              else:
                os.system("sed -i 's|###CMUBB{0}###|{1}|g' {2}/{3}_customizecards.dat".format(writeHel,0.0,datasetName,datasetName))
                os.system("sed -i 's|###CELBB{0}###|{1}|g' {2}/{3}_customizecards.dat".format(writeHel,0.0,datasetName,datasetName))

            else:
              os.system("sed -i 's|###CMUBB{0}###|{1}|g' {2}/{3}_customizecards.dat".format(writeHel,coupling,datasetName,datasetName))
              os.system("sed -i 's|###CELBB{0}###|{1}|g' {2}/{3}_customizecards.dat".format(writeHel,coupling,datasetName,datasetName))

          print "./gridpack_generation.sh {0} cards/shjeon/bbll/{1}".format(datasetName, datasetName)

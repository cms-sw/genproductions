import os

basedir = os.getcwd() + "/"
basename = "DYTypeI_NLO"
basename_tmp = "skeletons_DYTypeI_NLO/DYTypeI_NLO"

os.system("mkdir -p " + basedir + basename)

#masses = [70, 75, 85, 90, 95, 100, 125, 150, 200, 250, 300, 400, 500, 600, 700, 800, 900, 1000, 1100, 1200, 1300, 1400, 1500, 1700, 2000]
#channels = ["EMu", "EE", "MuMu", "MuE"]
masses = [300, 70]
channels = ["MuE"]

mixing_EMu = {'ven1' : 1.000000e-02, 'vmun1' : 1.000000e-02}
mixing_MuE = {'ven1' : 1.000000e-02, 'vmun1' : 1.000000e-02}
mixing_EE = {'ven1' : 1.000000e-02, 'vmun1' : 0.000000e+00}
mixing_MuMu = {'ven1' : 0.000000e+00, 'vmun1' : 1.000000e-02}

if 'DYTypeI' in basename:
  with open("skeletons_DYTypeI_NLO/DYTypeI_templateNLO_customizecards.dat", "r") as f1:
    customizecards = f1.read()
  with open("skeletons_DYTypeI_NLO/DYTypeI_templateNLO_proc_card.dat", "r") as g1:
    proc_card = g1.read()
  with open("skeletons_DYTypeI_NLO/DYTypeI_templateNLO_extramodels.dat", "r") as h1:
    extramodels = h1.read()
  with open("skeletons_DYTypeI_NLO/DYTypeI_templateNLO_run_card.dat", "r") as i1:
    run_card = i1.read()
  with open("skeletons_DYTypeI_NLO/DYTypeI_templateNLO_madspin_card.dat", "r") as j1:
    madspin_card = j1.read()

elif 'VBFTypeI' in basename:
  with open("skeletons_VBFTypeI_NLO/VBFTypeI_templateNLO_customizecards.dat", "r") as f1:
    customizecards = f1.read()
  with open("skeletons_VBFTypeI_NLO/VBFTypeI_templateNLO_proc_card.dat", "r") as g1:
    proc_card = g1.read()
  with open("skeletons_VBFTypeI_NLO/VBFTypeI_templateNLO_extramodels.dat", "r") as h1:
    extramodels = h1.read()
  with open("skeletons_VBFTypeI_NLO/VBFTypeI_templateNLO_run_card.dat", "r") as i1:
    run_card = i1.read()
  with open("skeletons_VBFTypeI_NLO/VBFTypeI_templateNLO_madspin_card.dat", "r") as j1:
    madspin_card = j1.read()


for mass in masses:

  for channel in channels:
    
    output = "output " + basename + "_" + channel + "_M" + str(mass) + " --nojpeg"

    os.mkdir(basedir + basename + "/" + basename + "_{0}_M{1}".format(channel, mass))

    if mass < 80: 

      if 'DYTypeI' in basename:
        process = ["generate p p > e n1 [QCD]", "generate p p > mu n1 [QCD]", "generate p p > e n1 [QCD]", "generate p p > mu n1 [QCD]"]
      if 'VBFTypeI' in basename:
        process = ["generate p a > n1 e j [QCD]\nadd process a p > n1 e j [QCD]", "generate p a > n1 mu j [QCD]\nadd process a p > n1 mu j [QCD]", "generate p a > n1 e j [QCD]\nadd process a p > n1 e j [QCD]", "generate p a > n1 mu j [QCD]\nadd process a p > n1 mu j [QCD]"]
      
      decay = ["set spinmode none\n\ndecay n1 > mu j j", "set spinmode none\n\ndecay n1 > e j j", "set spinmode none\n\ndecay n1 > e j j", "set spinmode none\n\ndecay n1 > mu j j"]

      if channel == "EMu":

        with open(basename_tmp + "_{0}_M{1}_customizecards.dat".format(channel, mass), "w") as f2:
          f2.write(customizecards.format(mass=mass, ven1=mixing_EMu['ven1'], vmun1=mixing_EMu['vmun1']))

        with open(basename_tmp + "_{0}_M{1}_proc_card.dat".format(channel, mass), "w") as g2:
          g2.write(proc_card.format(process=process[0], output=output))

        with open(basename_tmp + "_{0}_M{1}_extramodels.dat".format(channel, mass), "w") as h2:
          h2.write(extramodels)

        with open(basename_tmp + "_{0}_M{1}_run_card.dat".format(channel, mass), "w") as i2:
          i2.write(run_card)

        with open(basename_tmp + "_{0}_M{1}_madspin_card.dat".format(channel, mass), "w") as j2:
          j2.write(madspin_card.format(decay=decay[0]))

      if channel == "MuE":

        with open(basename_tmp + "_{0}_M{1}_customizecards.dat".format(channel, mass), "w") as f3:
          f3.write(customizecards.format(mass=mass, ven1=mixing_MuE['ven1'], vmun1=mixing_MuE['vmun1']))

        with open(basename_tmp + "_{0}_M{1}_proc_card.dat".format(channel, mass), "w") as g3:
          g3.write(proc_card.format(process=process[1], output=output))

        with open(basename_tmp + "_{0}_M{1}_extramodels.dat".format(channel, mass), "w") as h3:
          h3.write(extramodels)

        with open(basename_tmp + "_{0}_M{1}_run_card.dat".format(channel, mass), "w") as i3:
          i3.write(run_card)

        with open(basename_tmp + "_{0}_M{1}_madspin_card.dat".format(channel, mass), "w") as j3:
          j3.write(madspin_card.format(decay=decay[1]))

      elif channel == "EE":

        with open(basename_tmp + "_{0}_M{1}_customizecards.dat".format(channel, mass), "w") as f4:
          f4.write(customizecards.format(mass=mass, ven1=mixing_EE['ven1'], vmun1=mixing_EE['vmun1']))

        with open(basename_tmp + "_{0}_M{1}_proc_card.dat".format(channel, mass), "w") as g4:
          g4.write(proc_card.format(process=process[2], output=output))

        with open(basename_tmp + "_{0}_M{1}_extramodels.dat".format(channel, mass), "w") as h4:
          h4.write(extramodels)

        with open(basename_tmp + "_{0}_M{1}_run_card.dat".format(channel, mass), "w") as i4:
          i4.write(run_card)

        with open(basename_tmp + "_{0}_M{1}_madspin_card.dat".format(channel, mass), "w") as j4:
          j4.write(madspin_card.format(decay=decay[2]))

      elif channel == "MuMu":

        with open(basename_tmp + "_{0}_M{1}_customizecards.dat".format(channel, mass), "w") as f5:
          f5.write(customizecards.format(mass=mass, ven1=mixing_MuMu['ven1'], vmun1=mixing_MuMu['vmun1']))

        with open(basename_tmp + "_{0}_M{1}_proc_card.dat".format(channel, mass), "w") as g5:
          g5.write(proc_card.format(process=process[3], output=output))

        with open(basename_tmp + "_{0}_M{1}_extramodels.dat".format(channel, mass), "w") as h5:
          h5.write(extramodels)

        with open(basename_tmp + "_{0}_M{1}_run_card.dat".format(channel, mass), "w") as i5:
          i5.write(run_card)

        with open(basename_tmp + "_{0}_M{1}_madspin_card.dat".format(channel, mass), "w") as j5:
          j5.write(madspin_card.format(decay=decay[3]))



      os.system("mv " + basedir + basename_tmp + "_{0}_M{1}_customizecards.dat ".format(channel, mass) + basedir + basename + "/" + basename + "_{0}_M{1}".format(channel, mass))
      os.system("mv " + basedir + basename_tmp + "_{0}_M{1}_proc_card.dat ".format(channel, mass) + basedir + basename + "/" + basename + "_{0}_M{1}".format(channel, mass))
      os.system("mv " + basedir + basename_tmp + "_{0}_M{1}_extramodels.dat ".format(channel, mass) + basedir + basename + "/" + basename + "_{0}_M{1}".format(channel, mass))
      os.system("mv " + basedir + basename_tmp + "_{0}_M{1}_run_card.dat ".format(channel, mass) + basedir + basename + "/" + basename + "_{0}_M{1}".format(channel, mass))
      os.system("mv " + basedir + basename_tmp + "_{0}_M{1}_madspin_card.dat ".format(channel, mass) + basedir + basename + "/" + basename + "_{0}_M{1}".format(channel, mass))



    elif mass >= 80:
      
      if 'DYTypeI' in basename:
        process = ["generate p p > e n1 [QCD]", "generate p p > mu n1 [QCD]", "generate p p > e n1 [QCD]", "generate p p > mu n1 [QCD]"]
      if 'VBFTypeI' in basename:
        process = ["generate p a > n1 e j [QCD]\nadd process a p > n1 e j [QCD]", "generate p a > n1 mu j [QCD]\nadd process a p > n1 mu j [QCD]", "generate p a > n1 e j [QCD]\nadd process a p > n1 e j [QCD]", "generate p a > n1 mu j [QCD]\nadd process a p > n1 mu j [QCD]"]
      decay = ["decay n1 > w mu, w > j j", "decay n1 > w e, w > j j", "decay n1 > w e, w > j j", "decay n1 > w mu, w > j j"]

      if channel == "EMu":

        with open(basename_tmp + "_{0}_M{1}_customizecards.dat".format(channel, mass), "w") as f2:
          f2.write(customizecards.format(mass=mass, ven1=mixing_EMu['ven1'], vmun1=mixing_EMu['vmun1']))

        with open(basename_tmp + "_{0}_M{1}_proc_card.dat".format(channel, mass), "w") as g2:
          g2.write(proc_card.format(process=process[0], output=output))

        with open(basename_tmp + "_{0}_M{1}_extramodels.dat".format(channel, mass), "w") as h2:
          h2.write(extramodels)

        with open(basename_tmp + "_{0}_M{1}_run_card.dat".format(channel, mass), "w") as i2:
          i2.write(run_card)

        with open(basename_tmp + "_{0}_M{1}_madspin_card.dat".format(channel, mass), "w") as j2:
          j2.write(madspin_card.format(decay=decay[0]))

      if channel == "MuE":

        with open(basename_tmp + "_{0}_M{1}_customizecards.dat".format(channel, mass), "w") as f3:
          f3.write(customizecards.format(mass=mass, ven1=mixing_MuE['ven1'], vmun1=mixing_MuE['vmun1']))

        with open(basename_tmp + "_{0}_M{1}_proc_card.dat".format(channel, mass), "w") as g3:
          g3.write(proc_card.format(process=process[1], output=output))

        with open(basename_tmp + "_{0}_M{1}_extramodels.dat".format(channel, mass), "w") as h3:
          h3.write(extramodels)

        with open(basename_tmp + "_{0}_M{1}_run_card.dat".format(channel, mass), "w") as i3:
          i3.write(run_card)

        with open(basename_tmp + "_{0}_M{1}_madspin_card.dat".format(channel, mass), "w") as j3:
          j3.write(madspin_card.format(decay=decay[1]))

      elif channel == "EE":

        with open(basename_tmp + "_{0}_M{1}_customizecards.dat".format(channel, mass), "w") as f4:
          f4.write(customizecards.format(mass=mass, ven1=mixing_EE['ven1'], vmun1=mixing_EE['vmun1']))

        with open(basename_tmp + "_{0}_M{1}_proc_card.dat".format(channel, mass), "w") as g4:
          g4.write(proc_card.format(process=process[2], output=output))

        with open(basename_tmp + "_{0}_M{1}_extramodels.dat".format(channel, mass), "w") as h4:
          h4.write(extramodels)

        with open(basename_tmp + "_{0}_M{1}_run_card.dat".format(channel, mass), "w") as i4:
          i4.write(run_card)

        with open(basename_tmp + "_{0}_M{1}_madspin_card.dat".format(channel, mass), "w") as j4:
          j4.write(madspin_card.format(decay=decay[2]))

      elif channel == "MuMu":

        with open(basename_tmp + "_{0}_M{1}_customizecards.dat".format(channel, mass), "w") as f5:
          f5.write(customizecards.format(mass=mass, ven1=mixing_MuMu['ven1'], vmun1=mixing_MuMu['vmun1']))

        with open(basename_tmp + "_{0}_M{1}_proc_card.dat".format(channel, mass), "w") as g5:
          g5.write(proc_card.format(process=process[3], output=output))

        with open(basename_tmp + "_{0}_M{1}_extramodels.dat".format(channel, mass), "w") as h5:
          h5.write(extramodels)

        with open(basename_tmp + "_{0}_M{1}_run_card.dat".format(channel, mass), "w") as i5:
          i5.write(run_card)

        with open(basename_tmp + "_{0}_M{1}_madspin_card.dat".format(channel, mass), "w") as j5:
          j5.write(madspin_card.format(decay=decay[3]))



      os.system("mv " + basedir + basename_tmp + "_{0}_M{1}_customizecards.dat ".format(channel, mass) + basedir + basename + "/" + basename + "_{0}_M{1}".format(channel, mass))
      os.system("mv " + basedir + basename_tmp + "_{0}_M{1}_proc_card.dat ".format(channel, mass) + basedir + basename + "/" + basename + "_{0}_M{1}".format(channel, mass))
      os.system("mv " + basedir + basename_tmp + "_{0}_M{1}_extramodels.dat ".format(channel, mass) + basedir + basename + "/" + basename + "_{0}_M{1}".format(channel, mass))
      os.system("mv " + basedir + basename_tmp + "_{0}_M{1}_run_card.dat ".format(channel, mass) + basedir + basename + "/" + basename + "_{0}_M{1}".format(channel, mass))
      os.system("mv " + basedir + basename_tmp + "_{0}_M{1}_madspin_card.dat ".format(channel, mass) + basedir + basename + "/" + basename + "_{0}_M{1}".format(channel, mass))



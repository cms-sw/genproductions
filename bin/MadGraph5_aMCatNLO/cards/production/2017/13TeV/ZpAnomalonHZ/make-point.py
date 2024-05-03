def makeMassPoint(zpMass, ndMass, nsMass):
  from os import mkdir
  from os.path import join
  from shutil import copyfile

  templDir = "ZpAnomalonHZ_UFO-Zp3000-ND800-NS200"
  outDir = "ZpAnomalonHZ_UFO-Zp%i-ND%i-NS%i" % (zpMass, ndMass, nsMass)
  mkdir(outDir)

  templates = [join(templDir, card) for card in  [
                                                   "ZpAnomalonHZ_UFO-Zp3000-ND800-NS200_extramodels.dat" ,
                                                   "ZpAnomalonHZ_UFO-Zp3000-ND800-NS200_run_card.dat"
                                                 ]
              ]
  for template in templates:
    copyfile(template, template.replace("Zp3000-ND800-NS200", "Zp%i-ND%i-NS%i"%(zpMass, ndMass, nsMass)))
  with open(join(outDir, "ZpAnomalonHZ_UFO-Zp%i-ND%i-NS%i_customizecards.dat" % (zpMass, ndMass, nsMass)), "w") as cc:
    cc.write("set param_card MZp %i\n" % zpMass)
    cc.write("set param_card WZp %0.3f\n" % (zpMass/float(1e4)))
    cc.write("set param_card MND %i\n" % ndMass)
    cc.write("set param_card WND %0.3f\n" % (ndMass/float(1e4)))
    cc.write("set param_card MNS %i" % nsMass)
  with open(join(templDir, "ZpAnomalonHZ_UFO-Zp3000-ND800-NS200_proc_card.dat"), "r") as procTempl:
    with open(join(outDir, "ZpAnomalonHZ_UFO-Zp%i-ND%i-NS%i_proc_card.dat" % (zpMass, ndMass, nsMass)), "w") as procOut:
      for line in procTempl:
        if "ZpAnomalonHZ_UFO-Zp3000-ND800-NS200" in line:
          line = line.replace("ZpAnomalonHZ_UFO-Zp3000-ND800-NS200", "ZpAnomalonHZ_UFO-Zp%i-ND%i-NS%i" % (zpMass, ndMass, nsMass))
        procOut.write(line)

if __name__ == "__main__":
  from sys import argv

  if len(argv) != 4:
    print "please run this script with three arguments, the Zp mass, the ND mass, and the NS mass"
    exit(1)

  zpMass = int(argv[1])
  ndMass = int(argv[2])
  nsMass = int(argv[3])
  makeMassPoint(zpMass, ndMass, nsMass)

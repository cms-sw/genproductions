def makeMassPoint(mass):
  from os import mkdir
  from os.path import join
  from shutil import copyfile

  templDir = "Zprime_Gh_M1200"
  outDir = "Zprime_Gh_M%i" % mass
  mkdir(outDir)

  templates = [join(templDir, card) for card in  [
                                                   "ZpHgamma_UFO-M1200_extramodels.dat" ,
                                                   "ZpHgamma_UFO-M1200_run_card.dat"
                                                 ]
              ]
  for template in templates:
    copyfile(template, template.replace("1200", str(mass)))
  with open(join(outDir, "ZpHgamma_UFO-M%i_customizecards.dat" % mass), "w") as cc:
    cc.write("set param_card MZp %i\n" % mass)
    cc.write("set param_card WZp %0.3f" % (mass/float(1e4)))
  with open(join(templDir, "ZpHgamma_UFO-M1200_proc_card.dat"), "r") as procTempl:
    with open(join(outDir, "ZpHgamma_UFO-M%i_proc_card.dat" % mass), "w") as procOut:
      for line in procTempl:
        if "ZpHgamma_UFO-M1200" in line:
          line = line.replace("ZpHgamma_UFO-M1200", "ZpHgamma_UFO-M%i" % mass)
        procOut.write(line)

if __name__ == "__main__":
  from sys import argv
  
  if len(argv) != 2:
    print "please run this script with one argument, the mass of the Zp->Gh sample to make"
    exit(1)

  mass = int(argv[1])
  makeMassPoint(mass)

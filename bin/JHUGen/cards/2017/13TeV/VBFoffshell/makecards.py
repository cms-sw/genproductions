#!/usr/bin/env python

hmass = 125
hwidth = 0.00407

def makecard(signalbkgbsi, coupling, finalstate):
  parameters = []

  parameters.append("Process={:d}".format({
    "signal": 66,
    "bkg": 67,
    "BSI": 68,
    "BSI10": 68,
  }[signalbkgbsi]))

  parameters += {
    "ZZ4l_withtaus": ["DecayMode1=8", "DecayMode2=8"],
    "ZZ2l2nu_withtaus": ["DecayMode1=8", "DecayMode2=3"],
  }[finalstate]

  if signalbkgbsi == "bkg":
    if coupling is not None:
      raise ValueError("coupling has to be None for bkg")
  else:
    widthmultiplier = {
      "signal": 1,
      "BSI": 1,
      "BSI10": 10,
    }[signalbkgbsi]
    parameters.append("MReso={}".format(hmass))
    parameters.append("GaReso={}".format(hwidth * widthmultiplier))

    SMcoupling = {
      "SM": "ghz1=1,0",
      "a2": None,
      "a2mix": "ghz1=1,0",
      "a3": None,
      "a3mix": "ghz1=1,0",
      "L1": None,
      "L1mix": "ghz1=1,0",
    }[coupling]

    BSMcoupling = {
      "SM": None,
      "a2": "ghz2=0.207049,0",
      "a3": "ghz4=0.216499,0",
      "L1": "ghz1_prime2=-1549.165,0",
    }[coupling.replace("mix", "")]

    if SMcoupling is not None: parameters.append(SMcoupling)
    if BSMcoupling is not None: parameters.append(BSMcoupling)

  parameters += [
    "VegasNc0=5000000",
    "LHAPDF=NNPDF30_lo_as_0130/NNPDF30_lo_as_0130.info",
    "ReweightInterf=1",

    "mJJcut=30",
    "MPhotonCutoff=4",
    "deltaRcut=0.3",
    "pTjetcut=15",
    "m4l_min=70",
    "m4l_max=13000",
    "detajetcut=0",
    "JetsOppositeEta=0",
    "etajetcut=6.5",

    "FacScheme=1",
    "MuFacMultiplier=1",
    "RenScheme=1",
    "MuRenMultiplier=1",
  ]

  outputfile = signalbkgbsi + finalstate
  if coupling is not None:
    outputfile += "_" + coupling
  outputfile += ".input"

  with open(outputfile, "w") as f:
    f.write(" ".join(parameters)+"\n")

if __name__ == "__main__":
  for finalstate in "ZZ4l_withtaus", "ZZ2l2nu_withtaus":
    makecard("bkg", None, finalstate)
    for signalbkgbsi in "signal", "BSI", "BSI10":
      for coupling in "SM", "a2", "a2mix", "a3", "a3mix", "L1", "L1mix":
        makecard(signalbkgbsi, coupling, finalstate)

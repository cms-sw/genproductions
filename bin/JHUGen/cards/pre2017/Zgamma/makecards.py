card = "Process={spin} PChannel={PChannel} VegasNc0=1000000 MReso={mass} GaReso={width} DecayMode1={DecayMode1} DecayMode2={DecayMode2} {couplings} LHAPDF=NNPDF30_lo_as_0130/NNPDF30_lo_as_0130.info ReadCSmax"
cardname = "{spin_forname}{prod}To{decaymode}To{decaymode_final}_{width_forname}_M-{mass}_13TeV-JHUgenV6.input"

masses = [300, 350, 400, 500, 600, 750, 900, 1200, 1500, 2000, 3000, 4000, 5000]
widthfractions_leptonic = [0.00014, 0.056]
widthfractions_hadronic = [0.00014, 0.10]
spins = [0, 2]
PChannels = [0, 1]
DecayMode1s = 1, 8
couplingsspin0 = "ghz1=0,0 ghzgs2=1,0"
couplingsspin0gammastargamma = "ghz1=0,0 ghgsgs2=1,0"
couplingsspin2 = "a1=1,0 b1=1,0"

def makecard(mass, widthfraction, spin, PChannel, DecayMode1, DecayMode2, couplings):

    themap = {
              "mass": mass,
              "width": widthfraction*mass,
              "width_forname": "width" + str(widthfraction).replace(".", "p"),
              "spin": spin,
              "PChannel": PChannel,
              "DecayMode1": DecayMode1,
              "DecayMode2": DecayMode2,
              "couplings": couplings,
             }

    if mass not in masses: raise ValueError("Invalid mass {}".format(mass))
    if PChannel not in PChannels: raise ValueError("Invalid PChannel {}".format(PChannel))

    if DecayMode2 != 7: raise ValueError("Invalid DecayMode2 {}".format(DecayMode2))
    if DecayMode1 == 1:
        themap["decaymode_final"] = "2QG"
        if widthfraction not in widthfractions_hadronic: raise ValueError("Invalid widthfraction {} for hadronic Z".format(widthfraction))
    elif DecayMode1 == 8:
        themap["decaymode_final"] = "2LG"
        if widthfraction not in widthfractions_leptonic: raise ValueError("Invalid widthfraction {} for leptonic Z".format(widthfraction))
    else:
        raise ValueError("Invalid DecayMode1 {}".format(DecayMode1))

    if spin == 0:
        if PChannel != 0: raise ValueError("Invalid PChannel {} for spin 0".format(PChannel))
        themap["spin_forname"] = "Higgs0PM"
        themap["prod"] = ""
        if couplings == couplingsspin0:
            themap["decaymode"] = "ZG"
        elif couplings == couplingsspin0gammastargamma and mass == 750:
            themap["decaymode"] = "GG"
        else:
            raise ValueError("Invalid couplings {}".format(couplings))
    elif spin == 2:
        themap["spin_forname"] = "Graviton2PM"
        themap["decaymode"] = "ZG"
        if PChannel == 0:
            themap["prod"] = ""
        elif PChannel == 1:
            themap["prod"] = "qqbar"
        else:
            raise ValueError("Invalid PChannel {}".format(PChannel))
        if couplings != couplingsspin2: raise ValueError("Invalid couplings {}".format(couplings))
    else:
        raise ValueError("Invalid spin {}".format(spin))

    with open(cardname.format(**themap), "w") as f:
        f.write(card.format(**themap))

DecayMode2 = 7
for mass in masses:
    for spin in spins:
        for PChannel in PChannels:
            for DecayMode1 in DecayMode1s:
                for widthfraction in set(widthfractions_hadronic+widthfractions_leptonic):
                    for couplings in couplingsspin0, couplingsspin0gammastargamma, couplingsspin2:
                        try:
                            makecard(mass, widthfraction, spin, PChannel, DecayMode1, DecayMode2, couplings)
                        except ValueError:
                            pass

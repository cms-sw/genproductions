#MC plans as documented here:
#https://indico.cern.ch/event/481417/contribution/7/attachments/1212251/1768492/High_mass_ZZ4l.pdf

card = "Process=2 VegasNc0=1000000 VegasNc2=NEVT a1=1,0 a2=0,0 b2=0,0 b5=1,0 MReso=%(mass)i GaReso=%(width)i DecayMode1=%(decaymode1)i DecayMode2=%(decaymode2)i OffshellX=1 PChannel=%(pchannel)i LHAPDF=NNPDF30_lo_as_0130/NNPDF30_lo_as_0130.info Seed=SEED"
card_narrow = "Process=2 VegasNc0=1000000 VegasNc2=NEVT a1=1,0 a2=0,0 b2=0,0 b5=1,0 MReso=%(mass)i GaReso=0.000001 DecayMode1=%(decaymode1)i DecayMode2=%(decaymode2)i OffshellX=0 PChannel=%(pchannel)i LHAPDF=NNPDF30_lo_as_0130/NNPDF30_lo_as_0130.info Seed=SEED"
cardname = "Graviton2PB%(prod)sTo%(decaymode)s_%(width_forname)s_M-%(mass)i_13TeV-JHUgenV6.input"

masses = [750, 800, 1200, 2000, 3000, 4000]
widths = {mass: [mass/10, mass*2/10, mass*3/10] for mass in masses}
PChannels = {0: "gg", 1: "qqbar"}
DecayModes = {
              (9, 9):   "ZZ",
              (9, 3):   "ZZTo2Nu2Any",
              (0, 1):   "ZZTo2L2Q",
              (0, 0):   "ZZTo4L",
              (0, 3):   "ZZTo2L2Nu",
              (11, 11): "WW",
              (10, 5):  "WWToLNu2Q",
              (10, 10): "WW2L2Nu",
             }

for mass in masses:
    for PChannel in PChannels:
        for DecayMode in DecayModes:
            for width in widths[mass]:
                themap = {
                          "mass": mass,
                          "width": width,
                          "width_forname": "width"+str(float(width) / mass).replace(".","p"),
                          "pchannel": PChannel,
                          "prod": "qqbar" if PChannel == 1 else "",
                          "decaymode1": DecayMode[0],
                          "decaymode2": DecayMode[1],
                          "decaymode": DecayModes[DecayMode],
                         }
                with open(cardname % themap, "w") as f:
                    f.write(card % themap)

            if "2L2Nu" in DecayModes[DecayMode] or "4L" in DecayModes[DecayMode] or mass == 750:
                #narrow width
                themap["width"] = 0
                themap["width_forname"] = "width0"
                with open(cardname % themap, "w") as f:
                    f.write(card_narrow % themap)

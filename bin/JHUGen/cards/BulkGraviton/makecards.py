#MC plans as documented here:
#https://indico.cern.ch/event/481417/contribution/7/attachments/1212251/1768492/High_mass_ZZ4l.pdf

card = "Process=2 VegasNc2=NEVT a1=1,0 a2=0,0 b2=0,0 b5=1,0 MReso=%(mass)i GaReso=%(width)i DecayMode1=%(decaymode1)i DecayMode2=%(decaymode2)i OffXVV=111 PChannel=%(pchannel)i"
card_narrow = "Process=2 VegasNc2=NEVT a1=1,0 a2=0,0 b2=0,0 b5=1,0 MReso=%(mass)i GaReso=0.000001 DecayMode1=%(decaymode1)i DecayMode2=%(decaymode2)i OffXVV=011 PChannel=%(pchannel)i"
cardname = "JHUGen_2bp_%(pchannelname)s_m%(mass)i_ga%(width)i_%(decaymode)s.input"

masses = [800, 1200, 2000, 3000, 4000]
widths = {mass: [mass/10, mass*2/10, mass*3/10] for mass in masses}
PChannels = {0: "gg", 1: "qqbar"}
DecayModes = {
              (1, 1):   "ZZ4q",
              (1, 3):   "ZZ2q2nu",
              (8, 1):   "ZZ2l2q",
              (8, 8):   "ZZ4l",
              (8, 3):   "ZZ2l2nu",
              (5, 5):   "WW4q",
              (10, 5):  "WWlnu2q",
              (10, 10): "WW2l2nu",
             }

for mass in masses:
    for PChannel in PChannels:
        for DecayMode in DecayModes:
            for width in widths[mass]:
                themap = {
                          "mass": mass,
                          "width": width,
                          "pchannel": PChannel,
                          "pchannelname": PChannels[PChannel],
                          "decaymode1": DecayMode[0],
                          "decaymode2": DecayMode[1],
                          "decaymode": DecayModes[DecayMode],
                         }
                with open(cardname % themap, "w") as f:
                    f.write(card % themap)

            if "2l2nu" in DecayModes[DecayMode] or "4l" in DecayModes[DecayMode]:
                #narrow width
                themap["width"] = 0
                print cardname % themap
                with open(cardname % themap, "w") as f:
                    f.write(card_narrow % themap)

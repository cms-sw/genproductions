#!/usr/bin/env python

masswidth = (
 (120, 0.00351),
 (125, 0.00407),
 (130, 0.00491),
)

with open("ggHZ_HanythingJ_NNPDF31_13TeV_Vinclusive_template.input") as f:
  template = f.read()

for mass, width in masswidth:
  print "creating cards for mass", mass 
  with open("ggHZ_HanythingJ_NNPDF31_13TeV_M{}_Vinclusive.input".format(mass), "w") as f:
    f.write(template.format(mass=mass, width=width))

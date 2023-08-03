#!/usr/bin/env python

parameters = (
  (120, 0.00353,59.5),
  (130, 0.00492,60.5),
 )

with open("gg_H_quark-mass-effects_NNPDF31_13p6TeV_template.input") as f:
  template = f.read()

dct = {}

for dct["mass"], dct["width"], dct["hfact"] in parameters:
  with open("gg_H_quark-mass-effects_NNPDF31_13p6TeV_M{}.input".format(str(dct["mass"]).replace(".","p")), "w") as f:
    f.write(template.format(**dct))

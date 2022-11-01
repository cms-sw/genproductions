#!/usr/bin/env python

parameters = (
  (124, 0.00396, 59.9),
  (124.5, 0.00402, 59.95),
  (125, 0.00409, 60.0),
  (125.5, 0.00416, 60.05),
  (126, 0.00423, 60.1),
 )

with open("gg_H_quark-mass-effects_NNPDF31_13p6TeV_template.input") as f:
  template = f.read()

dct = {}

for dct["mass"], dct["width"], dct["hfact"] in parameters:
  with open("gg_H_quark-mass-effects_NNPDF31_13p6TeV_M{}.input".format(str(dct["mass"]).replace(".","p")), "w") as f:
    f.write(template.format(**dct))

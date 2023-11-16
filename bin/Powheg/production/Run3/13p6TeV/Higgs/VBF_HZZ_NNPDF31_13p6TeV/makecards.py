#!/usr/bin/env python

parameters = (
  (124, 0.00396),
  (124.5, 0.00402),
  (125, 0.00409),
  (125.5, 0.00416),
  (126, 0.00423),
 )

with open("VBF_H_ZZ_NNPDF31_13p6TeV_template.input") as f:
  template = f.read()

dct = {}

for dct["mass"], dct["width"] in parameters:
  with open("VBF_H_ZZ_NNPDF31_13p6TeV_M{}.input".format(str(dct["mass"]).replace(".","p")), "w") as f:
    f.write(template.format(**dct))

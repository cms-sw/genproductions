#!/usr/bin/env python
#150,200,300,400,500,600,800,1000?
parameters = (
#  (125, 0.00407),
  (150, 0.0173),
  (200, 1.43),
  (300, 8.43),
  (400, 29.2),
  (500, 68.0),
  (600, 123.0),
  (700, 199.0),
  (800, 304.0),
  (900, 449.0),
  (1000, 647.0),
)

with open("ttH_inclusive_hdamp_NNPDF31_13TeV_MH_template.input") as f:
  template = f.read()

dct = {}

for dct["mass"], dct["width"] in parameters:
  with open("ttH_inclusive_hdamp_NNPDF31_13TeV_M{}.input".format(dct["mass"]), "w") as f:
    f.write(template.format(**dct))

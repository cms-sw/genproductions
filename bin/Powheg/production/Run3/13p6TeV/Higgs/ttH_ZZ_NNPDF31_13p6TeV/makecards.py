#!/usr/bin/env python

masswidth = (
  (124, 0.00396),
  (124.5, 0.00402),
  (125, 0.00409),
  (125.5, 0.00416),
  (126, 0.00423),
)

with open("ttH_NNPDF31_13p6TeV_template.input") as f:
  template = f.read()

for mass, width in masswidth:
  with open("ttH_NNPDF31_13p6TeV_M{}.input".format(mass), "w") as f:
    f.write(template.format(mass=mass, width=width))

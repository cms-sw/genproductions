#!/usr/bin/env python

masswidth = (
  (115, 0.00312),
  (120, 0.00351),
  (124, 0.00394),
  (125, 0.00407),
  (126, 0.00421),
  (130, 0.00491),
  (135, 0.00618),
  (140, 0.00817),
  (145, 0.0114),
)

with open("ttH_inclusive_NNPDF31_13TeV_template.input") as f:
  template = f.read()

for mass, width in masswidth:
  with open("ttH_inclusive_NNPDF31_13TeV_M{}.input".format(mass), "w") as f:
    f.write(template.format(mass=mass, width=width))

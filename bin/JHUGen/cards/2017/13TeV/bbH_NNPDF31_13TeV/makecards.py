#!/usr/bin/env python

masses = (
  115,
  120,
  124,
  125,
  126,
  130,
  135,
  140,
  145,
)

with open("bbH_NNPDF31_13TeV_template.input") as f:
  template = f.read()

for mass in masses:
  with open("bbH_NNPDF31_13TeV_M{}.input".format(mass), "w") as f:
    f.write(template.format(mass=mass))

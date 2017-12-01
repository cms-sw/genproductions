#!/usr/bin/env python

masswidth = (
  (450, 46.8),
  (550, 93.0),
  (600, 123.0),
  (700, 199.0),
  (750, 247.0),
  (800, 304.0),
  (900, 449.0),
  (1000, 647.0),
  (1500, 750.0),
  (2000, 1000.0),
  (2500, 1250.0),
  (3000, 1500.0),
)

with open("HWJ_HanythingJ_NNPDF30_13TeV_template.input") as f:
  template = f.read()

for mass, width in masswidth:
  for sign, W in ("plus", 24), ("minus", -24):
    with open("HW{}J_HanythingJ_NNPDF30_13TeV_M{}.input".format(sign, mass), "w") as f:
      f.write(template.format(mass=mass, width=width, W=W))

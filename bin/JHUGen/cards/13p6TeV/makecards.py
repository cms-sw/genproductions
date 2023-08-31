#!/usr/bin/env python

masseswidth = (
  (124, 0.00396),
  (124.5, 0.00402),
  (125, 0.00409),
  (125.5, 0.00416),
  (126, 0.00423),
)

with open("bbH_NNPDF31_13p6TeV_template.input") as f:
  template = f.read()

for mass,width in masseswidth:
  with open("bbH_NNPDF31_13p6TeV_M{}.input".format(str(mass).replace(".","p")), "w") as f:
    f.write(template.format(mass=mass,width=width))

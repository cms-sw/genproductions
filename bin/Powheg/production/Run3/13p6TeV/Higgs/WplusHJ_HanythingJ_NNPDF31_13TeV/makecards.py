#!/usr/bin/env python

import argparse

masswidth = (
  (124, 0.00396),
  (124.5, 0.00402),
  (125, 0.00409),
  (125.5, 0.00416),
  (126, 0.00423),
 )

# Read the mass points from the command line
parser = argparse.ArgumentParser()
parser.add_argument('mass_points', help='List of mass points for which the cards will be generated.', nargs='*', type=int)
parser.add_argument('--decay_mode', help='The decay mode for the Z boson. Specify as: hadronic, leptonic, inclusive or neutrinos.', default='inclusive')
args = parser.parse_args()

mass_points = args.mass_points
decay_mode = args.decay_mode
template_filename = 'HWplusJ_HanythingJ_NNPDF31_13p6TeV_V{decay_mode}_template.input'.format(decay_mode=decay_mode)

with open(template_filename) as f:
  template = f.read()

for mass, width in masswidth:
  if mass not in mass_points:
    continue
  print "creating cards for mass", mass 
  with open("HWplusJ_HanythingJ_NNPDF31_13p6TeV_M{mass}_V{decay_mode}.input".format(mass=mass, decay_mode=decay_mode), "w") as f:
    # Calculate minimum and maximum Higgs masses to go into the Powheg card
    min_h_mass = mass / 10
    max_h_mass = mass * 10
    f.write(template.format(mass=mass, width=width, min_h_mass=min_h_mass, max_h_mass=max_h_mass))

#!/usr/bin/env python

import argparse

masswidth = (
  (115, 0.00315),
  (120, 0.00353),
  (130, 0.00492),
  (135, 0.00618),
  (140, 0.00817),
  (145, 0.0114),
  (150, 0.0173),
  (155, 0.0309),
  (160, 0.0831),
  (165, 0.246),
  (170, 0.38),
  (175, 0.501),
  (180, 0.631),
  (190, 1.04),
  (200, 1.43),
  (210, 1.85),
  (230, 2.82),
  (250, 4.04),
  (270, 5.55),
  (300, 8.43),
  (350, 15.2),
  (400, 29.2),
  (450, 46.8),
  (500, 68.0),
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
  print("creating cards for mass", mass)
  with open("HWplusJ_HanythingJ_NNPDF31_13p6TeV_M{mass}_V{decay_mode}.input".format(mass=mass, decay_mode=decay_mode), "w") as f:
    # Calculate minimum and maximum Higgs masses to go into the Powheg card
    min_h_mass = mass / 10
    max_h_mass = mass * 10
    f.write(template.format(mass=mass, width=width, min_h_mass=min_h_mass, max_h_mass=max_h_mass))

#!/usr/bin/env python
#-*- coding:utf-8 -*-
import os
import sys
import fnmatch
import logging
import shutil
from lib.card_lib import *
from lib.DM_lib import *
from lib.DMSimp_lib import *

log = logging.getLogger("create_dmsimp_monozll_spin0_cards.py")
def commandline():
    import optparse
    usage = '%prog [options]'
    descr = ""

    parser = optparse.OptionParser( usage=usage, description=descr, version='%prog v0' )

    parser.add_option(       '--debug', metavar='LEVEL', default='INFO',
                        help='Set the debug level. Allowed values: ERROR, WARNING, INFO, DEBUG. [default = %default]' )
    parser.add_option( '-j', '--jobs', metavar='NUMJOBS', type ='int', default=1,
                        help='Set the maximum number of parallel jobs to be ' \
                             'started [default = %default].' )
    parser.add_option( '-o', '--outpath', default='./out',
                        help='Set the output directory. [default = %default]' )
    ( options, args ) = parser.parse_args()

    format = '%(levelname)s (%(name)s) [%(asctime)s]: %(message)s'
    date = '%F %H:%M:%S'
    logging.basicConfig( level=logging._levelNames[ options.debug ], format=format, datefmt=date )

    # All good to go
    return ( options, args )


def main():
    options,args = commandline()
    create_dmsimp_spin0_monozll_cards_mass_scan()


def set_scalar_coupling(parameter_point):
    # Pseudoscalar Y0-Fermion couplings
    parameter_point.parameters["gpxd"].value =  0
    parameter_point.parameters["gpd11"].value = 0
    parameter_point.parameters["gpu11"].value = 0
    parameter_point.parameters["gpd22"].value = 0
    parameter_point.parameters["gpu22"].value = 0
    parameter_point.parameters["gpd33"].value = 0
    parameter_point.parameters["gpu33"].value = 0

    # Pseudoscalar Y0-Boson couplings
    parameter_point.parameters["gpg"].value = 0
    parameter_point.parameters["gpb"].value = 0
    parameter_point.parameters["gpw"].value = 0

    # Scalar Y0-Fermion couplings
    parameter_point.parameters["gsxd"].value =  1
    parameter_point.parameters["gsd11"].value = 1
    parameter_point.parameters["gsu11"].value = 1
    parameter_point.parameters["gsd22"].value = 1
    parameter_point.parameters["gsu22"].value = 1
    parameter_point.parameters["gsd33"].value = 1
    parameter_point.parameters["gsu33"].value = 1

    # Scalar couplings to non-fermion DM
    parameter_point.parameters["gsxr"].value =  0
    parameter_point.parameters["gsxc"].value =  0

    # Scalar Y0-Boson couplings
    parameter_point.parameters["gsg"].value = 0
    parameter_point.parameters["gsh1"].value = 0
    parameter_point.parameters["gsh2"].value = 0
    parameter_point.parameters["gsb"].value = 0
    parameter_point.parameters["gsw"].value = 0

def set_pseudo_coupling(parameter_point):
    # Pseudoscalar Y0-Fermion couplings
    parameter_point.parameters["gpxd"].value =  1
    parameter_point.parameters["gpd11"].value = 1
    parameter_point.parameters["gpu11"].value = 1
    parameter_point.parameters["gpd22"].value = 1
    parameter_point.parameters["gpu22"].value = 1
    parameter_point.parameters["gpd33"].value = 1
    parameter_point.parameters["gpu33"].value = 1

    # Pseudoscalar Y0-Boson couplings
    parameter_point.parameters["gpg"].value = 0
    parameter_point.parameters["gpb"].value = 0
    parameter_point.parameters["gpw"].value = 0

    # Scalar Y0-Fermion couplings
    parameter_point.parameters["gsxd"].value =  0
    parameter_point.parameters["gsd11"].value = 0
    parameter_point.parameters["gsu11"].value = 0
    parameter_point.parameters["gsd22"].value = 0
    parameter_point.parameters["gsu22"].value = 0
    parameter_point.parameters["gsd33"].value = 0
    parameter_point.parameters["gsu33"].value = 0

    # Scalar couplings to non-fermion DM
    parameter_point.parameters["gsxr"].value =  0
    parameter_point.parameters["gsxc"].value =  0

    # Scalar Y0-Boson couplings
    parameter_point.parameters["gsg"].value = 0
    parameter_point.parameters["gsh1"].value = 0
    parameter_point.parameters["gsh2"].value = 0
    parameter_point.parameters["gsb"].value = 0
    parameter_point.parameters["gsw"].value = 0


def create_dmsimp_spin0_monozll_cards_mass_scan():
    log.info("Writing DMSimp Pseudoscalar cards for mass scan.")
    cw = CardWriter("")
    cw.model = "DMsimp_s_spin0"

    ### ma-mA scan
    constant = DMSimp_Spin0_ParameterPoint()
    constant.parameters["mxd"].value = 1

    for my0 in [10] + range(50,550,50) + [750,1000]:
        p = DMSimp_Spin0_ParameterPoint(constant)
        p.parameters["MY0"].value = my0
        p.parameters["MY0"].printme = True
        p.parameters["mxd"].printme = True
        cw.parameter_points.append(p)

    # Widths for madgraph to calculate
    cw.auto_widths = ["WY0"]

    # Decay definition for madspin.
    # Only needed if you want to enforce a decay.
    cw.decay    = None

    
    cw.process = '''
define l+ = e+ mu+
define l- = e- mu-
generate       g g > xd xd~ l+ l- / h [QCD] @1
'''
    cw.template_path = "./templates/dmsimp_monozll_spin0_5f_nomerging/"


    for p in cw.parameter_points:
        set_pseudo_coupling(constant)

    cw.output_path = "./output/DMSimp_MonoZLL_Scalar/"
    cw.name = "MonoZLL_Scalar_GQ1p0_GDM1p0"
    cw.write_cards()


    for p in cw.parameter_points:
        set_pseudo_coupling(constant)
    cw.output_path = "./output/DMSimp_MonoZLL_Pseudo/"
    cw.name = "MonoZLL_Pseudo_GQ1p0_GDM1p0"
    cw.write_cards()

if __name__ == '__main__':
    main()


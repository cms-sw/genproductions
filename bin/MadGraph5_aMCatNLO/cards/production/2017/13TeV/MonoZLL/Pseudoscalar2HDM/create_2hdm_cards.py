#!/usr/bin/env python
#-*- coding:utf-8 -*-
import os
import sys
import fnmatch
import logging
import shutil
from lib.card_lib import *
from lib.TwoHDM_lib import *

log = logging.getLogger("create_2hdm_cards.py")
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
    create_2hdm_monozll_cards_mass_scan()

def create_2hdm_monozll_cards_mass_scan():
    log.info("Writing 2HDM cards for mass scan.")
    cw = CardWriter("")
    cw.model = "Pseudoscalar_2HDM"

    ### ma-mA scan
    constant = TwoHDMParameterPoint()
    constant.parameters["tanbeta"].value = 1
    constant.parameters["sinp"   ].value = 0.35
    constant.parameters["lam3"   ].value = 3
    constant.parameters["laP1"   ].value = 3
    constant.parameters["laP2"   ].value = 3
    constant.parameters["gPXd"   ].value = 1
    constant.parameters["sinbma" ].value = 1
    constant.parameters["Mxd"    ].value = 10
    constant.parameters["mh1"    ].value = 125


    for mh4 in range(100,600,100):
        for mh3 in range(mh4+100,1100,100) + range(1200,1600,200):
            p = TwoHDMParameterPoint(constant)
            p.parameters["mh4"  ].value = mh4
            p.parameters["mh2"  ].value = mh3
            p.parameters["mh3"  ].value = mh3
            p.parameters["mhc"  ].value = mh3

            for toprint in ["mh4","mh3"]:
                p.parameters[toprint].printme = True

            cw.parameter_points.append(p)

    # Widths for madgraph to calculate
    cw.auto_widths = ["Wh1","Wh2","Wh3","Whc","Wh4"]

    # Decay definition for madspin.
    # Only needed if you want to enforce a decay.
    cw.decay    = None

    # The process you want to simulate
    # Plain Madgraph syntax
    cw.process = '''
define l+ = e+ mu+
define l- = e- mu-
generate       g g > xd xd~ l+ l- / h1 [QCD]
'''

    ### Write 5F cards
    cw.template_path = "./templates/pseudoscalar_2hdm_5f/"
    cw.output_path = "./output/"
    cw.restrict = "bbMET_5FS"
    cw.name = "mScan_5F"
    cw.write_cards()

if __name__ == '__main__':
    main()

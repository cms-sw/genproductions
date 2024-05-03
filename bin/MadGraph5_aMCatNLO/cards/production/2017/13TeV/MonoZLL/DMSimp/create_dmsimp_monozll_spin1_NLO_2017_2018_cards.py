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

log = logging.getLogger("create_dmsimp_monozll_spin1_NLO_2017_2018_cards.py")


def commandline():
    import optparse
    usage = '%prog [options]'
    descr = ""

    parser = optparse.OptionParser(
        usage=usage, description=descr, version='%prog v0')

    parser.add_option('--debug', metavar='LEVEL', default='INFO',
                      help='Set the debug level. Allowed values: ERROR, WARNING, INFO, DEBUG. [default = %default]')
    parser.add_option('-j', '--jobs', metavar='NUMJOBS', type='int', default=1,
                      help='Set the maximum number of parallel jobs to be '
                      'started [default = %default].')
    parser.add_option('-o', '--outpath', default='./out',
                      help='Set the output directory. [default = %default]')
    (options, args) = parser.parse_args()

    format = '%(levelname)s (%(name)s) [%(asctime)s]: %(message)s'
    date = '%F %H:%M:%S'
    logging.basicConfig(
        level=logging._levelNames[options.debug], format=format, datefmt=date)

    # All good to go
    return (options, args)


def main():
    options, args = commandline()
    create_dmsimp_spin1_monozll_LO_vector()


def create_dmsimp_spin1_monozll_LO_vector():
    log.info("Writing DMSimp Vector cards for mass scan.")
    cw = CardWriter("")
    cw.model = "DMsimp_s_spin1"

    constant = DMSimp_Spin1_ParameterPoint()

    # Vector couplings betwen mediator and fermions
    constant.parameters["gvxd"].value = 1
    constant.parameters["gvxc"].value = 0
    constant.parameters["gvd11"].value = 0.25
    constant.parameters["gvu11"].value = 0.25
    constant.parameters["gvd22"].value = 0.25
    constant.parameters["gvu22"].value = 0.25
    constant.parameters["gvd33"].value = 0.25
    constant.parameters["gvu33"].value = 0.25

    # Vector couplings betwen mediator and bosons
    constant.parameters["gVh"].value = 0

    # Axial couplings betwen mediator and fermions
    constant.parameters["gaxd"].value = 0
    constant.parameters["gaxc"].value = 0
    constant.parameters["gad11"].value = 0
    constant.parameters["gau11"].value = 0
    constant.parameters["gad22"].value = 0
    constant.parameters["gau22"].value = 0
    constant.parameters["gad33"].value = 0
    constant.parameters["gau33"].value = 0

    for my0 in [10, 100, 250, 500, 750, 1000, 1250, 1500, 1750, 2000]:
        mx = 1
        p = DMSimp_Spin1_ParameterPoint(constant)
        p.parameters["MY1"].value = my0

        p.parameters["MY1"].printme = True

        p.parameters["MXd"].value = mx
        p.parameters["MXd"].printme = True
        cw.parameter_points.append(p)

    my0 = 1e3
    for mx in [1, 10, 50, 100, 200, 300, 350, 400, 450, 490, 510, 600]:
        p = DMSimp_Spin1_ParameterPoint(constant)
        p.parameters["MY1"].value = my0

        p.parameters["MY1"].printme = True

        p.parameters["MXd"].value = mx
        p.parameters["MXd"].printme = True
        cw.parameter_points.append(p)

    my0 = 5e2
    for mx in [1, 10, 50, 100, 200, 240, 260, 300]:
        p = DMSimp_Spin1_ParameterPoint(constant)
        p.parameters["MY1"].value = my0

        p.parameters["MY1"].printme = True

        p.parameters["MXd"].value = mx
        p.parameters["MXd"].printme = True
        cw.parameter_points.append(p)

    # Widths for madgraph to calculate
    cw.auto_widths = ["WY1"]

    # Decay definition for madspin.
    # Only needed if you want to enforce a decay.
    cw.decay = None

    # The process you want to simulate
    # Plain Madgraph syntax
    cw.process = '''
define l+ = e+ mu+
define l- = e- mu-
generate       p p > l+ l- xd xd~ [QCD]
'''

    # Vector
    cw.template_path = "./templates/dmsimp_monozll_spin1_NLO_5f_mg26x"
    cw.output_path = "./output/DMSimp_MonoZLL_NLO_Vector/"
    cw.restrict = ""
    cw.name = "MonoZLL_NLO_Vector_GQ0p25_GDM1p0"

    cw.write_cards()


    cw.output_path = "./output/DMSimp_MonoZLL_NLO_Axial/"
    cw.restrict = ""
    cw.name = "MonoZLL_NLO_Axial_GQ0p25_GDM1p0"

   # Vector couplings betwen mediator and fermions
    constant.parameters["gvxd"].value = 0
    constant.parameters["gvxc"].value = 0
    constant.parameters["gvd11"].value = 0
    constant.parameters["gvu11"].value = 0
    constant.parameters["gvd22"].value = 0
    constant.parameters["gvu22"].value = 0
    constant.parameters["gvd33"].value = 0
    constant.parameters["gvu33"].value = 0

    # Vector couplings betwen mediator and bosons
    constant.parameters["gVh"].value = 0

    # Axial couplings betwen mediator and fermions
    constant.parameters["gaxd"].value = 1
    constant.parameters["gaxc"].value = 0
    constant.parameters["gad11"].value = 0.25
    constant.parameters["gau11"].value = 0.25
    constant.parameters["gad22"].value = 0.25
    constant.parameters["gau22"].value = 0.25
    constant.parameters["gad33"].value = 0.25
    constant.parameters["gau33"].value = 0.25


    cw.write_cards()


if __name__ == '__main__':
    main()

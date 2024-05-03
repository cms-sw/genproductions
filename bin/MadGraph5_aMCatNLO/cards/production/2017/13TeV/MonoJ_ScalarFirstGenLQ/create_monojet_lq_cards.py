#!/usr/bin/env python
#-*- coding:utf-8 -*-
import os
import sys
import fnmatch
import logging
import shutil
from lib.card_lib import *
from lib.LQlib import ScalarFirstGenLQ_ParameterPoint

log = logging.getLogger("create_monojet_lq_cards.py")
def commandline():
    import optparse
    usage = '%prog [options]'
    descr = ""

    parser = optparse.OptionParser( usage=usage, description=descr, version='%prog v0' )

    parser.add_option(       '--debug', metavar='LEVEL', default='INFO',
                        help='Set the debug level. Allowed values: ERROR, WARNING, INFO, DEBUG. [default = %default]' )
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
    create_monojet_ScalarFirstGenLQ_cards()

def create_monojet_ScalarFirstGenLQ_cards():
    log.info("Writing monojet first generation LQ cards.")
    cw = CardWriter("")
    cw.model = "sleptoquark_uv_UFO"
    lambdas_all = [0.01, 0.05, 0.1, 0.25, 0.5, 0.7, 1, 1.25, 1.5]
    
    def parameterAdd(mlq, lam):
	p = ScalarFirstGenLQ_ParameterPoint()
	p.parameters["Mlq"].value = mlq
	p.parameters["Mlq"].printme = True
	p.parameters["Ylq"].value = lam
	p.parameters["Ylq"].printme = True
	cw.parameter_points.append(p)


    for mlq in range(500, 2750, 250): 
	if mlq == 500:
	    lambdas = lambdas_all[:2]
	    for lam in lambdas: 
		parameterAdd(mlq, lam)

	elif mlq == 750:
	    lambdas = lambdas_all[1:4] 
	    for lam in lambdas: 
		parameterAdd(mlq, lam)
	
	elif mlq == 1000:
	    lambdas = lambdas_all[2:5]
	    for lam in lambdas: 
		parameterAdd(mlq, lam)
	
	elif mlq == 1250:
	    lambdas = lambdas_all[3:6]
	    for lam in lambdas: 
		parameterAdd(mlq, lam)
	
	elif mlq == 1500:
	    lambdas = lambdas_all[4:7]
	    for lam in lambdas: 
		parameterAdd(mlq, lam)
	
	elif mlq == 1750:
	    lambdas = lambdas_all[5:8]
	    for lam in lambdas: 
		parameterAdd(mlq, lam)
	
	elif mlq == 2000:
	    lambdas = lambdas_all[6:9]
	    for lam in lambdas: 
		parameterAdd(mlq, lam)
	
	elif mlq == 2250:
	    lambdas = lambdas_all[7:9]
	    for lam in lambdas: 
		parameterAdd(mlq, lam)
	
	else:
	    lambdas = lambdas_all[-1]
	    parameterAdd(mlq, lambdas)
	
    # Widths for madgraph to calculate
    cw.auto_widths = ["Wlq"]

    # Decay definition for madspin.
    # Only needed if you want to enforce a decay.
    cw.decay    = None

    # The process you want to simulate
    # Plain Madgraph syntax
    cw.process = '''
set group_subprocesses Auto
set ignore_six_quark_processes False
set loop_optimized_output True
set loop_color_flows False
set gauge unitary
set complex_mass_scheme False
set max_npoint_for_channel 0
define p = g u c d s b u~ c~ d~ s~ b~
define j = g u c d s b u~ c~ d~ s~ b~
define l+ = e+ mu+
define l- = e- mu-
define vl = ve vm vt
define vl~ = ve~ vm~ vt~
generate p p > j ve ve~ / z h a QED = 99
add process p p > sun sun~ QED = 99, sun > u ve~, sun~ > u~ ve
    '''

    ### Write 5F cards
    cw.template_path = "./templates/monojet_scalar_lq_gen1/"
    cw.output_path = "./output/MonoJ_ScalarFirstGenLQ/"
    cw.name = "ScalarFirstGenLQ"
    cw.write_cards()


if __name__ == "__main__":
    main()

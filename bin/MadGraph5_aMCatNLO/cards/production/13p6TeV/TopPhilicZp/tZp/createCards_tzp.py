import os

# make proc card
def make_proc_card(M, W):
	card = """set default_unset_couplings 99
set group_subprocesses Auto 
set ignore_six_quark_processes False 
set loop_optimized_output True 
set loop_color_flows False
set gauge unitary
set complex_mass_scheme True 
set max_npoint_for_channel 0
import model sm
define p = g u c d s u~ c~ d~ s~
define j = g u c d s u~ c~ d~ s~
define l+ = e+ mu+ ta+
define l- = e- mu- ta-
define vl = ve vm vt
define vl~ = ve~ vm~ vt~
import model Top-Philic_UFO_V1 
define p = p b b~ 
define quark = u u~ d d~ s s~ c c~
define top = t t~
define wboson = w+ w-
define wall = u u~ d d~ s s~ c c~ b b~ e+ e- mu+ mu- ta+ ta- ve ve~ vm vm~ vt vt~
generate p p > quark top v1, v1 > t t~
add process p p > wboson top v1, v1 > t t~
output TopPhilic_tzp_13p6TeV_nodecay_m{:.0f}_relwidth{:.0f} -nojpeg""".format(M, W)

	return card

def make_run_card():
	card = """#*********************************************************************
#                       MadGraph5_aMC@NLO                            *
#                                                                    *
#                     run_card.dat MadEvent                          *
#                                                                    *
#  This file is used to set the parameters of the run.               *
#                                                                    *
#  Some notation/conventions:                                        *
#                                                                    *
#   Lines starting with a '# ' are info or comments                  *
#                                                                    *
#   mind the format:   value    = variable     ! comment             *
#                                                                    *
#   To display more options, you can type the command:               *
#      update full_run_card                                          *
#*********************************************************************
#                                                                    
#*********************************************************************
# Tag name for the run (one word)                                    *
#*********************************************************************
  tag_1     = run_tag ! name of the run 
#*********************************************************************
# Number of events and rnd seed                                      *
# Warning: Do not generate more than 1M events in a single run       *
#*********************************************************************
  10000 = nevents ! Number of unweighted events requested 
  0   = iseed   ! rnd seed (0=assigned automatically=default))
#*********************************************************************
# Collider type and energy                                           *
# lpp: 0=No PDF, 1=proton, -1=antiproton, 2=photon from proton,      *
#                                         3=photon from electron     *
#*********************************************************************
     1        = lpp1    ! beam 1 type 
     1        = lpp2    ! beam 2 type
     6800.0     = ebeam1  ! beam 1 total energy in GeV 
     6800.0     = ebeam2  ! beam 2 total energy in GeV 
# To see polarised beam options: type "update beam_pol"

#*********************************************************************
# PDF CHOICE: this automatically fixes also alpha_s and its evol.    *
#*********************************************************************
  lhapdf  = pdlabel ! PDF set    
  $DEFAULT_PDF_SETS = lhaid
  $DEFAULT_PDF_MEMBERS = reweight_PDF
# To see heavy ion options: type "update ion_pdf"
#*********************************************************************
# Renormalization and factorization scales                           *
#*********************************************************************
 False = fixed_ren_scale  ! if .true. use fixed ren scale
 False        = fixed_fac_scale  ! if .true. use fixed fac scale
 91.188  = scale            ! fixed ren scale
 91.188  = dsqrt_q2fact1    ! fixed fact scale for pdf1
 91.188  = dsqrt_q2fact2    ! fixed fact scale for pdf2
 -1 = dynamical_scale_choice ! Choose one of the preselected dynamical choices
 1.0  = scalefact        ! scale factor for event-by-event scales
#*********************************************************************
# Type and output format
#*********************************************************************
  False     = gridpack  !True = setting up the grid pack
  -1.0 = time_of_flight ! threshold (in mm) below which the invariant livetime is not written (-1 means not written)
  average =  event_norm       ! average/sum. Normalization of the weight in the LHEF
# To see MLM/CKKW  merging options: type "update MLM" or "update CKKW"

#*********************************************************************
#
#*********************************************************************
# handling of the helicities:
#  0: sum over all helicities
#  1: importance sampling over helicities
#*********************************************************************
   0  = nhel          ! using helicities importance sampling or not.
#*********************************************************************
# Generation bias, check the wiki page below for more information:   *
#  'cp3.irmp.ucl.ac.be/projects/madgraph/wiki/LOEventGenerationBias' *
#*********************************************************************
 None = bias_module  ! Bias type of bias, [None, ptj_bias, -custom_folder-]
 {} = bias_parameters ! Specifies the parameters of the module.
#
#*******************************
# Parton level cuts definition *
#*******************************
#
#
#*********************************************************************
#*********************************************************************
  15.0  = bwcutoff      ! (M+/-bwcutoff*Gamma)
#*********************************************************************
# Standard Cuts                                                      *
#*********************************************************************
# Minimum and maximum pt's (for max, -1 means no cut)                *
#*********************************************************************
 {} = pt_min_pdg ! pt cut for other particles (use pdg code). Applied on particle and anti-particle
 {} = pt_max_pdg ! pt cut for other particles (syntax e.g. {6: 100, 25: 50})
#
# For display option for energy cut in the partonic center of mass frame type 'update ecut'
#
#*********************************************************************
# Maximum and minimum absolute rapidity (for max, -1 means no cut)   *
#*********************************************************************
 {} = eta_min_pdg ! rap cut for other particles (use pdg code). Applied on particle and anti-particle
 {} = eta_max_pdg ! rap cut for other particles (syntax e.g. {6: 2.5, 23: 5})
#*********************************************************************
# Minimum and maximum DeltaR distance                                *
#*********************************************************************
#*********************************************************************
# Minimum and maximum invariant mass for pairs                       *
#*********************************************************************
 {} = mxx_min_pdg ! min invariant mass of a pair of particles X/X~ (e.g. {6:250})
 {'default': False} = mxx_only_part_antipart ! if True the invariant mass is applied only
                       ! to pairs of particle/antiparticle and not to pairs of the same pdg codes.
#*********************************************************************
# Inclusive cuts                                                     *
#*********************************************************************
 0.0  = ptheavy   ! minimum pt for at least one heavy final state
#*********************************************************************
# maximal pdg code for quark to be considered as a light jet         *
# (otherwise b cuts are applied)                                     *
#*********************************************************************
 4 = maxjetflavor    ! Maximum jet pdg code
#*********************************************************************
#
#*********************************************************************
# Store info for systematics studies                                 *
# WARNING: Do not use for interference type of computation           *
#*********************************************************************
   True  = use_syst      ! Enable systematics studies"""
	return card

def make_customizecards(M,W):
	card = """set param_card Mv1 {}
set param_card theta1 7.853982e-01
set param_card ct1 1
set param_card mb 0
set param_card mt 172.5
set param_card Wv1 {}""".format(M, W)
	return card

def make_extramodels():
	card = "Top-Philic_UFO_V1.zip"
	return card
	
if __name__ == "__main__":
	# To make example cards for genproduction
	
	# create cards for each mass point
	for M in [500,750,1000,1250,1500,1750,2000,2500,3000,4000]:
		for W in [4,10,20,50]:
			print("creating crads for Z' mass {}, relative width {}%...".format(M, W))
			base = "TopPhilic_tzp_13p6TeV_nodecay_m{:.0f}_relwidth{:.0f}".format(M, W)

			# create directory
			os.mkdir(base)

			# make cards
			proc_card = open("{}/proc_card.dat".format(base), "w")
			proc_card.write(make_proc_card(M, W))
			proc_card.close()

			run_card = open("{}/run_card.dat".format(base), "w")
			run_card.write(make_run_card())
			run_card.close()

			customizecards = open("{}/customizecards.dat".format(base), "w")
			customizecards.write(make_customizecards(M, W))
			customizecards.close()

			extramodels = open("{}/extramodels.dat".format(base), "w")
			extramodels.write(make_extramodels())
			extramodels.close()
	print("all cards are created")

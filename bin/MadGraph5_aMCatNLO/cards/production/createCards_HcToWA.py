import os, shutil

# make proc card
def make_proc_card(MHc, MA):
	card = """import model 2HDM_UFO-type1_5FS

define p = g u d c s b u~ d~ c~ s~ b~
define j = p
define l+ = e+ mu+ ta+
define l- = e- mu- ta-
define vl = ve vm vt
define vl~ = ve~ vm~ vt~

generate p p > t t~, (t > b h+, (h+ > h3 l+ vl, (h3 > mu+ mu-))), (t~ > b~ w-, (w- > j j )) @0
add process p p > t t~, (t > b h+, (h+ > h3 j j, (h3 > mu+ mu-))), (t~ > b~ w-, (w- > l- vl~)) @1
add process p p > t t~, (t > b h+, (h+ > h3 l+ vl, (h3 > mu+ mu-))), (t~ > b~ w-, (w- > l- vl~)) @2
add process p p > t t~, (t > b w+, (w+ > j j )), (t~ > b~ h-, (h- > h3 l- vl~, (h3 > mu+ mu-))) @3
add process p p > t t~, (t > b w+, (w+ > l+ vl)), (t~ > b~ h-, (h- > h3 j j, (h3  > mu+ mu-))) @4
add process p p > t t~, (t > b w+, (w+ > l+ vl)), (t~ > b~ h-, (h- > h3 l- vl~, (h3 > mu+ mu-))) @5
output TTToHcToWA_AToMuMu_MHc{}_MA{} -nojpeg
""".format(MHc, MA)

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
#*******************
# Running parameters
#*******************
#
#*********************************************************************
# Tag name for the run (one word)                                    *
#*********************************************************************
  tag_1     = run_tag ! name of the run
#*********************************************************************
# Number of events and rnd seed                                      *
# Warning: Do not generate more than 1M events in a single run       *
# If you want to run Pythia, avoid more than 50k events in a run.    *
#*********************************************************************
  2000 = nevents ! Number of unweighted events requested
  0   = iseed   ! rnd seed (0=assigned automatically=default))
#*********************************************************************
# Collider type and energy                                           *
# lpp: 0=No PDF, 1=proton, -1=antiproton, 2=photon from proton,      *
#                                         3=photon from electron     *
#*********************************************************************
     1        = lpp1    ! beam 1 type
     1        = lpp2    ! beam 2 type
     6500.0     = ebeam1  ! beam 1 total energy in GeV
     6500.0     = ebeam2  ! beam 2 total energy in GeV
# To see polarised beam options: type "update beam_pol"
#*********************************************************************
# PDF CHOICE: this automatically fixes also alpha_s and its evol.    *
#*********************************************************************
     lhapdf    = pdlabel     ! PDF set
     $DEFAULT_PDF_SETS    = lhaid     ! if pdlabel=lhapdf, this is the lhapdf number
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
  3.0 = lhe_version       ! Change the way clustering information pass to shower.
  True = clusinfo         ! include clustering tag in output
  average =  event_norm       ! average/sum. Normalization of the weight in the LHEF

#*********************************************************************
# Matching parameter (MLM only)
#*********************************************************************
 0 = ickkw            ! 0 no matching, 1 MLM
 1.0 = alpsfact         ! scale factor for QCD emission vx
 False = chcluster        ! cluster only according to channel diag
 5 = asrwgtflavor     ! highest quark flavor for a_s reweight
 False  = auto_ptj_mjj  ! Automatic setting of ptj and mjj if xqcut >0
                                   ! (turn off for VBF and single top processes)
 0.0   = xqcut   ! minimum kt jet measure between partons
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
# BW cutoff (M+/-bwcutoff*Gamma) ! Define on/off-shell for "$" and decay
#*********************************************************************
  15.0  = bwcutoff      ! (M+/-bwcutoff*Gamma)
#*********************************************************************
# Apply pt/E/eta/dr/mij/kt_durham cuts on decay products or not
# (note that etmiss/ptll/ptheavy/ht/sorted cuts always apply)
#*********************************************************************
   False  = cut_decays    ! Cut decay products
#*********************************************************************
# Standard Cuts
# Minimum and maximum pt's (for max, -1 means no cut)                *
#*********************************************************************
 0.0  = ptj       ! minimum pt for the jets
 0.0  = ptb       ! minimum pt for the b
 0.0  = pta       ! minimum pt for the photons
 0.0  = ptl       ! minimum pt for the charged leptons
 0.0  = misset    ! minimum missing Et (sum of neutrino's momenta)
 -1.0  = ptjmax    ! maximum pt for the jets
 -1.0  = ptbmax    ! maximum pt for the b
 -1.0  = ptamax    ! maximum pt for the photons
 -1.0  = ptlmax    ! maximum pt for the charged leptons
 -1.0  = missetmax ! maximum missing Et (sum of neutrino's momenta)
 {} = pt_min_pdg ! pt cut for other particles (use pdg code). Applied on particle and anti-particle
 {} = pt_max_pdg ! pt cut for other particles (syntax e.g. {6: 100, 25: 50})
#*********************************************************************
# Minimum and maximum E's (in the center of mass frame)              *
#*********************************************************************
  0.0  = ej     ! minimum E for the jets
  0.0  = eb     ! minimum E for the b
  0.0  = ea     ! minimum E for the photons
  0.0  = el     ! minimum E for the charged leptons
  -1.0   = ejmax ! maximum E for the jets
 -1.0   = ebmax ! maximum E for the b
 -1.0   = eamax ! maximum E for the photons
 -1.0   = elmax ! maximum E for the charged leptons
 {} = e_min_pdg ! E cut for other particles (use pdg code). Applied on particle and anti-particle
 {} = e_max_pdg ! E cut for other particles (syntax e.g. {6: 100, 25: 50})
#*********************************************************************
# Maximum and minimum absolute rapidity (for max, -1 means no cut)   *
#*********************************************************************
 -1.0 = etaj    ! max rap for the jets
 -1.0  = etab    ! max rap for the b
 -1.0  = etaa    ! max rap for the photons
 -1.0  = etal    ! max rap for the charged leptons
 0.0  = etajmin ! min rap for the jets
 0.0  = etabmin ! min rap for the b
 0.0  = etaamin ! min rap for the photons
 0.0  = etalmin ! main rap for the charged leptons
 {} = eta_min_pdg ! rap cut for other particles (use pdg code). Applied on particle and anti-particle
 {} = eta_max_pdg ! rap cut for other particles (syntax e.g. {6: 2.5, 23: 5})
#*********************************************************************
# Minimum and maximum DeltaR distance                                *
#*********************************************************************
0.0 = drjj    ! min distance between jets
 0.0   = drbb    ! min distance between b's
 0.0 = drll    ! min distance between leptons
 0.0 = draa    ! min distance between gammas
 0.0   = drbj    ! min distance between b and jet
 0.0 = draj    ! min distance between gamma and jet
 0.0 = drjl    ! min distance between jet and lepton
 0.0   = drab    ! min distance between gamma and b
 0.0   = drbl    ! min distance between b and lepton
 0.0 = dral    ! min distance between gamma and lepton
 -1.0  = drjjmax ! max distance between jets
 -1.0  = drbbmax ! max distance between b's
 -1.0  = drllmax ! max distance between leptons
 -1.0  = draamax ! max distance between gammas
 -1.0  = drbjmax ! max distance between b and jet
 -1.0  = drajmax ! max distance between gamma and jet
 -1.0  = drjlmax ! max distance between jet and lepton
 -1.0  = drabmax ! max distance between gamma and b
 -1.0  = drblmax ! max distance between b and lepton
 -1.0  = dralmax ! maxdistance between gamma and lepton
#*********************************************************************
# Minimum and maximum invariant mass for pairs                       *
# WARNING: for four lepton final state mmll cut require to have      *
#          different lepton masses for each flavor!                  *
#*********************************************************************
 0.0   = mmjj    ! min invariant mass of a jet pair
 0.0   = mmbb    ! min invariant mass of a b pair
 0.0   = mmaa    ! min invariant mass of gamma gamma pair
 0.0   = mmll    ! min invariant mass of l+l- (same flavour) lepton pair
 -1.0  = mmjjmax ! max invariant mass of a jet pair
 -1.0  = mmbbmax ! max invariant mass of a b pair
 -1.0  = mmaamax ! max invariant mass of gamma gamma pair
 -1.0  = mmllmax ! max invariant mass of l+l- (same flavour) lepton pair
 {} = mxx_min_pdg ! min invariant mass of a pair of particles X/X~ (e.g. {6:250})
 {'default': False} = mxx_only_part_antipart ! if True the invariant mass is applied only
                       ! to pairs of particle/antiparticle and not to pairs of the same pdg codes.
#*********************************************************************
# Minimum and maximum invariant mass for all letpons                 *
#*********************************************************************
 0.0   = mmnl    ! min invariant mass for all letpons (l+- and vl)
 -1.0  = mmnlmax ! max invariant mass for all letpons (l+- and vl)
#*********************************************************************
# Minimum and maximum pt for 4-momenta sum of leptons                *
#*********************************************************************
 0.0   = ptllmin  ! Minimum pt for 4-momenta sum of leptons(l and vl)
 -1.0  = ptllmax  ! Maximum pt for 4-momenta sum of leptons(l and vl)
#*********************************************************************
# Inclusive cuts                                                     *
#*********************************************************************
 0.0  = ptheavy   ! minimum pt for at least one heavy final state
 0.0  = xptj ! minimum pt for at least one jet
 0.0  = xptb ! minimum pt for at least one b
 0.0  = xpta ! minimum pt for at least one photon
 0.0  = xptl ! minimum pt for at least one charged lepton
#*********************************************************************
# Control the pt's of the jets sorted by pt                          *
#*********************************************************************
 0.0   = ptj1min ! minimum pt for the leading jet in pt
 0.0   = ptj2min ! minimum pt for the second jet in pt
 0.0   = ptj3min ! minimum pt for the third jet in pt
 0.0   = ptj4min ! minimum pt for the fourth jet in pt
 -1.0  = ptj1max ! maximum pt for the leading jet in pt
 -1.0  = ptj2max ! maximum pt for the second jet in pt
 -1.0  = ptj3max ! maximum pt for the third jet in pt
 -1.0  = ptj4max ! maximum pt for the fourth jet in pt
 0   = cutuse  ! reject event if fails any (0) / all (1) jet pt cuts
#*********************************************************************
# Control the pt's of leptons sorted by pt                           *
#*********************************************************************
 0.0   = ptl1min ! minimum pt for the leading lepton in pt
 0.0   = ptl2min ! minimum pt for the second lepton in pt
 0.0   = ptl3min ! minimum pt for the third lepton in pt
 0.0   = ptl4min ! minimum pt for the fourth lepton in pt
 -1.0  = ptl1max ! maximum pt for the leading lepton in pt
 -1.0  = ptl2max ! maximum pt for the second lepton in pt
 -1.0  = ptl3max ! maximum pt for the third lepton in pt
 -1.0  = ptl4max ! maximum pt for the fourth lepton in pt
#*********************************************************************
# Control the Ht(k)=Sum of k leading jets                            *
#*********************************************************************
 0.0   = htjmin ! minimum jet HT=Sum(jet pt)
 -1.0  = htjmax ! maximum jet HT=Sum(jet pt)
 0.0   = ihtmin  !inclusive Ht for all partons (including b)
 -1.0  = ihtmax  !inclusive Ht for all partons (including b)
 0.0   = ht2min ! minimum Ht for the two leading jets
 0.0   = ht3min ! minimum Ht for the three leading jets
 0.0   = ht4min ! minimum Ht for the four leading jets
 -1.0  = ht2max ! maximum Ht for the two leading jets
 -1.0  = ht3max ! maximum Ht for the three leading jets
 -1.0  = ht4max ! maximum Ht for the four leading jets
#***********************************************************************
# Photon-isolation cuts, according to hep-ph/9801442                   *
# When ptgmin=0, all the other parameters are ignored                  *
# When ptgmin>0, pta and draj are not going to be used                 *
#***********************************************************************
 0.0 = ptgmin ! Min photon transverse momentum
 0.4 = R0gamma ! Radius of isolation code
 1.0 = xn ! n parameter of eq.(3.4) in hep-ph/9801442
 1.0 = epsgamma ! epsilon_gamma parameter of eq.(3.4) in hep-ph/9801442
 True = isoEM ! isolate photons from EM energy (photons and leptons)
#*********************************************************************
# WBF cuts                                                           *
#*********************************************************************
 0.0   = xetamin ! minimum rapidity for two jets in the WBF case
 0.0   = deltaeta ! minimum rapidity for two jets in the WBF case
#***********************************************************************
# Turn on either the ktdurham or ptlund cut to activate                *
# CKKW(L) merging with Pythia8 [arXiv:1410.3012, arXiv:1109.4829]      *
#***********************************************************************
-1.0  =  ktdurham
 0.4   =  dparameter
 -1.0  =  ptlund
 1, 2, 3, 4, 5, 6, 21  =  pdgs_for_merging_cut ! PDGs for two cuts above
#*********************************************************************
# maximal pdg code for quark to be considered as a light jet         *
# (otherwise b cuts are applied)                                     *
#*********************************************************************
 5 = maxjetflavor    ! Maximum jet pdg code
#*********************************************************************
#
#*********************************************************************
# Store info for systematics studies                                 *
# WARNING: Do not use for interference type of computation           *
#*********************************************************************
   True  = use_syst      ! Enable systematics studies"""
	
	return card

def make_customizecards(MHc, MA):
	card = """# Define Higgs mass
# 25:h, 35:H, 36: A, 37: Hc
set param_card mass 25 125.1
set param_card mass 35 3000.0
set param_card mass 36 {}
set param_card mass 37 {}
set param_card DECAY 25 0.013
# set param_card DECAY 35 0.001
set param_card DECAY 36 0.001
set param_card DECAY 37 0.001""".format(MA, MHc)
	return card

def make_extramodels():
	card = "2HDM_UFO.tar.gz"
	return card
	
if __name__ == "__main__":
	# mass points as a dictionary
	# key: MHc, value: MAs
	mass_points = dict()
	# mass_points[160] = [15, 85, 120, 155]
	# mass_points[130] = [15, 55, 90, 125]
	# mass_points[100] = [15, 60, 95]
	# mass_points[80] = [15, 75]
	
	# To make example cards for genproduction
	mass_points[160] = [15, 120]
	
	# create base directory
	try:
		if os.listdir("13Tev/TTToHcToWA_AToMuMu"):
			shutil.rmtree("13TeV/TTToHcToWA_AToMuMu")
	except:
		pass
	os.mkdir("13TeV/TTToHcToWA_AToMuMu")
	
	# create cards for each mass point
	for MHc, MAs in mass_points.items():
		for MA in MAs:
			print("creating crads for mass point MHc{}, MA{}...".format(MHc, MA))
			base = "TTToHcToWA_AToMuMu_MHc{}_MA{}".format(MHc, MA)

			# erase directory if exists
			try:
				if os.listdir("13TeV/TTToHcToWA_AToMuMu/{}".format(base)):
					shutil.rmtree("13TeV/TTToHcToWA_AToMuMu/{}".format(base))
			except:
				pass
			# create directory
			os.mkdir("13TeV/TTToHcToWA_AToMuMu/{}".format(base))

			# make cards
			proc_card = open("13TeV/TTToHcToWA_AToMuMu/{}/{}_proc_card.dat".format(base, base), "w")
			proc_card.write(make_proc_card(MHc, MA))
			proc_card.close()

			run_card = open("13TeV/TTToHcToWA_AToMuMu/{}/{}_run_card.dat".format(base, base), "w")
			run_card.write(make_run_card())
			run_card.close()

			customizecards = open("13TeV/TTToHcToWA_AToMuMu/{}/{}_customizecards.dat".format(base, base), "w")
			customizecards.write(make_customizecards(MHc, MA))
			customizecards.close()

			extramodels = open("13TeV/TTToHcToWA_AToMuMu/{}/{}_extramodels.dat".format(base, base), "w")
			extramodels.write(make_extramodels())
			extramodels.close()
	print("all cards are created")
		

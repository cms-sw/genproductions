#!/bin/bash
code_dir=$PWD
for mass in 100 150 200 250 300 350 400 450 500 550 600 650 700 750 800 850 900 950 1000
do
    cd $code_dir
    echo $mass;
    dir1=$code_dir"/VectorLikeLeptons_M-"$mass"_13TeV-madgraph_MLM"
    mkdir $dir1
    cd $dir1
    
	#for card in customize_card extramodels proc_card run_card
	#do
	 #   touch VectorLikeLeptons_TAUprimeNUTAUprime$mode"_M-"$mass"_13TeV-madgraph_"$card".dat"	   
	#done
	customize_card=VectorLikeLeptons_M-$mass"_13TeV-madgraph_MLM_customize_card.dat"
	extramodels=VectorLikeLeptons_M-$mass"_13TeV-madgraph_MLM_extramodels.dat"
	proc_card=VectorLikeLeptons_M-$mass"_13TeV-madgraph_MLM_proc_card.dat"
	run_card=VectorLikeLeptons_M-$mass"_13TeV-madgraph_MLM_run_card.dat"	
	touch $customize_card
 	touch $extramodels
	touch $proc_card
	touch $run_card	
	echo "writing customize card for mass " $mass " in " $mode " mode "
	echo "set param_card mass 17 "$mass".0 #MTP" >> $customize_card 
	echo "set param_card mass 18 "$mass".0 #MVTP" >> $customize_card 
	
	echo "writing extramodels card for mass " $mass " in " $mode " mode "

	echo "VectorLikeLeptons_M-"$mass"_UFO.tgz" >> $extramodels
	
	echo "writing proc_card for mass " $mass " in " $mode " mode "

	echo "#************************************************************" >> $proc_card
	echo "#*                     MadGraph5_aMC@NLO                    *" >> $proc_card
	echo "#*                                                          *" >> $proc_card
	echo "#*                *                       *                 *" >> $proc_card
	echo "#*                  *        * *        *                   *" >> $proc_card
	echo "#*                    * * * * 5 * * * *                     *" >> $proc_card
	echo "#*                  *        * *        *                   *" >> $proc_card
	echo "#*                *                       *                 *" >> $proc_card
	echo "#*                                                          *" >> $proc_card
	echo "#*                                                          *" >> $proc_card
	echo "#*         VERSION 2.4.3                 2016-08-01         *" >> $proc_card
	echo "#*                                                          *" >> $proc_card
	echo "#*    The MadGraph5_aMC@NLO Development Team - Find us at   *" >> $proc_card
	echo "#*    https://server06.fynu.ucl.ac.be/projects/madgraph     *" >> $proc_card
	echo "#*                                                          *" >> $proc_card
	echo "#************************************************************" >> $proc_card
	echo "#*                                                          *" >> $proc_card
	echo "#*               Command File for MadGraph5_aMC@NLO         *" >> $proc_card
	echo "#*                                                          *" >> $proc_card
	echo "#*     run as ./bin/mg5_aMC  filename                       *" >> $proc_card
	echo "#*                                                          *" >> $proc_card
	echo "#************************************************************" >> $proc_card
        echo "set group_subprocesses Auto" >> $proc_card
	echo "set ignore_six_quark_processes False" >> $proc_card
	echo "set loop_optimized_output True" >> $proc_card
	echo "set complex_mass_scheme False" >> $proc_card
	echo "import model VectorLikeLeptons_M-"$mass"_UFO" >> $proc_card
	echo "# Define multiparticle labels" >> $proc_card
	echo "define p = g u c d s u~ c~ d~ s~" >> $proc_card
	echo "define j = g u c d s u~ c~ d~ s~" >> $proc_card
	echo "define l+ = e+ mu+ ta+" >> $proc_card
	echo "define l- = e- mu- ta-" >> $proc_card
	echo "define w = w+ w-" >> $proc_card
	echo "define l = l+ l-" >> $proc_card
	echo "define v = w+ w- z h" >> $proc_card
	echo "define tp = tp+ tp-" >> $proc_card
	echo "define vtpp = vtp vtp~" >> $proc_card
	echo "generate p p > tp tp, (tp > v l)" >> $proc_card
	echo "add process p p > vtpp vtpp, (vtpp > v l)" >> $proc_card
	echo "add process p p > tp vtpp, (tp > v l), (vtpp > v l)" >> $proc_card
	#echo "add process p p > tp tp j, (tp > v l)" >> $proc_card
	#echo "add process p p > vtpp vtpp j, (vtpp > v l)" >> $proc_card
	#echo "add process p p > tp vtpp j, (tp > v l), (vtpp > v l)" >> $proc_card
	#echo "add process p p > tp tp j j, (tp > v l)" >> $proc_card
	#echo "add process p p > vtpp vtpp j j, (vtpp > v l)" >> $proc_card
	#echo "add process p p > tp vtpp j j, (tp > v l), (vtpp > v l)" >> $proc_card
#
	echo "output VectorLikeLeptons_M-"$mass"_13TeV-madgraph_MLM -nojpeg" >> $proc_card
	echo "" >> $proc_card
	echo "# To generate events, you can go to the created directory and " >> $proc_card
	echo "# run ./bin/generate_events" >> $proc_card

	echo "writing run_card for mass " $mass " in " $mode " mode "
	
	echo "#*********************************************************************" >> $run_card
	echo "#                       MadGraph5_aMC@NLO                            *" >> $run_card
	echo "#                                                                    *" >> $run_card
	echo "#                     run_card.dat MadEvent                          *" >> $run_card
	echo "#                                                                    *" >> $run_card
	echo "#  This file is used to set the parameters of the run.               *" >> $run_card
	echo "#                                                                    *" >> $run_card
	echo "#  Some notation/conventions:                                        *" >> $run_card
	echo "#                                                                    *" >> $run_card
	echo "#   Lines starting with a '# ' are info or comments                  *" >> $run_card
	echo "#                                                                    *" >> $run_card
	echo "#   mind the format:   value    = variable     ! comment             *" >> $run_card
	echo "#*********************************************************************" >> $run_card
	echo "#" >> $run_card
	echo "#*******************                                                 " >> $run_card
	echo "# Running parameters" >> $run_card
	echo "#*******************                                                 " >> $run_card
	echo "#                                                                    " >> $run_card
	echo "#*********************************************************************" >> $run_card
	echo "# Tag name for the run (one word)                                    *" >> $run_card
	echo "#*********************************************************************" >> $run_card
	echo "  tag_1     = run_tag ! name of the run " >> $run_card
	echo "#*********************************************************************" >> $run_card
	echo "# Run to generate the grid pack                                      *" >> $run_card
	echo "#*********************************************************************" >> $run_card
	echo "  False     = gridpack  !True = setting up the grid pack" >> $run_card
	echo "#*********************************************************************" >> $run_card
	echo "# Number of events and rnd seed                                      *" >> $run_card
	echo "# Warning: Do not generate more than 1M events in a single run       *" >> $run_card
	echo "# If you want to run Pythia, avoid more than 50k events in a run.    *" >> $run_card
	echo "#*********************************************************************" >> $run_card
	echo "  100000 = nevents ! Number of unweighted events requested " >> $run_card
	echo "  0   = iseed   ! rnd seed (0=assigned automatically=default))" >> $run_card
	echo "#*********************************************************************" >> $run_card
	echo "# Collider type and energy                                           *" >> $run_card
	echo "# lpp: 0=No PDF, 1=proton, -1=antiproton, 2=photon from proton,      *" >> $run_card
	echo "#                                         3=photon from electron     *" >> $run_card
	echo "#*********************************************************************" >> $run_card
	echo "     1        = lpp1    ! beam 1 type " >> $run_card
	echo "     1        = lpp2    ! beam 2 type" >> $run_card
	echo "     6500.0     = ebeam1  ! beam 1 total energy in GeV" >> $run_card
	echo "     6500.0     = ebeam2  ! beam 2 total energy in GeV" >> $run_card
	echo "#*********************************************************************" >> $run_card
	echo "# Beam polarization from -100 (left-handed) to 100 (right-handed)    *" >> $run_card
	echo "#*********************************************************************" >> $run_card
	echo "     0.0     = polbeam1 ! beam polarization for beam 1" >> $run_card
	echo "     0.0     = polbeam2 ! beam polarization for beam 2" >> $run_card
	echo "#*********************************************************************" >> $run_card
	echo "# PDF CHOICE: this automatically fixes also alpha_s and its evol.    *" >> $run_card
	echo "#*********************************************************************" >> $run_card
	echo " lhapdf = pdlabel ! PDF set" >> $run_card
	echo ' $DEFAULT_PDF_SETS = lhaid' >> $run_card
	echo ' $DEFAULT_PDF_MEMBERS  = reweight_PDF' >> $run_card
#	echo "     nn23lo1    = pdlabel     ! PDF set                                     " >> $run_card
#	echo "     230000    = lhaid     ! if pdlabel=lhapdf, this is the lhapdf number" >> $run_card
	echo "#*********************************************************************" >> $run_card
	echo "# Renormalization and factorization scales                           *" >> $run_card
	echo "#*********************************************************************" >> $run_card
	echo " False = fixed_ren_scale  ! if .true. use fixed ren scale" >> $run_card
	echo " False        = fixed_fac_scale  ! if .true. use fixed fac scale" >> $run_card
	echo " 91.188  = scale            ! fixed ren scale" >> $run_card
	echo " 91.188  = dsqrt_q2fact1    ! fixed fact scale for pdf1" >> $run_card
	echo " 91.188  = dsqrt_q2fact2    ! fixed fact scale for pdf2" >> $run_card
	echo " -1 = dynamical_scale_choice ! Choose one of the preselected dynamical choices" >> $run_card
	echo " 1.0  = scalefact        ! scale factor for event-by-event scales" >> $run_card
	echo "#*********************************************************************" >> $run_card
	echo "# Time of flight information. (-1 means not run)" >> $run_card
	echo "#*********************************************************************" >> $run_card
	echo " -1.0 = time_of_flight ! threshold below which info is not written" >> $run_card
	echo "#*********************************************************************" >> $run_card
	echo "# Matching - Warning! ickkw > 1 is still beta" >> $run_card
	echo "#*********************************************************************" >> $run_card
	echo " 0 = ickkw            ! 0 no matching, 1 MLM, 2 CKKW matching" >> $run_card
	echo " 1 = highestmult      ! for ickkw=2, highest mult group" >> $run_card
	echo " 1 = ktscheme         ! for ickkw=1, 1 Durham kT, 2 Pythia pTE" >> $run_card
	echo " 1.0 = alpsfact         ! scale factor for QCD emission vx" >> $run_card
	echo " False = chcluster        ! cluster only according to channel diag" >> $run_card
	echo " True = pdfwgt           ! for ickkw=1, perform pdf reweighting" >> $run_card
	echo " 4 = asrwgtflavor     ! highest quark flavor for a_s reweight" >> $run_card
	echo " True = clusinfo         ! include clustering tag in output" >> $run_card
	echo " 3.0 = lhe_version       ! Change the way clustering information pass to shower.        " >> $run_card
	echo "#*********************************************************************" >> $run_card
	echo "#**********************************************************" >> $run_card
	echo "#" >> $run_card
	echo "#**********************************************************" >> $run_card
	echo "# Automatic ptj and mjj cuts if xqcut > 0" >> $run_card
	echo "# (turn off for VBF and single top processes)" >> $run_card
	echo "#**********************************************************" >> $run_card
	echo "   True  = auto_ptj_mjj  ! Automatic setting of ptj and mjj" >> $run_card
	echo "#**********************************************************" >> $run_card
	echo "#                                                                    " >> $run_card
	echo "#**********************************" >> $run_card
	echo "# BW cutoff (M+/-bwcutoff*Gamma)" >> $run_card
	echo "#**********************************" >> $run_card
	echo "  15.0  = bwcutoff      ! (M+/-bwcutoff*Gamma)" >> $run_card
	echo "#**********************************************************" >> $run_card
	echo "# Apply pt/E/eta/dr/mij/kt_durham cuts on decay products or not" >> $run_card
	echo "# (note that etmiss/ptll/ptheavy/ht/sorted cuts always apply)" >> $run_card
	echo "#*************************************************************" >> $run_card
	echo "   False  = cut_decays    ! Cut decay products " >> $run_card
	echo "#*************************************************************" >> $run_card
	echo "# Number of helicities to sum per event (0 = all helicities)" >> $run_card
	echo "# 0 gives more stable result, but longer run time (needed for" >> $run_card
	echo "# long decay chains e.g.)." >> $run_card
	echo "# Use >=2 if most helicities contribute, e.g. pure QCD." >> $run_card
	echo "#*************************************************************" >> $run_card
	echo "   0  = nhel          ! Number of helicities used per event" >> $run_card
	echo "#*******************                                                 " >> $run_card
	echo "# Standard Cuts" >> $run_card
	echo "#*******************                                                 " >> $run_card
	echo "#                                                                    " >> $run_card
	echo "#*********************************************************************" >> $run_card
	echo "# Minimum and maximum pt's (for max, -1 means no cut)                *" >> $run_card
	echo "#*********************************************************************" >> $run_card
	echo " 20.0  = ptj       ! minimum pt for the jets " >> $run_card
	echo " 0.0  = ptb       ! minimum pt for the b " >> $run_card
	echo " 10.0  = pta       ! minimum pt for the photons " >> $run_card
	echo " 10.0  = ptl       ! minimum pt for the charged leptons " >> $run_card
	echo " 0.0  = misset    ! minimum missing Et (sum of neutrino's momenta)" >> $run_card
	echo " 0.0  = ptheavy   ! minimum pt for one heavy final state" >> $run_card
	echo " -1.0  = ptjmax    ! maximum pt for the jets" >> $run_card
	echo " -1.0  = ptbmax    ! maximum pt for the b" >> $run_card
	echo " -1.0  = ptamax    ! maximum pt for the photons" >> $run_card
	echo " -1.0  = ptlmax    ! maximum pt for the charged leptons" >> $run_card
	echo " -1.0  = missetmax ! maximum missing Et (sum of neutrino's momenta)" >> $run_card
	echo "#*********************************************************************" >> $run_card
	echo "# Minimum and maximum E's (in the center of mass frame)              *" >> $run_card
	echo "#*********************************************************************" >> $run_card
	echo "  0.0  = ej     ! minimum E for the jets " >> $run_card
	echo "  0.0  = eb     ! minimum E for the b " >> $run_card
	echo "  0.0  = ea     ! minimum E for the photons " >> $run_card
	echo "  0.0  = el     ! minimum E for the charged leptons " >> $run_card
	echo "  -1.0   = ejmax ! maximum E for the jets" >> $run_card
	echo " -1.0   = ebmax ! maximum E for the b" >> $run_card
	echo " -1.0   = eamax ! maximum E for the photons" >> $run_card
	echo " -1.0   = elmax ! maximum E for the charged leptons" >> $run_card
	echo "#*********************************************************************" >> $run_card
	echo "# Maximum and minimum absolute rapidity (for max, -1 means no cut)   *" >> $run_card
	echo "#*********************************************************************" >> $run_card
	echo "  5.0 = etaj    ! max rap for the jets " >> $run_card
	echo "  -1.0  = etab    ! max rap for the b" >> $run_card
	echo " 2.5  = etaa    ! max rap for the photons " >> $run_card
	echo " 2.5  = etal    ! max rap for the charged leptons " >> $run_card
	echo " 0.0  = etajmin ! min rap for the jets" >> $run_card
	echo " 0.0  = etabmin ! min rap for the b" >> $run_card
	echo " 0.0  = etaamin ! min rap for the photons" >> $run_card
	echo " 0.0  = etalmin ! main rap for the charged leptons" >> $run_card
	echo "#*********************************************************************" >> $run_card
	echo "# Minimum and maximum DeltaR distance                                *" >> $run_card
	echo "#*********************************************************************" >> $run_card
	echo " 0.4 = drjj    ! min distance between jets " >> $run_card
	echo " 0.0   = drbb    ! min distance between b's " >> $run_card
	echo " 0.4 = drll    ! min distance between leptons " >> $run_card
	echo " 0.4 = draa    ! min distance between gammas " >> $run_card
	echo " 0.0   = drbj    ! min distance between b and jet " >> $run_card
	echo " 0.4 = draj    ! min distance between gamma and jet " >> $run_card
	echo " 0.4 = drjl    ! min distance between jet and lepton " >> $run_card
	echo " 0.0   = drab    ! min distance between gamma and b " >> $run_card
	echo " 0.0   = drbl    ! min distance between b and lepton " >> $run_card
	echo " 0.4 = dral    ! min distance between gamma and lepton " >> $run_card
	echo " -1.0  = drjjmax ! max distance between jets" >> $run_card
	echo " -1.0  = drbbmax ! max distance between b's" >> $run_card
	echo " -1.0  = drllmax ! max distance between leptons" >> $run_card
	echo " -1.0  = draamax ! max distance between gammas" >> $run_card
	echo " -1.0  = drbjmax ! max distance between b and jet" >> $run_card
	echo " -1.0  = drajmax ! max distance between gamma and jet" >> $run_card
	echo " -1.0  = drjlmax ! max distance between jet and lepton" >> $run_card
	echo " -1.0  = drabmax ! max distance between gamma and b" >> $run_card
	echo " -1.0  = drblmax ! max distance between b and lepton" >> $run_card
	echo " -1.0  = dralmax ! maxdistance between gamma and lepton" >> $run_card
	echo "#*********************************************************************" >> $run_card
	echo "# Minimum and maximum invariant mass for pairs                       *" >> $run_card
	echo "# WARNING: for four lepton final state mmll cut require to have      *" >> $run_card
	echo "#          different lepton masses for each flavor!                  *           " >> $run_card
	echo "#*********************************************************************" >> $run_card
	echo " 0.0   = mmjj    ! min invariant mass of a jet pair " >> $run_card
	echo " 0.0   = mmbb    ! min invariant mass of a b pair " >> $run_card
	echo " 0.0   = mmaa    ! min invariant mass of gamma gamma pair" >> $run_card
	echo " 0.0   = mmll    ! min invariant mass of l+l- (same flavour) lepton pair" >> $run_card
	echo " -1.0  = mmjjmax ! max invariant mass of a jet pair" >> $run_card
	echo " -1.0  = mmbbmax ! max invariant mass of a b pair" >> $run_card
	echo " -1.0  = mmaamax ! max invariant mass of gamma gamma pair" >> $run_card
	echo " -1.0  = mmllmax ! max invariant mass of l+l- (same flavour) lepton pair" >> $run_card
	echo "#*********************************************************************" >> $run_card
	echo "# Minimum and maximum invariant mass for all letpons                 *" >> $run_card
	echo "#*********************************************************************" >> $run_card
	echo " 0.0   = mmnl    ! min invariant mass for all letpons (l+- and vl) " >> $run_card
	echo " -1.0  = mmnlmax ! max invariant mass for all letpons (l+- and vl) " >> $run_card
	echo "#*********************************************************************" >> $run_card
	echo "# Minimum and maximum pt for 4-momenta sum of leptons                *" >> $run_card
	echo "#*********************************************************************" >> $run_card
	echo " 0.0   = ptllmin  ! Minimum pt for 4-momenta sum of leptons(l and vl)" >> $run_card
	echo " -1.0  = ptllmax  ! Maximum pt for 4-momenta sum of leptons(l and vl)" >> $run_card
	echo "#*********************************************************************" >> $run_card
	echo "# Inclusive cuts                                                     *" >> $run_card
	echo "#*********************************************************************" >> $run_card
	echo " 0.0  = xptj ! minimum pt for at least one jet  " >> $run_card
	echo " 0.0  = xptb ! minimum pt for at least one b " >> $run_card
	echo " 0.0  = xpta ! minimum pt for at least one photon " >> $run_card
	echo " 0.0  = xptl ! minimum pt for at least one charged lepton " >> $run_card
	echo "#*********************************************************************" >> $run_card
	echo "# Control the pt's of the jets sorted by pt                          *" >> $run_card
	echo "#*********************************************************************" >> $run_card
	echo " 0.0   = ptj1min ! minimum pt for the leading jet in pt" >> $run_card
	echo " 0.0   = ptj2min ! minimum pt for the second jet in pt" >> $run_card
	echo " 0.0   = ptj3min ! minimum pt for the third jet in pt" >> $run_card
	echo " 0.0   = ptj4min ! minimum pt for the fourth jet in pt" >> $run_card
	echo " -1.0  = ptj1max ! maximum pt for the leading jet in pt " >> $run_card
	echo " -1.0  = ptj2max ! maximum pt for the second jet in pt" >> $run_card
	echo " -1.0  = ptj3max ! maximum pt for the third jet in pt" >> $run_card
	echo " -1.0  = ptj4max ! maximum pt for the fourth jet in pt" >> $run_card
	echo " 0   = cutuse  ! reject event if fails any (0) / all (1) jet pt cuts" >> $run_card
	echo "#*********************************************************************" >> $run_card
	echo "# Control the pt's of leptons sorted by pt                           *" >> $run_card
	echo "#*********************************************************************" >> $run_card
	echo " 0.0   = ptl1min ! minimum pt for the leading lepton in pt" >> $run_card
	echo " 0.0   = ptl2min ! minimum pt for the second lepton in pt" >> $run_card
	echo " 0.0   = ptl3min ! minimum pt for the third lepton in pt" >> $run_card
	echo " 0.0   = ptl4min ! minimum pt for the fourth lepton in pt" >> $run_card
	echo " -1.0  = ptl1max ! maximum pt for the leading lepton in pt " >> $run_card
	echo " -1.0  = ptl2max ! maximum pt for the second lepton in pt" >> $run_card
	echo " -1.0  = ptl3max ! maximum pt for the third lepton in pt" >> $run_card
	echo " -1.0  = ptl4max ! maximum pt for the fourth lepton in pt" >> $run_card
	echo "#*********************************************************************" >> $run_card
	echo "# Control the Ht(k)=Sum of k leading jets                            *" >> $run_card
	echo "#*********************************************************************" >> $run_card
	echo " 0.0   = htjmin ! minimum jet HT=Sum(jet pt)" >> $run_card
	echo " -1.0  = htjmax ! maximum jet HT=Sum(jet pt)" >> $run_card
	echo " 0.0   = ihtmin  !inclusive Ht for all partons (including b)" >> $run_card
	echo " -1.0  = ihtmax  !inclusive Ht for all partons (including b)" >> $run_card
	echo " 0.0   = ht2min ! minimum Ht for the two leading jets" >> $run_card
	echo " 0.0   = ht3min ! minimum Ht for the three leading jets" >> $run_card
	echo " 0.0   = ht4min ! minimum Ht for the four leading jets" >> $run_card
	echo " -1.0  = ht2max ! maximum Ht for the two leading jets" >> $run_card
	echo " -1.0  = ht3max ! maximum Ht for the three leading jets" >> $run_card
	echo " -1.0  = ht4max ! maximum Ht for the four leading jets" >> $run_card
	echo "#***********************************************************************" >> $run_card
	echo "# Photon-isolation cuts, according to hep-ph/9801442                   *" >> $run_card
	echo "# When ptgmin=0, all the other parameters are ignored                  *" >> $run_card
	echo "# When ptgmin>0, pta and draj are not going to be used                 *" >> $run_card
	echo "#***********************************************************************" >> $run_card
	echo " 0.0 = ptgmin ! Min photon transverse momentum" >> $run_card
	echo " 0.4 = R0gamma ! Radius of isolation code" >> $run_card
	echo " 1.0 = xn ! n parameter of eq.(3.4) in hep-ph/9801442" >> $run_card
	echo " 1.0 = epsgamma ! epsilon_gamma parameter of eq.(3.4) in hep-ph/9801442" >> $run_card
	echo " True = isoEM ! isolate photons from EM energy (photons and leptons)" >> $run_card
	echo "#*********************************************************************" >> $run_card
	echo "# WBF cuts                                                           *" >> $run_card
	echo "#*********************************************************************" >> $run_card
	echo " 0.0   = xetamin ! minimum rapidity for two jets in the WBF case  " >> $run_card
	echo " 0.0   = deltaeta ! minimum rapidity for two jets in the WBF case " >> $run_card
	echo "#*********************************************************************" >> $run_card
	echo "# KT DURHAM CUT                                                      *" >> $run_card
	echo "#*********************************************************************" >> $run_card
	echo " -1.0    =  ktdurham        " >> $run_card
	echo " 0.4  =  dparameter " >> $run_card
	echo "#*********************************************************************" >> $run_card
	echo "# maximal pdg code for quark to be considered as a light jet         *" >> $run_card
	echo "# (otherwise b cuts are applied)                                     *" >> $run_card
	echo "#*********************************************************************" >> $run_card
	echo " 4 = maxjetflavor    ! Maximum jet pdg code" >> $run_card
	echo "#*********************************************************************" >> $run_card
	echo "# Jet measure cuts                                                   *" >> $run_card
	echo "#*********************************************************************" >> $run_card
	echo " 0.0   = xqcut   ! minimum kt jet measure between partons" >> $run_card
	echo "#*********************************************************************" >> $run_card
	echo "#" >> $run_card
	echo "#*********************************************************************" >> $run_card
	echo "# Store info for systematics studies                                 *" >> $run_card
	echo "# WARNING: If use_syst is T, matched Pythia output is                *" >> $run_card
	echo "#          meaningful ONLY if plotted taking matchscale              *" >> $run_card
	echo "#          reweighting into account!                                 *" >> $run_card
	echo "#*********************************************************************" >> $run_card
	echo "   True  = use_syst      ! Enable systematics studies" >> $run_card
	echo "#" >> $run_card
	echo "#**************************************" >> $run_card
	echo "# Parameter of the systematics study" >> $run_card
	echo "#  will be used by SysCalc (if installed)" >> $run_card
	echo "#**************************************                                  " >> $run_card
	echo "#" >> $run_card
	echo "0.5 1 2 = sys_scalefact  # factorization/renormalization scale factor" >> $run_card
	echo "None = sys_alpsfact  # \alpha_s emission scale factors" >> $run_card
	echo "30 50 = sys_matchscale # variation of merging scale" >> $run_card
	echo "# PDF sets and number of members (0 or none for all members)." >> $run_card
	echo "NNPDF23_lo_as_0130_qed = sys_pdf # matching scales" >> $run_card
	echo "# MSTW2008nlo68cl.LHgrid 1  = sys_pdf" >> $run_card	
	cd $code_dir
done 

MODULE ModParameters
implicit none
save
! 
! 
character(len=6),parameter :: JHUGen_Version="v5.2.5"
! 
! 
integer, public :: Collider, PDFSet,PChannel,Process,DecayMode1,DecayMode2,TopDecays
integer, public :: VegasIt1,VegasNc0,VegasNc1,VegasNc2
real(8), public :: Collider_Energy
integer, public :: VegasIt1_default,VegasNc0_default,VegasNc1_default,VegasNc2_default
real(8), public :: VegasSeed
integer, public :: NumHistograms
logical, public :: unweighted,OffShellReson,OffShellV1,OffShellV2,ReadLHEFile,ConvertLHEFile,ReadCSmax,GenerateEvents
integer(8), public :: EvalCounter=0
integer(8), public :: RejeCounter=0
integer(8), public :: AccepCounter=0
integer(8), public :: AlertCounter=0
integer(8), public :: AccepCounter_part(-5:5,-5:5)=0
real(8) :: time_start,time_end,time_int
logical, public :: warmup
character(len=100) :: DataFile
character(len=100) :: LogFile
character(len=500) :: LHEProdFile
logical, public :: includeInterference
real(8), public :: M_V,Ga_V
real(8), public, parameter :: GeV=1d0/100d0 ! we are using units of 100GeV, i.e. Lambda=10 is 1TeV 
real(8), public, parameter :: percent=1d0/100d0
real(8),public :: GlobalMax=-1d99
real(8),public :: GlobalMin=+1d99
integer,parameter :: NPart=200
real(8),public :: PartitionMax(0:NPart,0:NPart)=-1d99
real(8),public :: minCS=1d10,maxCS=0d0,avgCS=0d0


logical, public, parameter :: seed_random = .true.

logical, public, parameter :: fix_channels_ratio = .false.

real(8), public, parameter :: channels_ratio_fix = 0.25d0    ! desired ratio of  N_qq/(N_qq+N_gg)

logical, public, parameter :: importExternal_LHEinit = .true.

logical, public, parameter :: writeWeightedLHE = .false. 

logical, public, parameter :: includeGammaStar = .false. 

real(8),parameter :: MPhotonCutoff = 4d0*GeV

logical, public, parameter :: RandomizeVVdecays = .true. ! randomize DecayMode1 and DecayMode2 in H-->VV decays

real(8), public, parameter :: M_Top   = 173d0     *GeV      ! 
real(8), public, parameter :: Ga_Top  = 1.33d0    *GeV      ! 
real(8), public, parameter :: M_Z     = 91.1876d0 *GeV      ! Z boson mass (PDG-2011)
real(8), public, parameter :: Ga_Z    = 2.4952d0  *GeV      ! Z boson width(PDG-2011)
!real(8), public, parameter :: Ga_Z    = 2.5012d0  *GeV      
real(8), public, parameter :: M_W     = 80.399d0  *GeV      ! W boson mass (PDG-2011)
real(8), public, parameter :: Ga_W    = 2.085d0   *GeV      ! W boson width(PDG-2011)
real(8), public            :: M_Reso  = 125.6d0   *GeV      ! X resonance mass (spin 0, spin 1, spin 2)     (carefule: no longer a parameter, can be overwritten by command line argument)
real(8), public, parameter :: Ga_Reso = 0.00415d0 *GeV      ! X resonance width
real(8), public, parameter :: Lambda  = 1000d0    *GeV      ! Lambda coupling enters in two places
                                                            ! overal scale for x-section and in power suppressed
                                                            ! operators/formfactors (former r).

real(8), public, parameter :: m_el = 0.00051100d0  *GeV         ! electron mass
real(8), public, parameter :: m_mu = 0.10566d0     *GeV         ! muon mass
real(8), public, parameter :: m_tau = 1.7768d0     *GeV         ! tau mass
real(8), public, parameter :: m_bot = 4.2000d0     *GeV         ! bottom quark mass

real(8), public, parameter :: HiggsDecayLengthMM = 0d0      ! Higgs decay length in [mm]
real(8), public, parameter :: Gf = 1.16639d-5/GeV**2        ! fermi constant
real(8), public, parameter :: vev = 1.0d0/sqrt(Gf*sqrt(2.0d0))
real(8), public, parameter :: gwsq = 4.0d0 * M_W**2/vev**2  ! weak constant squared
real(8), public, parameter :: alpha_QED = 1d0/132.2319d0    ! el.magn. coupling
real(8), public, parameter :: alphas = 0.13229060d0         ! strong coupling
real(8), public, parameter :: sitW = dsqrt(0.23119d0)       ! sin(Theta_Weinberg) (PDG-2008)
!real(8), public, parameter :: sitW = dsqrt(0.222897209d0)      
real(8), public            :: Mu_Fact                       ! pdf factorization scale (set to M_Reso in main.F90)
real(8), public, parameter :: LHC_Energy=13000d0  *GeV      ! LHC hadronic center of mass energy
real(8), public, parameter :: TEV_Energy=1960d0  *GeV       ! Tevatron hadronic center of mass energy
real(8), public, parameter :: ILC_Energy=250d0  *GeV        ! Linear collider center of mass energy
real(8), public, parameter :: POL_A = 0d0                   !e+ polarization. 0: no polarization, 100: helicity = 1, -100: helicity = -1
real(8), public, parameter :: POL_B = 0d0                   !e- polarization. 0: no polarization, 100: helicity = 1, -100: helicity = -1
logical, public, parameter :: H_DK =.false.                 !default to false so H in V* > VH (Process = 50) does not decay
!logical, public, parameter :: V_DK =.true.                  !default to true so V in V* > VH (Process = 50) decays
real(8), public, parameter :: ptjetcut = 15d0*GeV           ! jet min pt
real(8), public, parameter :: Rjet = 0.5d0                  ! jet deltaR, antikt algorithm 

!----------------------------------------------------------------------------------------------------

! absolute branching fraction (taken from PDG-2014)
real(8), public, parameter :: Br_Z_ll   = 10.10d0*percent                             ! leptonic Z branching
real(8), public, parameter :: Br_Z_hadr = 69.91d0*percent                             ! hadronic Z branching
real(8), public, parameter :: Br_Z_inv  = 100d0*percent - Br_Z_ll - Br_Z_hadr         ! invisible Z branching
real(8), public, parameter :: Br_Z_uu   = 11.6d0*percent                              ! up upbar Z branching
real(8), public, parameter :: Br_Z_cc   = 11.6d0*percent                              ! chm chmbar Z branching
real(8), public, parameter :: Br_Z_dd   = 15.6d0*percent                              ! dn dnbar Z branching
real(8), public, parameter :: Br_Z_ss   = 15.6d0*percent                              ! str strbar Z branching
real(8), public, parameter :: Br_Z_bb   = Br_Z_hadr - Br_Z_uu - Br_Z_dd - Br_Z_cc - Br_Z_ss
real(8), public, parameter :: Br_W_ll   = 32.72d0*percent                             ! leptonic W branching
real(8), public, parameter :: Br_W_hadr = 100d0*percent - Br_W_ll                     ! hadronic W branching


! derived branching fractions (assuming CKM=1)
real(8), public, parameter :: Br_Z_ee   = 1d0/3d0*Br_Z_ll                             ! electron Z branching
real(8), public, parameter :: Br_Z_mm   = 1d0/3d0*Br_Z_ll                             ! muon Z branching
real(8), public, parameter :: Br_Z_tt   = 1d0/3d0*Br_Z_ll                             ! tau Z branching
real(8), public, parameter :: Br_Z_nn   = 1d0/3d0*Br_Z_inv                            ! neutrino Z branching
real(8), public, parameter :: Br_W_en   = 1d0/3d0*Br_W_ll                             ! electron W branching
real(8), public, parameter :: Br_W_mn   = 1d0/3d0*Br_W_ll                             ! muon W branching
real(8), public, parameter :: Br_W_tn   = 1d0/3d0*Br_W_ll                             ! electron W branching
real(8), public, parameter :: Br_W_ud   = 1d0/2d0*Br_W_hadr                           ! u-d W branching
real(8), public, parameter :: Br_W_cs   = 1d0/2d0*Br_W_hadr                           ! c-s W branching

real(8), public, parameter :: Brlept_Z_ee = Br_Z_ee/Br_Z_ll                           ! Z branching fraction Ga(el)/Ga(leptonic)
real(8), public, parameter :: Brlept_Z_mm = Br_Z_mm/Br_Z_ll                           ! Z branching fraction Ga(mu)/Ga(leptonic)
real(8), public, parameter :: Brlept_Z_tt = Br_Z_tt/Br_Z_ll                           ! Z branching fraction Ga(tau)/Ga(leptonic)
real(8), public, parameter :: Brlept_Z_nn = Br_Z_nn/Br_Z_inv                          ! Z branching fraction Ga(neu)/Ga(invisible)
real(8), public, parameter :: Brlept_W_en = Br_W_en/Br_W_ll                           ! W branching fraction Ga(el)/Ga(leptonic)
real(8), public, parameter :: Brlept_W_mn = Br_W_mn/Br_W_ll                           ! W branching fraction Ga(mu)/Ga(leptonic)
real(8), public, parameter :: Brlept_W_tn = Br_W_tn/Br_W_ll                           ! W branching fraction Ga(tau)/Ga(leptonic)

real(8), public, parameter :: Brhadr_Z_uu = Br_Z_uu/Br_Z_hadr                         ! Z branching fraction Ga(up)/Ga(hadronic)
real(8), public, parameter :: Brhadr_Z_cc = Br_Z_cc/Br_Z_hadr                         ! Z branching fraction Ga(chm)/Ga(hadronic)
real(8), public, parameter :: Brhadr_Z_dd = Br_Z_dd/Br_Z_hadr                         ! Z branching fraction Ga(don)/Ga(hadronic)
real(8), public, parameter :: Brhadr_Z_ss = Br_Z_ss/Br_Z_hadr                         ! Z branching fraction Ga(str)/Ga(hadronic)
real(8), public, parameter :: Brhadr_Z_bb = Br_Z_bb/Br_Z_hadr                         ! Z branching fraction Ga(bot)/Ga(hadronic)
real(8), public, parameter :: Brhadr_W_ud = Br_W_ud/Br_W_hadr                         ! W branching fraction Ga(up)/Ga(hadronic)
real(8), public, parameter :: Brhadr_W_cs = Br_W_cs/Br_W_hadr                         ! W branching fraction Ga(chm)/Ga(hadronic)

real(8), public :: scale_alpha_Z_uu = 1.037560d0 ! scaling factor of alpha (~partial width) for Z > u u~, c c~
real(8), public :: scale_alpha_Z_dd = 1.037560d0 ! scaling factor of alpha (~partial width) for Z > d d~, s s~, b b~
real(8), public :: scale_alpha_Z_ll = 1d0 ! scaling factor of alpha (~partial width) for Z > l+ l-  (l=e,mu)
real(8), public :: scale_alpha_Z_tt = 1d0 ! scaling factor of alpha (~partial width) for Z > tau+ tau-
real(8), public :: scale_alpha_Z_nn = 1d0 ! scaling factor of alpha (~partial width) for Z > nu nu~
real(8), public :: scale_alpha_W_ud = 1.038200d0 ! scaling factor of alpha (~partial width) for W > u d
real(8), public :: scale_alpha_W_cs = 1.038200d0 ! scaling factor of alpha (~partial width) for W > c s
real(8), public :: scale_alpha_W_ln = 1d0 ! scaling factor of alpha (~partial width) for W > l nu (l=e,mu)
real(8), public :: scale_alpha_W_tn = 1d0 ! scaling factor of alpha (~partial width) for W > tau nu
! real(8), public, parameter :: scale_alpha_Z_ll = (1d0-(Br_Z_uu+Br_Z_cc)*scale_alpha_Z_uu-(Br_Z_dd+Br_Z_ss+Br_Z_bb)*scale_alpha_Z_dd)/(3d0*Br_Z_nn+3d0*Br_Z_ee) ! scaling factor of alpha (~partial width) for Z > l+ l- which restores the total width
! real(8), public, parameter :: scale_alpha_Z_nn = scale_alpha_Z_ll ! scaling factor of alpha (~partial width) for Z > nu nu~ which restores total width
! real(8), public, parameter :: scale_alpha_W_ln = (1d0-Br_W_ud*scale_alpha_W_ud-Br_W_cs*scale_alpha_W_cs)/(3d0*Br_W_en) ! scaling factor of alpha (~partial width) for W > l nu
!
! sum rules
! 1 = 3*Br_Z_nn + 3*Br_Z_ee + 2*Br_Z_uu + 3*Br_Z_dd
!
! sum rule with scaling factors
! 1 = 3*(Br_Z_nn*scale_alpha_Z_nn) + 3*(Br_Z_ee*scale_alpha_Z_ll) + 2*(Br_Z_uu*scale_alpha_Z_uu) + 3*(Br_Z_dd*scale_alpha_Z_dd)
integer, public :: Br_Z_ll_counter=0
integer, public :: Br_Z_inv_counter=0
integer, public :: Br_Z_uu_counter=0
integer, public :: Br_Z_dd_counter=0
integer, public :: Br_W_ll_counter=0
integer, public :: Br_W_ud_counter=0
integer, public :: Br_counter(1:5,1:5)=0

!-- parameters that define on-shell spin 0 coupling to SM fields, see note
   logical, public, parameter :: generate_as = .false.
   complex(8), public, parameter :: ahg1 = (0.0d0,0d0)
   complex(8), public, parameter :: ahg2 = (0.0d0,0d0)
   complex(8), public, parameter :: ahg3 = (0.0d0,0d0)  ! pseudoscalar
   complex(8), public, parameter :: ahz1 = (0.0d0,0d0)
   complex(8), public, parameter :: ahz2 = (0.0d0,0d0)  ! this coupling does not contribute for gamma+gamma final states
   complex(8), public, parameter :: ahz3 = (0.0d0,0d0)  ! pseudoscalar

!-- parameters that define off-shell spin 0 coupling to SM fields, see note
   complex(8), public, parameter :: ghg2 = (1.0d0,0d0)
   complex(8), public, parameter :: ghg3 = (0.0d0,0d0)
   complex(8), public, parameter :: ghg4 = (0.0d0,0d0)   ! pseudoscalar
   complex(8), public, parameter :: ghz1 = (2.0d0,0d0)   ! SM=2
   complex(8), public, parameter :: ghz2 = (0.0d0,0d0)
   complex(8), public, parameter :: ghz3 = (0.0d0,0d0)
   complex(8), public, parameter :: ghz4 = (0.0d0,0d0)   ! pseudoscalar 

   complex(8), public, parameter :: ghzgs2  = (0.00d0,0d0)
   complex(8), public, parameter :: ghzgs3  = (0.00d0,0d0)
   complex(8), public, parameter :: ghzgs4  = (0.00d0,0d0)
   complex(8), public, parameter :: ghgsgs2 = (0.00d0,0d0)
   complex(8), public, parameter :: ghgsgs3 = (0.00d0,0d0)
   complex(8), public, parameter :: ghgsgs4 = (0.00d0,0d0)


!-- parameters that define q^2 dependent form factors
   complex(8), public, parameter :: ghz1_prime = (0.0d0,0d0)
   complex(8), public, parameter :: ghz1_prime2= (0.0d0,0d0)
   complex(8), public, parameter :: ghz1_prime3= (0.0d0,0d0)
   complex(8), public, parameter :: ghz1_prime4= (0.0d0,0d0)
   complex(8), public, parameter :: ghz1_prime5= (0.0d0,0d0)
   complex(8), public, parameter :: ghz1_prime6= (0.0d0,0d0)
   complex(8), public, parameter :: ghz1_prime7= (0.0d0,0d0)

   complex(8), public, parameter :: ghz2_prime = (0.0d0,0d0)
   complex(8), public, parameter :: ghz2_prime2= (0.0d0,0d0)
   complex(8), public, parameter :: ghz2_prime3= (0.0d0,0d0)
   complex(8), public, parameter :: ghz2_prime4= (0.0d0,0d0)
   complex(8), public, parameter :: ghz2_prime5= (0.0d0,0d0)
   complex(8), public, parameter :: ghz2_prime6= (0.0d0,0d0)
   complex(8), public, parameter :: ghz2_prime7= (0.0d0,0d0)

   complex(8), public, parameter :: ghz3_prime = (0.0d0,0d0)
   complex(8), public, parameter :: ghz3_prime2= (0.0d0,0d0)
   complex(8), public, parameter :: ghz3_prime3= (0.0d0,0d0)
   complex(8), public, parameter :: ghz3_prime4= (0.0d0,0d0)
   complex(8), public, parameter :: ghz3_prime5= (0.0d0,0d0)
   complex(8), public, parameter :: ghz3_prime6= (0.0d0,0d0)
   complex(8), public, parameter :: ghz3_prime7= (0.0d0,0d0)

   complex(8), public, parameter :: ghz4_prime = (0.0d0,0d0)
   complex(8), public, parameter :: ghz4_prime2= (0.0d0,0d0)
   complex(8), public, parameter :: ghz4_prime3= (0.0d0,0d0)
   complex(8), public, parameter :: ghz4_prime4= (0.0d0,0d0)
   complex(8), public, parameter :: ghz4_prime5= (0.0d0,0d0)
   complex(8), public, parameter :: ghz4_prime6= (0.0d0,0d0)
   complex(8), public, parameter :: ghz4_prime7= (0.0d0,0d0)
   complex(8), public, parameter :: ghzgs1_prime2= (0.0d0,0d0)
   
   real(8),    public, parameter :: Lambda_z1 = 10000d0*GeV
   real(8),    public, parameter :: Lambda_z2 = 10000d0*GeV
   real(8),    public, parameter :: Lambda_z3 = 10000d0*GeV
   real(8),    public, parameter :: Lambda_z4 = 10000d0*GeV
   real(8),    public, parameter :: Lambda_z5 = 10000d0*GeV
   real(8),    public, parameter :: Lambda_Q  = 10000d0*GeV


!---parameters that define spin 1 coupling to SM fields, see note
   complex(8), public, parameter :: zprime_qq_left  = (1.0d0,0d0)
   complex(8), public, parameter :: zprime_qq_right = (1.0d0,0d0)
   complex(8), public, parameter :: zprime_zz_1 =  (1.0d0,0d0)!  =1 for JP=1- vector
   complex(8), public, parameter :: zprime_zz_2 =  (0.0d0,0d0)!  =1 for JP=1+ pseudovector

!-- parameters that define spin 2 coupling to SM fields, see note
! minimal coupling corresponds to a1 = b1 = b5 = 1 everything else 0
  complex(8), public, parameter :: a1 = (0.0d0,0d0)    ! g1  -- c.f. draft
  complex(8), public, parameter :: a2 = (1.0d0,0d0)    ! g2
  complex(8), public, parameter :: a3 = (0.0d0,0d0)    ! g3
  complex(8), public, parameter :: a4 = (0.0d0,0d0)    ! g4
  complex(8), public, parameter :: a5 = (0.0d0,0d0)    ! pseudoscalar, g8
  complex(8), public, parameter :: graviton_qq_left  = (1.0d0,0d0)! graviton coupling to quarks
  complex(8), public, parameter :: graviton_qq_right = (1.0d0,0d0)

!-- see mod_Graviton
  logical, public, parameter :: generate_bis = .true.
  logical, public, parameter :: use_dynamic_MG = .true.

  complex(8), public, parameter :: b1 = (0.0d0,0d0)    !  all b' below are g's in the draft
  complex(8), public, parameter :: b2 = (1.0d0,0d0)
  complex(8), public, parameter :: b3 = (0.0d0,0d0)
  complex(8), public, parameter :: b4 = (0.0d0,0d0)
  complex(8), public, parameter :: b5 = (0.0d0,0d0)
  complex(8), public, parameter :: b6 = (0.0d0,0d0)
  complex(8), public, parameter :: b7 = (0.0d0,0d0)
  complex(8), public, parameter :: b8 = (0.0d0,0d0)
  complex(8), public, parameter :: b9 = (0.0d0,0d0)  ! this coupling does not contribute to gamma+gamma final states
  complex(8), public, parameter :: b10 =(0.0d0,0d0)  ! this coupling does not contribute to gamma+gamma final states


  complex(8), public, parameter  :: c1 = (1.0d0,0d0)
  complex(8), public, parameter  :: c2 = (0.0d0,0d0)
  complex(8), public, parameter  :: c3 = (0.0d0,0d0)
  complex(8), public, parameter  :: c41= (0.0d0,0d0)
  complex(8), public, parameter  :: c42= (0.0d0,0d0)
  complex(8), public, parameter  :: c5 = (0.0d0,0d0)
  complex(8), public, parameter  :: c6 = (0.0d0,0d0)  ! this coupling does not contribute to gamma+gamma final states
  complex(8), public, parameter  :: c7 = (0.0d0,0d0)  ! this coupling does not contribute to gamma+gamma final states





!-- extra couplings for weak boson fusion when WW-spin-0 couplings are required to be different from ZZ-spin-0
!-- note: if all couplings below are set to zero, WW-spin-0 and ZZ-spin-0 couplings are assumed to be equal
   complex(8), public, parameter :: ghw1 = (0.0d0,0d0)
   complex(8), public, parameter :: ghw2 = (0.0d0,0d0)
   complex(8), public, parameter :: ghw3 = (0.0d0,0d0)
   complex(8), public, parameter :: ghw4 = (0.0d0,0d0)

!-- parameters that define q^2 dependent form factors in WBF WW-spin-0 case described above
   complex(8), public, parameter :: ghw1_prime = (0.0d0,0d0)
   complex(8), public, parameter :: ghw1_prime2= (0.0d0,0d0)
   complex(8), public, parameter :: ghw1_prime3= (0.0d0,0d0)
   complex(8), public, parameter :: ghw1_prime4= (0.0d0,0d0)
   complex(8), public, parameter :: ghw1_prime5= (0.0d0,0d0)
   complex(8), public, parameter :: ghw1_prime6= (0.0d0,0d0)
   complex(8), public, parameter :: ghw1_prime7= (0.0d0,0d0)

   complex(8), public, parameter :: ghw2_prime = (0.0d0,0d0)
   complex(8), public, parameter :: ghw2_prime2= (0.0d0,0d0)
   complex(8), public, parameter :: ghw2_prime3= (0.0d0,0d0)
   complex(8), public, parameter :: ghw2_prime4= (0.0d0,0d0)
   complex(8), public, parameter :: ghw2_prime5= (0.0d0,0d0)
   complex(8), public, parameter :: ghw2_prime6= (0.0d0,0d0)
   complex(8), public, parameter :: ghw2_prime7= (0.0d0,0d0)

   complex(8), public, parameter :: ghw3_prime = (0.0d0,0d0)
   complex(8), public, parameter :: ghw3_prime2= (0.0d0,0d0)
   complex(8), public, parameter :: ghw3_prime3= (0.0d0,0d0)
   complex(8), public, parameter :: ghw3_prime4= (0.0d0,0d0)
   complex(8), public, parameter :: ghw3_prime5= (0.0d0,0d0)
   complex(8), public, parameter :: ghw3_prime6= (0.0d0,0d0)
   complex(8), public, parameter :: ghw3_prime7= (0.0d0,0d0)

   complex(8), public, parameter :: ghw4_prime = (0.0d0,0d0)
   complex(8), public, parameter :: ghw4_prime2= (0.0d0,0d0)
   complex(8), public, parameter :: ghw4_prime3= (0.0d0,0d0)
   complex(8), public, parameter :: ghw4_prime4= (0.0d0,0d0)
   complex(8), public, parameter :: ghw4_prime5= (0.0d0,0d0)
   complex(8), public, parameter :: ghw4_prime6= (0.0d0,0d0)
   complex(8), public, parameter :: ghw4_prime7= (0.0d0,0d0)
   
   real(8),    public, parameter :: Lambda_w1 = 10000d0*GeV
   real(8),    public, parameter :: Lambda_w2 = 10000d0*GeV
   real(8),    public, parameter :: Lambda_w3 = 10000d0*GeV
   real(8),    public, parameter :: Lambda_w4 = 10000d0*GeV
   real(8),    public, parameter :: Lambda_w5 = 10000d0*GeV

!  couplings for ttbar+H
   complex(8),    public, parameter :: kappa       = (1d0,0d0)
   complex(8),    public, parameter :: kappa_tilde = (0d0,0d0) 
   complex(8),    public, parameter :: couplHTT_right_dyn = m_top/vev/2d0 * ( kappa + (0d0,1d0)*kappa_tilde )
   complex(8),    public, parameter :: couplHTT_left_dyn  = m_top/vev/2d0 * ( kappa - (0d0,1d0)*kappa_tilde )


! V-f-fbar couplings:
!   g_R(f) = -e*sw/cw*Q(f)                 = e/2/sw/cw * a(b,c)R,
!   g_L(f) = -e*sw/cw*Q(f) + e/sw/cw*T3(f) = e/2/sw/cw * a(b,c)L
! with
!   aR(f) = -2*sw**2*Q(f),
!   aL(f) = -2*sw**2*Q(f) + 2*T3(f).
! for V = Z-boson
! and
!   bR = 0
!   bL = dsqrt(2)*cw
! for V = W-boson
! and
!   cR = -2*sw*cw*Q(f)
!   cL = -2*sw*cw*Q(f)
! for V = photon*
!
!
real(8), public, parameter :: aR_lep =-2d0*sitW**2*(-1d0)
real(8), public, parameter :: aL_lep =-2d0*sitW**2*(-1d0)-1d0
real(8), public, parameter :: aR_neu =-2d0*sitW**2*(0d0)
real(8), public, parameter :: aL_neu =-2d0*sitW**2*(0d0)+1d0
real(8), public, parameter :: aR_QUp =-2d0*sitW**2*(2d0/3d0)
real(8), public, parameter :: aL_QUp =-2d0*sitW**2*(2d0/3d0)+1d0
real(8), public, parameter :: aR_QDn =-2d0*sitW**2*(-1d0/3d0)
real(8), public, parameter :: aL_QDn =-2d0*sitW**2*(-1d0/3d0)-1d0

real(8), public, parameter :: bL = dsqrt(2d0)*dsqrt(1d0-sitW**2)
real(8), public, parameter :: bR = 0d0

real(8), public, parameter :: cR_lep = -2d0*sitW*dsqrt(1d0-sitW**2)*(-1d0)
real(8), public, parameter :: cL_lep = -2d0*sitW*dsqrt(1d0-sitW**2)*(-1d0)
real(8), public, parameter :: cR_neu = -2d0*sitW*dsqrt(1d0-sitW**2)*(0d0)
real(8), public, parameter :: cL_neu = -2d0*sitW*dsqrt(1d0-sitW**2)*(0d0)
real(8), public, parameter :: cR_QUp = -2d0*sitW*dsqrt(1d0-sitW**2)*(2d0/3d0)
real(8), public, parameter :: cL_QUp = -2d0*sitW*dsqrt(1d0-sitW**2)*(2d0/3d0)
real(8), public, parameter :: cR_QDn = -2d0*sitW*dsqrt(1d0-sitW**2)*(-1d0/3d0)
real(8), public, parameter :: cL_QDn = -2d0*sitW*dsqrt(1d0-sitW**2)*(-1d0/3d0)

real(8), public, parameter :: fbGeV2=0.389379d12/(100d0**2)
real(8), public, parameter :: SymmFac=1d0/2d0, SpinAvg=1d0/4d0, QuarkColAvg=1d0/3d0, GluonColAvg=1d0/8d0
integer, public, target :: Up_  = 1
integer, public, target :: Dn_  = 2
integer, public, target :: Chm_ = 3
integer, public, target :: Str_ = 4
integer, public, target :: Top_ = 5
integer, public, target :: Bot_ = 6
integer, public, target :: ElP_ = 7
integer, public, target :: MuP_ = 8
integer, public, target :: TaP_ = 9
integer, public, target :: Glu_ = 10
integer, public, target :: Pho_ = 11
integer, public, target :: Z0_  = 12
integer, public, target :: Wp_  = 13
integer, public, target :: NuE_ = 14
integer, public, target :: NuM_ = 15
integer, public, target :: NuT_ = 16
integer, public, target :: Hig_ = 25
integer, public, target :: Zpr_ = 32
integer, public, target :: Gra_ = 39

integer, public, target :: AUp_  = -1
integer, public, target :: ADn_  = -2
integer, public, target :: AChm_ = -3
integer, public, target :: AStr_ = -4
integer, public, target :: ATop_ = -5
integer, public, target :: ABot_ = -6
integer, public, target :: ElM_  = -7
integer, public, target :: MuM_  = -8
integer, public, target :: TaM_  = -9
integer, public, target :: Wm_   = -13
integer, public, target :: ANuE_ = -14
integer, public, target :: ANuM_ = -15
integer, public, target :: ANuT_ = -16


real(8), public, parameter :: pi =3.141592653589793238462643383279502884197d0
real(8), public, parameter :: sqrt2 = 1.4142135623730950488016887242096980786d0
real(8), public, parameter :: pisq = pi**2
real(8), public, parameter :: one = 1.0d0, mone = -1.0d0
real(8), public, parameter :: half  = 0.5d0,two = 2.0d0
real(8), public, parameter :: zero  = 0.0d0
complex(8), parameter, public :: czero = (0.0d0,0.0d0) 
complex(8), parameter, public :: cone = 1.0d0
complex(8), parameter, public :: ci=(0.0d0,1.0d0)
complex(8), parameter, public :: ne=(0.0d0,1.0d0)

integer,parameter :: io_stdout=6
integer,parameter :: io_LHEOutFile=14
integer,parameter :: io_HistoFile=15
integer,parameter :: io_LHEInFile=16
integer,parameter :: io_LogFile=17
integer,parameter :: io_CSmaxFile=18
integer,parameter :: io_LHEOutFile2=19
integer,parameter :: io_LHEOutFile3=20

integer, public :: DebugCounter(0:10) = 0





CONTAINS



!--YaofuZhou
FUNCTION CKM(id1in,id2in)
implicit none
real(8) :: CKM
integer :: id1, id2, id1in, id2in
id1 = abs(id1in)
id2 = abs(id2in)
if((id1.eq.convertLHE(Up_)  .and.  id2.eq.convertLHE(Dn_))  .or.  (id1.eq.convertLHE(Dn_)  .and.  id2.eq.convertLHE(Up_)))then
  CKM= 0.97427d0 * dsqrt(scale_alpha_W_ud)
elseif((id1.eq.convertLHE(Up_)  .and.  id2.eq.convertLHE(Str_))  .or.  (id1.eq.convertLHE(Str_)  .and.  id2.eq.convertLHE(Up_)))then
  CKM= 0.22534d0
elseif((id1.eq.convertLHE(Up_)  .and.  id2.eq.convertLHE(Bot_))  .or.  (id1.eq.convertLHE(Bot_)  .and.  id2.eq.convertLHE(Up_)))then
  CKM= 0.00351d0
elseif((id1.eq.convertLHE(Chm_)  .and.  id2.eq.convertLHE(Dn_))  .or.  (id1.eq.convertLHE(Dn_)  .and.  id2.eq.convertLHE(Chm_)))then
  CKM= 0.22520d0
elseif((id1.eq.convertLHE(Chm_)  .and.  id2.eq.convertLHE(Str_))  .or.  (id1.eq.convertLHE(Str_)  .and.  id2.eq.convertLHE(Chm_)))then
  CKM= 0.97344d0 * dsqrt(scale_alpha_W_cs)
elseif((id1.eq.convertLHE(Chm_)  .and.  id2.eq.convertLHE(Bot_))  .or.  (id1.eq.convertLHE(Bot_)  .and.  id2.eq.convertLHE(Chm_)))then
  CKM= 0.0412d0
!elseif((id1.eq.convertLHE(Top_)  .and.  id2.eq.convertLHE(Dn_))  .or.  (id1.eq.convertLHE(Dn_)  .and.  id2.eq.convertLHE(Top_)))then
!  CKM= 0.22520d0
!elseif((id1.eq.convertLHE(Top_)  .and.  id2.eq.convertLHE(Str_))  .or.  (id1.eq.convertLHE(Str_)  .and.  id2.eq.convertLHE(Top_)))then
!  CKM= 0.0404d0
!elseif((id1.eq.convertLHE(Top_)  .and.  id2.eq.convertLHE(Bot_))  .or.  (id1.eq.convertLHE(Bot_)  .and.  id2.eq.convertLHE(Top_)))then
!  CKM= 0.999146
elseif((abs(id1).eq.abs(convertLHE(NuT_))  .and.  abs(id2).eq.abs(convertLHE(TaP_)))  .or.  (abs(id1).eq.abs(convertLHE(TaP_))  .and.  abs(id2).eq.abs(convertLHE(NuT_))))then
  CKM= 1d0 * dsqrt(scale_alpha_W_tn)
else
  CKM= 1d0 * dsqrt(scale_alpha_W_ln)
endif

END FUNCTION




FUNCTION convertLHEreverse(Part)
implicit none
integer :: convertLHEreverse
integer :: Part

  if(     Part.eq.1 ) then
      convertLHEreverse = Dn_
  elseif( Part.eq.2 ) then
      convertLHEreverse = Up_
  elseif( Part.eq.3 ) then
      convertLHEreverse = Str_
  elseif( Part.eq.4 ) then
      convertLHEreverse = Chm_
  elseif( Part.eq.5 ) then
      convertLHEreverse = Bot_
  elseif( Part.eq.6 ) then
      convertLHEreverse = Top_
  elseif( Part.eq.-1 ) then
      convertLHEreverse = ADn_
  elseif( Part.eq.-2 ) then
      convertLHEreverse = AUp_
  elseif( Part.eq.-3 ) then
      convertLHEreverse = AStr_
  elseif( Part.eq.-4 ) then
      convertLHEreverse = AChm_
  elseif( Part.eq.-5 ) then
      convertLHEreverse = ABot_
  elseif( Part.eq.-6 ) then
      convertLHEreverse = ATop_
  elseif( Part.eq.21 ) then
      convertLHEreverse = Glu_
  elseif( Part.eq.11 ) then
      convertLHEreverse = ElM_
  elseif( Part.eq.22 ) then
      convertLHEreverse = Pho_
  elseif( Part.eq.23 ) then
      convertLHEreverse = Z0_
  elseif( Part.eq.24 ) then
      convertLHEreverse = Wp_
  elseif( Part.eq.-24 ) then
      convertLHEreverse = Wm_
  elseif( Part.eq.-11 ) then
      convertLHEreverse = ElP_
  elseif( Part.eq.13 ) then
      convertLHEreverse = MuM_
  elseif( Part.eq.-13 ) then
      convertLHEreverse = MuP_
  elseif( Part.eq.15 ) then
      convertLHEreverse = TaM_
  elseif( Part.eq.-15 ) then
      convertLHEreverse = TaP_
  elseif( Part.eq.12 ) then
      convertLHEreverse = NuE_
  elseif( Part.eq.-12) then
      convertLHEreverse = ANuE_
  elseif( Part.eq.14) then
      convertLHEreverse = NuM_
  elseif( Part.eq.-14) then
      convertLHEreverse = ANuM_
  elseif( Part.eq.16 ) then
      convertLHEreverse = NuT_
  elseif( Part.eq.-16) then
      convertLHEreverse = ANuT_
  elseif( Part.eq.+25) then
      convertLHEreverse = Hig_
  elseif( Part.eq.-25) then
      convertLHEreverse = Hig_
  else
      print *, "MYLHE format not implemented for ",Part
      stop
  endif


END FUNCTION



FUNCTION convertLHE(Part)
implicit none
integer :: convertLHE
integer :: Part


  if(     Part.eq.0 ) then
      convertLHE = 0
  elseif( Part.eq.Glu_ ) then
      convertLHE = 21
  elseif( Part.eq.ElM_ ) then
      convertLHE = 11
  elseif( Part.eq.ElP_ ) then
      convertLHE =-11
  elseif( Part.eq.MuM_ ) then
      convertLHE = 13
  elseif( Part.eq.MuP_ ) then
      convertLHE =-13
  elseif( Part.eq.TaM_ ) then
      convertLHE = 15
  elseif( Part.eq.TaP_ ) then
      convertLHE =-15
  elseif( Part.eq.NuE_ ) then
      convertLHE = 12
  elseif( Part.eq.ANuE_) then
      convertLHE =-12
  elseif( Part.eq.NuM_) then
      convertLHE = 14
  elseif( Part.eq.ANuM_) then
      convertLHE =-14
  elseif( Part.eq.NuT_ ) then
      convertLHE = 16
  elseif( Part.eq.ANuT_) then
      convertLHE =-16
  elseif( Part.eq.Up_  ) then
      convertLHE = 2
  elseif( Part.eq.AUp_ ) then
      convertLHE =-2
  elseif( Part.eq.Dn_  ) then
      convertLHE = 1
  elseif( Part.eq.ADn_ ) then
      convertLHE =-1
  elseif( Part.eq.Chm_ ) then
      convertLHE = 4
  elseif( Part.eq.AChm_) then
      convertLHE =-4
  elseif( Part.eq.Str_ ) then
      convertLHE = 3
  elseif( Part.eq.AStr_) then
      convertLHE =-3
  elseif( Part.eq.Bot_ ) then
      convertLHE = 5
  elseif( Part.eq.ABot_) then
      convertLHE =-5
  elseif( Part.eq.Top_ ) then
      convertLHE = 6
  elseif( Part.eq.ATop_) then
      convertLHE =-6
  elseif( Part.eq.Z0_) then
      convertLHE =23
  elseif( Part.eq.Wp_) then
      convertLHE =24
  elseif( Part.eq.Wm_) then
      convertLHE =-24
  elseif( Part.eq.Pho_) then
      convertLHE =22
  elseif( Part.eq.Hig_) then
      convertLHE =25
  elseif( Part.eq.Zpr_) then
      convertLHE =32
  elseif( Part.eq.Gra_) then
      convertLHE =39
  elseif( Part.lt.-9000) then
      convertLHE =Part
  else
      print *, "LHE format not implemented for ",Part
      stop
  endif

END FUNCTION






FUNCTION getMass(Part)
implicit none
real(8) :: getMass
integer :: Part


  if( Part.eq.Glu_ ) then
      getMass = 0d0
  elseif( abs(Part).eq.abs(ElM_) ) then
      getMass = m_el
  elseif( abs(Part).eq.abs(MuM_) ) then
      getMass = m_mu
  elseif( abs(Part).eq.abs(TaM_) ) then
      getMass = m_tau
  elseif( abs(Part).eq.abs(NuE_) ) then
      getMass = 0d0
  elseif( abs(Part).eq.abs(NuM_) ) then
      getMass = 0d0
  elseif( abs(Part).eq.abs(NuT_) ) then
      getMass = 0d0
  elseif( abs(Part).eq.abs(Up_) ) then
      getMass = 0d0
  elseif( abs(Part).eq.abs(Dn_) ) then
      getMass = 0d0
  elseif( abs(Part).eq.abs(Chm_) ) then
      getMass = 0d0
  elseif( abs(Part).eq.abs(Str_) ) then
      getMass = 0d0
  elseif( abs(Part).eq.abs(Bot_) ) then
      getMass = m_bot
  elseif( abs(Part).eq.abs(Z0_) ) then
      getMass = M_Z
  elseif( abs(Part).eq.abs(Wp_) ) then
      getMass = M_W
  elseif( abs(Part).eq.abs(Pho_) ) then
      getMass = 0d0
  else
     print *, "Error in getMass",Part
     stop
  endif


END FUNCTION





FUNCTION IsAZDecay(DKMode)
implicit none
logical :: IsAZDecay
integer :: DKMode


  if( DKMode.eq.0 ) then
     IsAZDecay = .true.
  elseif( DKMode.eq.1 ) then
     IsAZDecay = .true.
  elseif( DKMode.eq.2 ) then
     IsAZDecay = .true.
  elseif( DKMode.eq.3 ) then
     IsAZDecay = .true.
  elseif( DKMode.eq.8 ) then
     IsAZDecay = .true.
  elseif( DKMode.eq.9 ) then
     IsAZDecay = .true.
  else
     IsAZDecay=.false.
  endif

END FUNCTION




FUNCTION IsAWDecay(DKMode)
implicit none
logical :: IsAWDecay
integer :: DKMode


  if( DKMode.eq.4 ) then
     IsAWDecay = .true.
  elseif( DKMode.eq.5 ) then
     IsAWDecay = .true.
  elseif( DKMode.eq.6 ) then
     IsAWDecay = .true.
  elseif( DKMode.eq.10 ) then
     IsAWDecay = .true.
  elseif( DKMode.eq.11 ) then
     IsAWDecay = .true.
  else
     IsAWDecay=.false.
  endif


END FUNCTION



FUNCTION IsAPhoton(DKMode)
implicit none
logical :: IsAPhoton
integer :: DKMode


  if( DKMode.eq.7 ) then
     IsAPhoton = .true.
  else
     IsAPhoton=.false.
  endif


END FUNCTION






FUNCTION IsAQuark(PartType)
implicit none
logical :: IsAQuark
integer :: PartType


  if( abs(PartType).ge.1 .and. abs(PartType).le.6 ) then
     IsAQuark = .true.
  else
     IsAQuark=.false.
  endif

END FUNCTION





FUNCTION IsANeutrino(PartType)
implicit none
logical :: IsANeutrino
integer :: PartType


  if( abs(PartType).ge.14 .and. abs(PartType).le.16 ) then
     IsANeutrino = .true.
  else
     IsANeutrino=.false.
  endif

END FUNCTION



FUNCTION IsABoson(PartType)
implicit none
logical :: IsABoson
integer :: PartType


  if( abs(PartType).eq.11 .or. abs(PartType).eq.12 .or. abs(PartType).eq.13 .or. abs(PartType).eq.25 ) then
     IsABoson = .true.
  else
     IsABoson=.false.
  endif


END FUNCTION



FUNCTION SU2flip(Part)
implicit none
integer :: SU2flip
integer :: Part

  if( abs(Part).eq.Up_ ) then
      SU2flip = sign(1,Part)*Dn_
  elseif( abs(Part).eq.Dn_ ) then
      SU2flip = sign(1,Part)*Up_
  elseif( abs(Part).eq.Chm_ ) then
      SU2flip = sign(1,Part)*Str_
  elseif( abs(Part).eq.Str_ ) then
      SU2flip = sign(1,Part)*Chm_
  elseif( abs(Part).eq.Bot_ ) then
      SU2flip = sign(1,Part)*Top_
  elseif( abs(Part).eq.Top_ ) then
      SU2flip = sign(1,Part)*Bot_
  elseif( abs(Part).eq.ElP_ ) then
      SU2flip = sign(1,Part)*NuE_
  elseif( abs(Part).eq.MuP_ ) then
      SU2flip = sign(1,Part)*NuM_
  elseif( abs(Part).eq.TaP_ ) then
      SU2flip = sign(1,Part)*NuT_
  elseif( abs(Part).eq.NuE_ ) then
      SU2flip = sign(1,Part)*ElP_
  elseif( abs(Part).eq.NuM_ ) then
      SU2flip = sign(1,Part)*MuP_
  elseif( abs(Part).eq.NuT_ ) then
      SU2flip = sign(1,Part)*TaP_
  else
      print *, "Error: Invalid flavor in SU2flip"
      stop
  endif

END FUNCTION




FUNCTION ChargeFlip(Part)
implicit none
integer :: ChargeFlip
integer :: Part

  if( (abs(Part).ge.1 .and. abs(Part).le.9)  .or. (abs(Part).ge.13 .and. abs(Part).le.16)) then! quarks, leptons, W's, neutrinos
      ChargeFlip = -Part
  elseif( (abs(Part).ge.10 .and. abs(Part).le.12) ) then!  glu,pho,Z0
      ChargeFlip = +Part
  elseif( Part.eq.25 .or. Part.eq.32 .or. Part.eq.39 ) then ! Higgs,Zprime,Graviton
      ChargeFlip = +Part
  else
      print *, "Error: Invalid flavor in ChargeFlip"
      stop
  endif

END FUNCTION





END MODULE




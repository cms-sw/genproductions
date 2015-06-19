COM_ENERGY = 13000 # GeV
SQUARK_MASS = 1500   # GeV
CHIZERO_MASS = 1050 # GeV
GLUINO_MASS = 5000 # GeV
CROSS_SECTION = 0.002673 # pb
CHIZERO1_CTAU = 1 # mm
CHIZERO2_CTAU = 10 # mm
CHIZERO3_CTAU = 100 # mm
CHIZERO4_CTAU = 100 # mm

SLHA_TABLE = """
#
#                              ======================
#                              | THE SUSYHIT OUTPUT |
#                              ======================
#
#
#              ------------------------------------------------------
#              |     This is the output of the SUSY-HIT package     |
#              |  created by A.Djouadi, M.Muehlleitner and M.Spira. |
#              |  In case of problems with SUSY-HIT email to        |
#              |           margarete.muehlleitner@kit.edu           |
#              |           michael.spira@psi.ch                     |
#              |           abdelhak.djouadi@cern.ch                 |
#              ------------------------------------------------------
#
#              ------------------------------------------------------
#              |  SUSY Les Houches Accord - MSSM Spectrum + Decays  |
#              |              based on the decay programs           |
#              |                                                    |
#              |                     SDECAY 1.5                     |
#              |                                                    |
#              |  Authors: M.Muhlleitner, A.Djouadi and Y.Mambrini  |
#              |  Ref.:    Comput.Phys.Commun.168(2005)46           |
#              |           [hep-ph/0311167]                         |
#              |                                                    |
#              |                     HDECAY 3.4                     |
#              |                                                    |
#              |  By: A.Djouadi,J.Kalinowski,M.Muhlleitner,M.Spira  |
#              |  Ref.:    Comput.Phys.Commun.108(1998)56           |
#              |           [hep-ph/9704448]                         |
#              |                                                    |
#              |                                                    |
#              |  If not stated otherwise all DRbar couplings and   |
#              |  soft SUSY breaking masses are given at the scale  |
#              |  Q=  0.46577748E+03
#              |                                                    |
#              ------------------------------------------------------
#
# This file contains spectra and decay modes calculated by SUSYHIT in ~tomalin/susy_susyhit/
# with all SUSY masses set high, except for the u,d,s,c squark and all four neutralino masses,
# and the gluino mass, which are set low by hand. Furthermore, the decay modes and width of the "neutralino1",
# "neutralino2", "neutralino3" and "neutralino4" are set by hand to allow R-parity violating 
# decays to the chosen final states with the chosen lifetimes. The u,d,s,c squarks given by hand a 100% BR
# to quark + neutralino.
#
BLOCK DCINFO  # Decay Program information
     1   SDECAY/HDECAY # decay calculator
     2   1.5  /3.4    # version number
#
BLOCK SPINFO  # Spectrum calculator information
     1   SuSpect     # RGE +Spectrum calculator            
     2   2.41        # version number                      
#
BLOCK MODSEL  # Model selection
     1     1   # #SUGRA                                            
#
BLOCK SMINPUTS  # Standard Model inputs
         1     1.27934000E+02   # alpha_em^-1(M_Z)^MSbar
         2     1.16639000E-05   # G_F [GeV^-2]
         3     1.17200000E-01   # alpha_S(M_Z)^MSbar
         4     9.11870000E+01   # M_Z pole mass
         5     4.25000000E+00   # mb(mb)^MSbar
         6     1.72500000E+02   # mt pole mass
         7     1.77710000E+00   # mtau pole mass
#
BLOCK MINPAR  # Input parameters - minimal models
         1     1.00000000E+02   # m0                  
         2     2.50000000E+02   # m_1                 
         3     1.00000000E+01   # tanbeta(mZ)         
         4     1.00000000E+00   # sign(mu)            
         5    -1.00000000E+02   # A0                  
#
BLOCK EXTPAR  # Input parameters - non-minimal models
         0     4.65777483E+02   # EWSB                
#
BLOCK MASS  # Mass Spectrum
# PDG code           mass       particle
        24     8.04847331E+01   # W+
        25     1.09932416E+02   # h
        35     3.94935594E+02   # H
        36     3.94525488E+02   # A
        37     4.02953218E+02   # H+
         5     4.87877839E+00   # b-quark pole mass calculated from mb(mb)_Msbar
   1000001     1.50000000e+03   # ~d_L
   2000001     1.50000000e+03   # ~d_R
   1000002     1.50000000e+03   # ~u_L
   2000002     1.50000000e+03   # ~u_R
   1000003     1.50000000e+03   # ~s_L
   2000003     1.50000000e+03   # ~s_R
   1000004     1.50000000e+03   # ~c_L
   2000004     1.50000000e+03   # ~c_R
   1000005     1.16777573E+05   # ~b_1
   2000005     1.46086561E+05   # ~b_2
   1000006     1.99615017E+05   # ~t_1
   2000006     1.86391641E+05   # ~t_2
   1000011     2.00774228E+05   # ~e_L
   2000011     1.42820157E+05   # ~e_R
   1000012     1.84853985E+05   # ~nu_eL
   1000013     2.00774228E+05   # ~mu_L
   2000013     1.42820157E+05   # ~mu_R
   1000014     1.84853985E+05   # ~nu_muL
   1000015     1.33342244E+05   # ~tau_1
   2000015     2.04795115E+05   # ~tau_2
   1000016     1.83966053E+05   # ~nu_tauL
   1000021     5.00000000e+03   # ~g
   1000022     1.05000000e+03   # ~chi_10
   1000023     1.05000000e+03   # ~chi_20
   1000025     1.05000000e+03   # ~chi_30
   1000035     1.05000000e+03   # ~chi_40
   1000024     1.79671182E+05   # ~chi_1+
   1000037     3.77983105E+05   # ~chi_2+
#
BLOCK NMIX  # Neutralino Mixing Matrix
  1  1     9.85345167E-01   # N_11
  1  2    -5.64225409E-02   # N_12
  1  3     1.51059160E-01   # N_13
  1  4    -5.56105151E-02   # N_14
  2  1     1.06123308E-01   # N_21
  2  2     9.39651214E-01   # N_22
  2  3    -2.80885422E-01   # N_23
  2  4     1.64002501E-01   # N_24
  3  1     6.12835220E-02   # N_31
  3  2    -9.07288796E-02   # N_32
  3  3    -6.95178480E-01   # N_33
  3  4    -7.10450196E-01   # N_34
  4  1     1.18646854E-01   # N_41
  4  2    -3.25023636E-01   # N_42
  4  3    -6.44213777E-01   # N_43
  4  4     6.82107887E-01   # N_44
#
BLOCK UMIX  # Chargino Mixing Matrix U
  1  1    -9.11420712E-01   # U_11
  1  2     4.11475741E-01   # U_12
  2  1     4.11475741E-01   # U_21
  2  2     9.11420712E-01   # U_22
#
BLOCK VMIX  # Chargino Mixing Matrix V
  1  1    -9.70421546E-01   # V_11
  1  2     2.41416701E-01   # V_12
  2  1     2.41416701E-01   # V_21
  2  2     9.70421546E-01   # V_22
#
BLOCK STOPMIX  # Stop Mixing Matrix
  1  1     5.52988023E-01   # cos(theta_t)
  1  2     8.33189202E-01   # sin(theta_t)
  2  1    -8.33189202E-01   # -sin(theta_t)
  2  2     5.52988023E-01   # cos(theta_t)
#
BLOCK SBOTMIX  # Sbottom Mixing Matrix
  1  1     9.30091013E-01   # cos(theta_b)
  1  2     3.67329154E-01   # sin(theta_b)
  2  1    -3.67329154E-01   # -sin(theta_b)
  2  2     9.30091013E-01   # cos(theta_b)
#
BLOCK STAUMIX  # Stau Mixing Matrix
  1  1     2.84460080E-01   # cos(theta_tau)
  1  2     9.58687886E-01   # sin(theta_tau)
  2  1    -9.58687886E-01   # -sin(theta_tau)
  2  2     2.84460080E-01   # cos(theta_tau)
#
BLOCK ALPHA  # Higgs mixing
          -1.14188002E-01   # Mixing angle in the neutral Higgs boson sector
#
BLOCK HMIX Q=  4.65777483E+02  # DRbar Higgs Parameters
         1     3.52164861E+02   # mu(Q)               
         2     9.75041102E+00   # tanbeta(Q)          
         3     2.45014641E+02   # vev(Q)              
         4     1.62371513E+05   # MA^2(Q)             
#
BLOCK GAUGE Q=  4.65777483E+02  # The gauge couplings
     1     3.60982135E-01   # gprime(Q) DRbar
     2     6.46351672E-01   # g(Q) DRbar
     3     1.09632112E+00   # g3(Q) DRbar
#
BLOCK AU Q=  4.65777483E+02  # The trilinear couplings
  1  1    -6.83184382E+02   # A_u(Q) DRbar
  2  2    -6.83184382E+02   # A_c(Q) DRbar
  3  3    -5.06144038E+02   # A_t(Q) DRbar
#
BLOCK AD Q=  4.65777483E+02  # The trilinear couplings
  1  1    -8.58985213E+02   # A_d(Q) DRbar
  2  2    -8.58985213E+02   # A_s(Q) DRbar
  3  3    -7.96595983E+02   # A_b(Q) DRbar
#
BLOCK AE Q=  4.65777483E+02  # The trilinear couplings
  1  1    -2.53298464E+02   # A_e(Q) DRbar
  2  2    -2.53298464E+02   # A_mu(Q) DRbar
  3  3    -2.51542764E+02   # A_tau(Q) DRbar
#
BLOCK Yu Q=  4.65777483E+02  # The Yukawa couplings
  1  1     0.00000000E+00   # y_u(Q) DRbar
  2  2     0.00000000E+00   # y_c(Q) DRbar
  3  3     8.78978125E-01   # y_t(Q) DRbar
#
BLOCK Yd Q=  4.65777483E+02  # The Yukawa couplings
  1  1     0.00000000E+00   # y_d(Q) DRbar
  2  2     0.00000000E+00   # y_s(Q) DRbar
  3  3     1.39517330E-01   # y_b(Q) DRbar
#
BLOCK Ye Q=  4.65777483E+02  # The Yukawa couplings
  1  1     0.00000000E+00   # y_e(Q) DRbar
  2  2     0.00000000E+00   # y_mu(Q) DRbar
  3  3     1.01147257E-01   # y_tau(Q) DRbar
#
BLOCK MSOFT Q=  4.65777483E+02  # The soft SUSY breaking masses at the scale Q
         1     1.01486794E+02   # M_1                 
         2     1.91565439E+02   # M_2                 
         3     5.86284400E+02   # M_3                 
        21     3.23226904E+04   # M^2_Hd              
        22    -1.24993993E+05   # M^2_Hu              
        31     1.95443359E+02   # M_eL                
        32     1.95443359E+02   # M_muL               
        33     1.94603750E+02   # M_tauL              
        34     1.35950985E+02   # M_eR                
        35     1.35950985E+02   # M_muR               
        36     1.33480599E+02   # M_tauR              
        41     5.45553618E+02   # M_q1L               
        42     5.45553618E+02   # M_q2L               
        43     4.97578078E+02   # M_q3L               
        44     5.27538927E+02   # M_uR                
        45     5.27538927E+02   # M_cR                
        46     4.23429537E+02   # M_tR                
        47     5.25444117E+02   # M_dR                
        48     5.25444117E+02   # M_sR                
        49     5.22139557E+02   # M_bR                
#
#
#
#                             =================
#                             |The decay table|
#                             =================
#
# - The QCD corrections to the decays gluino -> squark  + quark
#                                     squark -> gaugino + quark_prime
#                                     squark -> squark_prime + Higgs
#                                     squark -> gluino  + quark
#   are included.
#
# - The multi-body decays for the inos, stops and sbottoms are included.
#
# - The loop induced decays for the gluino, neutralinos and stops
#   are included.
#
# - The SUSY decays of the top quark are included.
#
#
#         PDG            Width
DECAY         6     1.44633943E+00   # top decays
#          BR         NDA      ID1       ID2
     1.00000000E+00    2           5        24   # BR(t ->  b    W+)
#
#         PDG            Width
DECAY   1000021     7.60054186E+02   # gluino decays
#          BR         NDA      ID1       ID2
     6.25000000E-02    2     1000001        -1   # BR(~g -> ~d_L  db)
     6.25000000E-02    2    -1000001         1   # BR(~g -> ~d_L* d )
     6.25000000E-02    2     2000001        -1   # BR(~g -> ~d_R  db)
     6.25000000E-02    2    -2000001         1   # BR(~g -> ~d_R* d )
     6.25000000E-02    2     1000002        -2   # BR(~g -> ~u_L  ub)
     6.25000000E-02    2    -1000002         2   # BR(~g -> ~u_L* u )
     6.25000000E-02    2     2000002        -2   # BR(~g -> ~u_R  ub)
     6.25000000E-02    2    -2000002         2   # BR(~g -> ~u_R* u )
     6.25000000E-02    2     1000003        -3   # BR(~g -> ~s_L  sb)
     6.25000000E-02    2    -1000003         3   # BR(~g -> ~s_L* s )
     6.25000000E-02    2     2000003        -3   # BR(~g -> ~s_R  sb)
     6.25000000E-02    2    -2000003         3   # BR(~g -> ~s_R* s )
     6.25000000E-02    2     1000004        -4   # BR(~g -> ~c_L  cb)
     6.25000000E-02    2    -1000004         4   # BR(~g -> ~c_L* c )
     6.25000000E-02    2     2000004        -4   # BR(~g -> ~c_R  cb)
     6.25000000E-02    2    -2000004         4   # BR(~g -> ~c_R* c )
#
#         PDG            Width
DECAY   1000006     5.20726800E+08   # stop1 decays
#          BR         NDA      ID1       ID2
     6.94407916E-07    2     1000022         6   # BR(~t_1 -> ~chi_10 t )
     7.54006758E-07    2     1000023         6   # BR(~t_1 -> ~chi_20 t )
     1.74512689E-06    2     1000025         6   # BR(~t_1 -> ~chi_30 t )
     9.52699953E-07    2     1000035         6   # BR(~t_1 -> ~chi_40 t )
     1.56675416E-07    2     1000024         5   # BR(~t_1 -> ~chi_1+ b )
     1.30596297E-05    2     1000021         6   # BR(~t_1 -> ~g      t )
     1.23218005E-11    2     1000005        37   # BR(~t_1 -> ~b_1    H+)
    -1.39984102E-12    2     2000005        37   # BR(~t_1 -> ~b_2    H+)
     9.48532652E-01    2     1000005        24   # BR(~t_1 -> ~b_1    W+)
     5.14499859E-02    2     2000005        24   # BR(~t_1 -> ~b_2    W+)
#
#         PDG            Width
DECAY   2000006     7.43034077E+08   # stop2 decays
#          BR         NDA      ID1       ID2
     1.17380400E-07    2     1000022         6   # BR(~t_2 -> ~chi_10 t )
     5.49682993E-07    2     1000023         6   # BR(~t_2 -> ~chi_20 t )
     9.14655161E-07    2     1000025         6   # BR(~t_2 -> ~chi_30 t )
     1.36777476E-06    2     1000035         6   # BR(~t_2 -> ~chi_40 t )
     1.06348847E-08    2     1000024         5   # BR(~t_2 -> ~chi_1+ b )
     8.72407685E-06    2     1000021         6   # BR(~t_2 -> ~g      t )
     1.27939288E-12    2     1000005        37   # BR(~t_2 -> ~b_1    H+)
    -1.03472409E-12    2     2000005        37   # BR(~t_2 -> ~b_2    H+)
     9.62083919E-01    2     1000005        24   # BR(~t_2 -> ~b_1    W+)
     3.79043964E-02    2     2000005        24   # BR(~t_2 -> ~b_2    W+)
#
#         PDG            Width
DECAY   1000005     1.45287195E+04   # sbottom1 decays
#          BR         NDA      ID1       ID2
     5.66130593E-03    2     1000022         5   # BR(~b_1 -> ~chi_10 b )
     6.03025772E-02    2     1000023         5   # BR(~b_1 -> ~chi_20 b )
     1.44120022E-03    2     1000025         5   # BR(~b_1 -> ~chi_30 b )
     6.36747404E-03    2     1000035         5   # BR(~b_1 -> ~chi_40 b )
     9.26227443E-01    2     1000021         5   # BR(~b_1 -> ~g      b )
#
#         PDG            Width
DECAY   2000005     7.36323181E+06   # sbottom2 decays
#          BR         NDA      ID1       ID2
     1.43513479E-05    2     1000022         5   # BR(~b_2 -> ~chi_10 b )
     1.14591077E-05    2     1000023         5   # BR(~b_2 -> ~chi_20 b )
     9.50353235E-06    2     1000025         5   # BR(~b_2 -> ~chi_30 b )
     1.65018741E-05    2     1000035         5   # BR(~b_2 -> ~chi_40 b )
     1.56864744E-03    2     1000021         5   # BR(~b_2 -> ~g      b )
    -2.65633090E-12    2     1000005        25   # BR(~b_2 -> ~b_1    h )
    -8.87029310E-11    2     1000005        35   # BR(~b_2 -> ~b_1    H )
    -1.62921553E-10    2     1000005        36   # BR(~b_2 -> ~b_1    A )
     9.98379537E-01    2     1000005        23   # BR(~b_2 -> ~b_1    Z )
#
#         PDG            Width
DECAY   1000002     4.39789139E+00   # sup_L decays
#          BR         NDA      ID1       ID2
     2.50000000E-01    2     1000022         2   # BR(~u_L -> ~chi_10 u)
     2.50000000E-01    2     1000023         2   # BR(~u_L -> ~chi_20 u)
     2.50000000E-01    2     1000025         2   # BR(~u_L -> ~chi_30 u)
     2.50000000E-01    2     1000035         2   # BR(~u_L -> ~chi_40 u)
#
#         PDG            Width
DECAY   2000002     2.35699810E+00   # sup_R decays
#          BR         NDA      ID1       ID2
     2.50000000E-01    2     1000022         2   # BR(~u_R -> ~chi_10 u)
     2.50000000E-01    2     1000023         2   # BR(~u_R -> ~chi_20 u)
     2.50000000E-01    2     1000025         2   # BR(~u_R -> ~chi_30 u)
     2.50000000E-01    2     1000035         2   # BR(~u_R -> ~chi_40 u)
#
#         PDG            Width
DECAY   1000001     4.39789139E+00   # sdown_L decays
#          BR         NDA      ID1       ID2
     2.50000000E-01    2     1000022         1   # BR(~d_L -> ~chi_10 d)
     2.50000000E-01    2     1000023         1   # BR(~d_L -> ~chi_20 d)
     2.50000000E-01    2     1000025         1   # BR(~d_L -> ~chi_30 d)
     2.50000000E-01    2     1000035         1   # BR(~d_L -> ~chi_40 d)
#
#         PDG            Width
DECAY   2000001     5.89249524E-01   # sdown_R decays
#          BR         NDA      ID1       ID2
     2.50000000E-01    2     1000022         1   # BR(~d_R -> ~chi_10 d)
     2.50000000E-01    2     1000023         1   # BR(~d_R -> ~chi_20 d)
     2.50000000E-01    2     1000025         1   # BR(~d_R -> ~chi_30 d)
     2.50000000E-01    2     1000035         1   # BR(~d_R -> ~chi_40 d)
#
#         PDG            Width
DECAY   1000004     4.39789139E+00   # scharm_L decays
#          BR         NDA      ID1       ID2
     2.50000000E-01    2     1000022         4   # BR(~c_L -> ~chi_10 c)
     2.50000000E-01    2     1000023         4   # BR(~c_L -> ~chi_20 c)
     2.50000000E-01    2     1000025         4   # BR(~c_L -> ~chi_30 c)
     2.50000000E-01    2     1000035         4   # BR(~c_L -> ~chi_40 c)
#
#         PDG            Width
DECAY   2000004     2.35699810E+00   # scharm_R decays
#          BR         NDA      ID1       ID2
     2.50000000E-01    2     1000022         4   # BR(~c_R -> ~chi_10 c)
     2.50000000E-01    2     1000023         4   # BR(~c_R -> ~chi_20 c)
     2.50000000E-01    2     1000025         4   # BR(~c_R -> ~chi_30 c)
     2.50000000E-01    2     1000035         4   # BR(~c_R -> ~chi_40 c)
#
#         PDG            Width
DECAY   1000003     4.39789139E+00   # sstrange_L decays
#          BR         NDA      ID1       ID2
     2.50000000E-01    2     1000022         3   # BR(~s_L -> ~chi_10 s)
     2.50000000E-01    2     1000023         3   # BR(~s_L -> ~chi_20 s)
     2.50000000E-01    2     1000025         3   # BR(~s_L -> ~chi_30 s)
     2.50000000E-01    2     1000035         3   # BR(~s_L -> ~chi_40 s)
#
#         PDG            Width
DECAY   2000003     5.89249524E-01   # sstrange_R decays
#          BR         NDA      ID1       ID2
     2.50000000E-01    2     1000022         3   # BR(~s_R -> ~chi_10 s)
     2.50000000E-01    2     1000023         3   # BR(~s_R -> ~chi_20 s)
     2.50000000E-01    2     1000025         3   # BR(~s_R -> ~chi_30 s)
     2.50000000E-01    2     1000035         3   # BR(~s_R -> ~chi_40 s)
#
#         PDG            Width
DECAY   1000011     1.14957407E+03   # selectron_L decays
#          BR         NDA      ID1       ID2
     1.77035178E-01    2     1000022        11   # BR(~e_L -> ~chi_10 e-)
     7.24219211E-01    2     1000023        11   # BR(~e_L -> ~chi_20 e-)
     2.31709727E-03    2     1000025        11   # BR(~e_L -> ~chi_30 e-)
     4.85963722E-02    2     1000035        11   # BR(~e_L -> ~chi_40 e-)
     4.78321411E-02    2    -1000024        12   # BR(~e_L -> ~chi_1- nu_e)
#
#         PDG            Width
DECAY   2000011     7.40491593E+02   # selectron_R decays
#          BR         NDA      ID1       ID2
     9.70905097E-01    2     1000022        11   # BR(~e_R -> ~chi_10 e-)
     1.12621565E-02    2     1000023        11   # BR(~e_R -> ~chi_20 e-)
     3.75567007E-03    2     1000025        11   # BR(~e_R -> ~chi_30 e-)
     1.40770760E-02    2     1000035        11   # BR(~e_R -> ~chi_40 e-)
#
#         PDG            Width
DECAY   1000013     1.14957407E+03   # smuon_L decays
#          BR         NDA      ID1       ID2
     1.77035178E-01    2     1000022        13   # BR(~mu_L -> ~chi_10 mu-)
     7.24219211E-01    2     1000023        13   # BR(~mu_L -> ~chi_20 mu-)
     2.31709727E-03    2     1000025        13   # BR(~mu_L -> ~chi_30 mu-)
     4.85963722E-02    2     1000035        13   # BR(~mu_L -> ~chi_40 mu-)
     4.78321411E-02    2    -1000024        14   # BR(~mu_L -> ~chi_1- nu_mu)
#
#         PDG            Width
DECAY   2000013     7.40491593E+02   # smuon_R decays
#          BR         NDA      ID1       ID2
     9.70905097E-01    2     1000022        13   # BR(~mu_R -> ~chi_10 mu-)
     1.12621565E-02    2     1000023        13   # BR(~mu_R -> ~chi_20 mu-)
     3.75567007E-03    2     1000025        13   # BR(~mu_R -> ~chi_30 mu-)
     1.40770760E-02    2     1000035        13   # BR(~mu_R -> ~chi_40 mu-)
#
#         PDG            Width
DECAY   1000015     7.21371477E+02   # stau_1 decays
#          BR         NDA      ID1       ID2
     8.79723524E-01    2     1000022        15   # BR(~tau_1 -> ~chi_10  tau-)
     9.78394101E-02    2     1000023        15   # BR(~tau_1 -> ~chi_20  tau-)
     1.36341837E-02    2     1000025        15   # BR(~tau_1 -> ~chi_30  tau-)
     8.80288245E-03    2     1000035        15   # BR(~tau_1 -> ~chi_40  tau-)
#
#         PDG            Width
DECAY   2000015     7.64810560E+07   # stau_2 decays
#          BR         NDA      ID1       ID2
     3.47470450E-06    2     1000022        15   # BR(~tau_2 -> ~chi_10  tau-)
     9.92864945E-06    2     1000023        15   # BR(~tau_2 -> ~chi_20  tau-)
     4.16934434E-07    2     1000025        15   # BR(~tau_2 -> ~chi_30  tau-)
     1.26535934E-06    2     1000035        15   # BR(~tau_2 -> ~chi_40  tau-)
     8.63848274E-07    2    -1000024        16   # BR(~tau_2 -> ~chi_1-  nu_tau)
     1.21539693E-16    2     1000016       -37   # BR(~tau_2 -> ~nu_tauL H-)
     4.76541501E-01    2     1000016       -24   # BR(~tau_2 -> ~nu_tauL W-)
     3.62362808E-13    2     1000015        25   # BR(~tau_2 -> ~tau_1   h)
     1.17492293E-13    2     1000015        35   # BR(~tau_2 -> ~tau_1   H)
     1.71889290E-13    2     1000015        36   # BR(~tau_2 -> ~tau_1   A)
     5.23442550E-01    2     1000015        23   # BR(~tau_2 -> ~tau_1   Z)
#
#         PDG            Width
DECAY   1000012     1.01221547E+03   # snu_eL decays
#          BR         NDA      ID1       ID2
     2.79372600E-01    2     1000022        12   # BR(~nu_eL -> ~chi_10 nu_e)
     5.88214477E-01    2     1000023        12   # BR(~nu_eL -> ~chi_20 nu_e)
     1.18495551E-02    2     1000025        12   # BR(~nu_eL -> ~chi_30 nu_e)
     1.16194049E-01    2     1000035        12   # BR(~nu_eL -> ~chi_40 nu_e)
     4.36931919E-03    2     1000024        11   # BR(~nu_eL -> ~chi_1+ e-)
#
#         PDG            Width
DECAY   1000014     1.01221547E+03   # snu_muL decays
#          BR         NDA      ID1       ID2
     2.79372600E-01    2     1000022        14   # BR(~nu_muL -> ~chi_10 nu_mu)
     5.88214477E-01    2     1000023        14   # BR(~nu_muL -> ~chi_20 nu_mu)
     1.18495551E-02    2     1000025        14   # BR(~nu_muL -> ~chi_30 nu_mu)
     1.16194049E-01    2     1000035        14   # BR(~nu_muL -> ~chi_40 nu_mu)
     4.36931919E-03    2     1000024        13   # BR(~nu_muL -> ~chi_1+ mu-)
#
#         PDG            Width
DECAY   1000016     3.45589203E+07   # snu_tauL decays
#          BR         NDA      ID1       ID2
     8.14339470E-06    2     1000022        16   # BR(~nu_tauL -> ~chi_10 nu_tau)
     1.71457854E-05    2     1000023        16   # BR(~nu_tauL -> ~chi_20 nu_tau)
     3.45401102E-07    2     1000025        16   # BR(~nu_tauL -> ~chi_30 nu_tau)
     3.38692486E-06    2     1000035        16   # BR(~nu_tauL -> ~chi_40 nu_tau)
     8.91121489E-08    2     1000024        15   # BR(~nu_tauL -> ~chi_1+ tau-)
     7.73336712E-13    2    -1000015       -37   # BR(~nu_tauL -> ~tau_1+ H-)
     9.99970889E-01    2    -1000015       -24   # BR(~nu_tauL -> ~tau_1+ W-)
#
#         PDG            Width
DECAY   1000024     7.01833737E+09   # chargino1+ decays
#          BR         NDA      ID1       ID2
     2.90894156E-07    2     1000002        -1   # BR(~chi_1+ -> ~u_L     db)
     2.56597205E-07    2    -1000001         2   # BR(~chi_1+ -> ~d_L*    u )
     2.90894156E-07    2     1000004        -3   # BR(~chi_1+ -> ~c_L     sb)
     2.56597205E-07    2    -1000003         4   # BR(~chi_1+ -> ~s_L*    c )
     1.08970890E-07    2    -1000005         6   # BR(~chi_1+ -> ~b_1*    t )
     2.66850233E-09    2    -2000005         6   # BR(~chi_1+ -> ~b_2*    t )
     2.21204331E-09    2    -1000015        16   # BR(~chi_1+ -> ~tau_1+  nu_tau)
     7.01106740E-03    2     1000022        24   # BR(~chi_1+ -> ~chi_10  W+)
     9.34913437E-01    2     1000023        24   # BR(~chi_1+ -> ~chi_20  W+)
     3.08104751E-02    2     1000025        24   # BR(~chi_1+ -> ~chi_30  W+)
     2.72637115E-02    2     1000035        24   # BR(~chi_1+ -> ~chi_40  W+)
     8.35493242E-09    2     1000022        37   # BR(~chi_1+ -> ~chi_10  H+)
     1.26441931E-10    2     1000023        37   # BR(~chi_1+ -> ~chi_20  H+)
     4.50124336E-08    2     1000025        37   # BR(~chi_1+ -> ~chi_30  H+)
     4.67335464E-08    2     1000035        37   # BR(~chi_1+ -> ~chi_40  H+)
#
#         PDG            Width
DECAY   1000037     3.93890988E+10   # chargino2+ decays
#          BR         NDA      ID1       ID2
     6.74883626E-09    2     1000002        -1   # BR(~chi_2+ -> ~u_L     db)
     1.96057175E-08    2    -1000001         2   # BR(~chi_2+ -> ~d_L*    u )
     6.74883626E-09    2     1000004        -3   # BR(~chi_2+ -> ~c_L     sb)
     1.96057175E-08    2    -1000003         4   # BR(~chi_2+ -> ~s_L*    c )
     1.15937825E-07    2     1000006        -5   # BR(~chi_2+ -> ~t_1     bb)
     1.19037282E-07    2     2000006        -5   # BR(~chi_2+ -> ~t_2     bb)
     1.11588460E-07    2    -1000005         6   # BR(~chi_2+ -> ~b_1*    t )
     1.44625833E-08    2    -2000005         6   # BR(~chi_2+ -> ~b_2*    t )
     1.34536540E-09    2     1000012       -11   # BR(~chi_2+ -> ~nu_eL   e+  )
     1.34536540E-09    2     1000014       -13   # BR(~chi_2+ -> ~nu_muL  mu+ )
     1.82588282E-09    2     1000016       -15   # BR(~chi_2+ -> ~nu_tau1 tau+)
     3.47934204E-09    2    -1000011        12   # BR(~chi_2+ -> ~e_L+    nu_e)
     3.47934204E-09    2    -1000013        14   # BR(~chi_2+ -> ~mu_L+   nu_mu)
     1.18484354E-11    2    -1000015        16   # BR(~chi_2+ -> ~tau_1+  nu_tau)
     3.76671403E-09    2    -2000015        16   # BR(~chi_2+ -> ~tau_2+  nu_tau)
     2.03789007E-02    2     1000024        23   # BR(~chi_2+ -> ~chi_1+  Z )
     5.36373400E-03    2     1000022        24   # BR(~chi_2+ -> ~chi_10  W+)
     4.86793289E-02    2     1000023        24   # BR(~chi_2+ -> ~chi_20  W+)
     3.97860738E-01    2     1000025        24   # BR(~chi_2+ -> ~chi_30  W+)
     5.27716802E-01    2     1000035        24   # BR(~chi_2+ -> ~chi_40  W+)
     1.42885308E-08    2     1000024        25   # BR(~chi_2+ -> ~chi_1+  h )
     1.05312175E-08    2     1000024        35   # BR(~chi_2+ -> ~chi_1+  H )
     1.45322715E-08    2     1000024        36   # BR(~chi_2+ -> ~chi_1+  A )
     2.63246430E-09    2     1000022        37   # BR(~chi_2+ -> ~chi_10  H+)
     2.29697208E-08    2     1000023        37   # BR(~chi_2+ -> ~chi_20  H+)
     2.47791857E-09    2     1000025        37   # BR(~chi_2+ -> ~chi_30  H+)
     3.81515982E-10    2     1000035        37   # BR(~chi_2+ -> ~chi_40  H+)
#
# These are edited by hand, allowing for R parity violating decays.
# With lambda_122 and lambda_121 equal and non-zero, all the decay modes below would be allowed.
# Each leptonic decay would be have a 12.5% BR, but we choose to have equal BR to 2*mu, 2*e and e+mu 
# final states. In addition, we then halve the total leptonic decay branching ratio, and force
# the remaining neutralinos to decay to q qbar nu
#
#         PDG            Width
DECAY   1000022      1.97812500e-13  # neutralino1 decays
     4.15000000E-02    3         -12       -13       11   # BR(~chi_10 --> nu_ebar mu+   e-)  
     4.15000000E-02    3          12        13      -11   # BR(~chi_10 --> nu_e    mu-   e+) 
     8.35000000E-02    3         -12       -13       13   # BR(~chi_10 --> nu_ebar mu+   mu-)  
     8.35000000E-02    3          12        13      -13   # BR(~chi_10 --> nu_e    mu-   mu+) 
     8.35000000E-02    3         -14       -11       11   # BR(~chi_10 --> nu_mubar e+   e-)  
     8.35000000E-02    3          14        11      -11   # BR(~chi_10 --> nu_mu    e-   e+) 
     4.15000000E-02    3         -14       -11       13   # BR(~chi_10 --> nu_mubar e+   mu-)  
     4.15000000E-02    3          14        11      -13   # BR(~chi_10 --> nu_mu    e-   mu+)
     2.50000000E-01    3           2         1        1   # BR(~chi_10 --> u        d    d)
     2.50000000E-01    3          -2        -1       -1   # BR(~chi_10 --> ubar     dbar dbar)
#
#         PDG            Width
DECAY   1000023     1.97812500e-14   # neutralino2 decays
#          BR         NDA      ID1       ID2
     4.15000000E-02    3         -12       -13       11   # BR(~chi_20 --> nu_ebar mu+   e-)  
     4.15000000E-02    3          12        13      -11   # BR(~chi_20 --> nu_e    mu-   e+) 
     8.35000000E-02    3         -12       -13       13   # BR(~chi_20 --> nu_ebar mu+   mu-)  
     8.35000000E-02    3          12        13      -13   # BR(~chi_20 --> nu_e    mu-   mu+) 
     8.35000000E-02    3         -14       -11       11   # BR(~chi_20 --> nu_mubar e+   e-)  
     8.35000000E-02    3          14        11      -11   # BR(~chi_20 --> nu_mu    e-   e+) 
     4.15000000E-02    3         -14       -11       13   # BR(~chi_20 --> nu_mubar e+   mu-)  
     4.15000000E-02    3          14        11      -13   # BR(~chi_20 --> nu_mu    e-   mu+)
     2.50000000E-01    3           2         1        1   # BR(~chi_20 --> u        d    d)
     2.50000000E-01    3          -2        -1       -1   # BR(~chi_20 --> ubar     dbar dbar)
#
#         PDG            Width
DECAY   1000025     1.97812500e-15   # neutralino3 decays
#          BR         NDA      ID1       ID2
     4.15000000E-02    3         -12       -13       11   # BR(~chi_30 --> nu_ebar mu+   e-)  
     4.15000000E-02    3          12        13      -11   # BR(~chi_30 --> nu_e    mu-   e+) 
     8.35000000E-02    3         -12       -13       13   # BR(~chi_30 --> nu_ebar mu+   mu-)  
     8.35000000E-02    3          12        13      -13   # BR(~chi_30 --> nu_e    mu-   mu+) 
     8.35000000E-02    3         -14       -11       11   # BR(~chi_30 --> nu_mubar e+   e-)  
     8.35000000E-02    3          14        11      -11   # BR(~chi_30 --> nu_mu    e-   e+) 
     4.15000000E-02    3         -14       -11       13   # BR(~chi_30 --> nu_mubar e+   mu-)  
     4.15000000E-02    3          14        11      -13   # BR(~chi_30 --> nu_mu    e-   mu+)
     2.50000000E-01    3           2         1        1   # BR(~chi_30 --> u        d    d)
     2.50000000E-01    3          -2        -1       -1   # BR(~chi_30 --> ubar     dbar dbar)
#
#         PDG            Width
DECAY   1000035     1.97812500e-16   # neutralino4 decays
#          BR         NDA      ID1       ID2
     4.15000000E-02    3         -12       -13       11   # BR(~chi_40 --> nu_ebar mu+   e-)  
     4.15000000E-02    3          12        13      -11   # BR(~chi_40 --> nu_e    mu-   e+) 
     8.35000000E-02    3         -12       -13       13   # BR(~chi_40 --> nu_ebar mu+   mu-)  
     8.35000000E-02    3          12        13      -13   # BR(~chi_40 --> nu_e    mu-   mu+) 
     8.35000000E-02    3         -14       -11       11   # BR(~chi_40 --> nu_mubar e+   e-)  
     8.35000000E-02    3          14        11      -11   # BR(~chi_40 --> nu_mu    e-   e+) 
     4.15000000E-02    3         -14       -11       13   # BR(~chi_40 --> nu_mubar e+   mu-)  
     4.15000000E-02    3          14        11      -13   # BR(~chi_40 --> nu_mu    e-   mu+)
     2.50000000E-01    3           2         1        1   # BR(~chi_40 --> u        d    d)
     2.50000000E-01    3          -2        -1       -1   # BR(~chi_40 --> ubar     dbar dbar)
#
#         PDG            Width
DECAY        25     4.37072897E-02   # h decays
#          BR         NDA      ID1       ID2
    -8.48956100E-04    2           5        -5   # BR(h -> b       bb     )
     6.49196146E-03    2         -15        15   # BR(h -> tau+    tau-   )
     2.29847114E-05    2         -13        13   # BR(h -> mu+     mu-    )
     4.98236650E-05    2           3        -3   # BR(h -> s       sb     )
     1.74013922E-03    2           4        -4   # BR(h -> c       cb     )
     4.24426486E-03    2          21        21   # BR(h -> g       g      )
     1.30150434E-04    2          22        22   # BR(h -> gam     gam    )
     2.51876211E-05    2          22        23   # BR(h -> Z       gam    )
     2.91783758E-03    2          24       -24   # BR(h -> W+      W-     )
     2.72932971E-04    2          23        23   # BR(h -> Z       Z      )
     1.09439297E-01    2     1000023   1000023   # BR(h -> ~chi_20 ~chi_20)
     1.09439297E-01    2     1000025   1000025   # BR(h -> ~chi_30 ~chi_30)
     1.09439297E-01    2     1000035   1000035   # BR(h -> ~chi_40 ~chi_40)
     2.18878594E-01    2     1000023   1000025   # BR(h -> ~chi_20 ~chi_30)
     2.18878594E-01    2     1000023   1000035   # BR(h -> ~chi_20 ~chi_40)
     2.18878594E-01    2     1000025   1000035   # BR(h -> ~chi_30 ~chi_40)
#
#         PDG            Width
DECAY        35     2.84187547E-01   # H decays
#          BR         NDA      ID1       ID2
    -2.33594670E-03    2           5        -5   # BR(H -> b       bb     )
     2.73102816E-01    2         -15        15   # BR(H -> tau+    tau-   )
     9.65523813E-04    2         -13        13   # BR(H -> mu+     mu-    )
     1.60807869E-03    2           3        -3   # BR(H -> s       sb     )
     9.94771297E-06    2           4        -4   # BR(H -> c       cb     )
     1.77882571E-01    2           6        -6   # BR(H -> t       tb     )
     2.68203727E-03    2          21        21   # BR(H -> g       g      )
     3.98544353E-06    2          22        22   # BR(H -> gam     gam    )
     1.37343122E-06    2          23        22   # BR(H -> Z       gam    )
     7.97367629E-03    2          24       -24   # BR(H -> W+      W-     )
     3.71979823E-03    2          23        23   # BR(H -> Z       Z      )
     3.65976272E-02    2          25        25   # BR(H -> h       h      )
     2.38721161E-20    2          36        36   # BR(H -> A       A      )
     4.40168535E-14    2          23        36   # BR(H -> Z       A      )
     1.63370566E-01    2     1000022   1000022   # BR(H -> ~chi_10 ~chi_10)
     2.61257666E-02    2     1000023   1000023   # BR(H -> ~chi_20 ~chi_20)
     2.61257666E-02    2     1000025   1000025   # BR(H -> ~chi_30 ~chi_30)
     2.61257666E-02    2     1000035   1000035   # BR(H -> ~chi_40 ~chi_40)
     3.30953487E-02    2     1000022   1000023   # BR(H -> ~chi_10 ~chi_20)
     3.30953487E-02    2     1000022   1000025   # BR(H -> ~chi_10 ~chi_30)
     3.30953487E-02    2     1000022   1000035   # BR(H -> ~chi_10 ~chi_40)
     5.22515333E-02    2     1000023   1000025   # BR(H -> ~chi_20 ~chi_30)
     5.22515333E-02    2     1000023   1000035   # BR(H -> ~chi_20 ~chi_40)
     5.22515333E-02    2     1000025   1000035   # BR(H -> ~chi_30 ~chi_40)
#
#         PDG            Width
DECAY        36     4.28802674E-01   # A decays
#          BR         NDA      ID1       ID2
    -1.55405765E-03    2           5        -5   # BR(A -> b       bb     )
     1.81296101E-01    2         -15        15   # BR(A -> tau+    tau-   )
     6.40899983E-04    2         -13        13   # BR(A -> mu+     mu-    )
     1.06839001E-03    2           3        -3   # BR(A -> s       sb     )
     5.32911397E-06    2           4        -4   # BR(A -> c       cb     )
     3.51193321E-01    2           6        -6   # BR(A -> t       tb     )
     1.70530600E-03    2          21        21   # BR(A -> g       g      )
     5.57854613E-06    2          22        22   # BR(A -> gam     gam    )
     1.09623588E-06    2          23        22   # BR(A -> Z       gam    )
     4.28392098E-03    2          23        25   # BR(A -> Z       h      )
     3.93439429E-01    2     1000022   1000022   # BR(A -> ~chi_10 ~chi_10)
     7.30805342E-03    2     1000023   1000023   # BR(A -> ~chi_20 ~chi_20)
     7.30805342E-03    2     1000025   1000025   # BR(A -> ~chi_30 ~chi_30)
     7.30805342E-03    2     1000035   1000035   # BR(A -> ~chi_40 ~chi_40)
     7.14068293E-04    2     1000022   1000023   # BR(A -> ~chi_10 ~chi_20)
     7.14068293E-04    2     1000022   1000025   # BR(A -> ~chi_10 ~chi_30)
     7.14068293E-04    2     1000022   1000035   # BR(A -> ~chi_10 ~chi_40)
     1.46161068E-02    2     1000023   1000025   # BR(A -> ~chi_20 ~chi_30)
     1.46161068E-02    2     1000023   1000035   # BR(A -> ~chi_20 ~chi_40)
     1.46161068E-02    2     1000025   1000035   # BR(A -> ~chi_30 ~chi_40)
#
#         PDG            Width
DECAY        37     7.39768709E-01   # H+ decays
#          BR         NDA      ID1       ID2
     1.29309542E-03    2           4        -5   # BR(H+ -> c       bb     )
     1.07332262E-01    2         -15        16   # BR(H+ -> tau+    nu_tau )
     3.79429727E-04    2         -13        14   # BR(H+ -> mu+     nu_mu  )
     8.27534994E-06    2           2        -5   # BR(H+ -> u       bb     )
     2.99043681E-05    2           2        -3   # BR(H+ -> u       sb     )
     6.18051235E-04    2           4        -3   # BR(H+ -> c       sb     )
     8.87507918E-01    2           6        -5   # BR(H+ -> t       bb     )
     2.83098990E-03    2          24        25   # BR(H+ -> W+      h      )
     7.37617799E-08    2          24        36   # BR(H+ -> W+      A      )
"""

import FWCore.ParameterSet.Config as cms

from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.Pythia8CUEP8M1Settings_cfi import *

generator = cms.EDFilter("Pythia8GeneratorFilter",
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(0.1875),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(COM_ENERGY),
    crossSection = cms.untracked.double(CROSS_SECTION),
    maxEventsToPrint = cms.untracked.int32(1000),
    SLHATableForPythia8 = cms.string('%s' % SLHA_TABLE),
    PythiaParameters = cms.PSet(
        pythia8CommonSettingsBlock,
        pythia8CUEP8M1SettingsBlock,
        processParameters = cms.vstring(
            'SUSY:all = off',
# Indirect neutralino production from squark decay.
            'SUSY:gg2squarkantisquark  = on',
            'SUSY:qqbar2squarkantisquark= on',
            'SUSY:qq2squarksquark = on',
# Direct neutralino pair production via weak process.
#            'SUSY:qqbar2chi0chi0 = on',
#            'RHadrons:allow  = on',
#            'RHadrons:allowDecay = on',
#            'RHadrons:setMasses = on',
             # Pythia calculates the lifetime from the width in the SLHA file, so this shouldn't matter.
            '1000022:tau0 = %.1f' % CHIZERO1_CTAU,
            '1000023:tau0 = %.1f' % CHIZERO2_CTAU,
            '1000025:tau0 = %.1f' % CHIZERO3_CTAU,
            '1000035:tau0 = %.1f' % CHIZERO4_CTAU,
             # Allow particles of any lifetime to decay in Pythia. (Unnecessary in Pythia8)
             # 'ParticleDecays:limitTau0 = off'
        ),
        parameterSets = cms.vstring(
            'pythia8CommonSettings',
            'pythia8CUEP8M1Settings',
            'processParameters'
        )
    )
)

#== Veto events containing two neutralinos of different species.
pairFilterChi1 = cms.EDFilter("MCParticlePairFilter",
    ParticleID1 = cms.untracked.vint32(1000022),
    ParticleID2 = cms.untracked.vint32(1000023,1000025,1000035)
)
pairFilterChi2 = cms.EDFilter("MCParticlePairFilter",
    ParticleID1 = cms.untracked.vint32(1000023),
    ParticleID2 = cms.untracked.vint32(1000022,1000025,1000035)
)
pairFilterChi3 = cms.EDFilter("MCParticlePairFilter",
    ParticleID1 = cms.untracked.vint32(1000025),
    ParticleID2 = cms.untracked.vint32(1000022,1000023,1000035)
)
pairFilterChi4 = cms.EDFilter("MCParticlePairFilter",
    ParticleID1 = cms.untracked.vint32(1000035),
    ParticleID2 = cms.untracked.vint32(1000022,1000023,1000025)
)

# Take logical AND of the NOT of each of the  above filters, so selecting events with a pair of identical 
# neutralinos.
pairFilterSequence = cms.Sequence(~pairFilterChi1 * ~pairFilterChi2 * ~pairFilterChi3 * ~pairFilterChi4)

# N.B. If your PYUPDA tables introduces new exotic particles, you will need
# to include:
#
from PhysicsTools.HepMCCandAlgos.genParticles_cfi import *
genParticlesForFilter = genParticles.clone()
genParticlesForFilter.abortOnUnknownPDGCode = cms.untracked.bool(False)

#== Require event to contain at least one neutralino that decays leptonically.
from GeneratorInterface.GenFilters.XtoFFbarFilter_cfi import *
chiZeroToLLbarFilter = XtoFFbarFilter.clone(
  src = cms.InputTag("genParticlesForFilter"),
# Specify PDG codes of exotics to be accepted by filter.
  idMotherX = cms.vint32(1000022,1000023,1000025,1000035),
  idMotherY = cms.vint32(),
# Allowed PDG ID of daughter of X (don't specify anti-particle code)
  idDaughterF = cms.vint32(11,13),
# Allowed PDG ID of daughter of Y (don't specify anti-particle code)
  idDaughterG = cms.vint32()
# If this is set true, then parameter idMotherY is ignored, and instead set equal to idMotherX.
# Furthermore, events are vetoed if they contain more than one species from the list idMotherX. 
# idYequalsX = cms.bool(False)
)

ProductionFilterSequence = cms.Sequence(generator*pairFilterSequence*genParticlesForFilter*chiZeroToLLbarFilter)

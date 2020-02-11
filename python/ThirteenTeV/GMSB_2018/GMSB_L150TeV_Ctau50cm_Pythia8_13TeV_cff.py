
SLHA_TABLE = '''
#  ISAJET SUSY parameters in SUSY Les Houches Accord 2 format
#  Created by ISALHA 2.0 Last revision: H Baer 27 May 2014
Block SPINFO   # Program information
     1   ISASUGRA/ISASUSY from ISAJET   # Spectrum Calculator
     2   7.88   02-JAN-2018 11:01:14    # Version number
Block MODSEL   # Model selection
     1     2   # Minimal gauge mediated (GMSB) model               
Block SMINPUTS   # Standard Model inputs
     1     1.28000000E+02   # alpha_em^(-1)
     2     1.16570000E-05   # G_Fermi
     3     1.19999997E-01   # alpha_s(M_Z)
     4     9.11699982E+01   # m_{Z}(pole)
     5     4.19999981E+00   # m_{b}(m_{b})
     6     1.73100006E+02   # m_{top}(pole)
     7     1.77699995E+00   # m_{tau}(pole)
Block MINPAR   # SUSY breaking input parameters
     1     1.50000000E+05   # Lambda scale of soft SSB
     2     3.00000000E+05   # M_mess overall messenger scale
     3     1.50000000E+01   # tan(beta)
     4     1.00000000E+00   # sign(mu)
     5     1.00000000E+00   # N_5 messenger index
     6     9.65383987E+01   # c_grav gravitino mass factor
    51     1.00000000E+00   # N5_1  U(1)_Y messenger index
    52     1.00000000E+00   # N5_2 SU(2)_L messenger index
    53     1.00000000E+00   # N5_3 SU(3)_C messenger index
   101     1.00000000E+00   # Rsl
   102     0.00000000E+00   # dmH_d^2
   103     0.00000000E+00   # dmH_u^2
   104     0.00000000E+00   # d_Y
Block MASS   # Scalar and gaugino mass spectrum
#  PDG code   mass                 particle
         6     1.73100006E+02   #  top            
        24     8.04229965E+01   #  W^+
        25     1.15257454E+02   #  h^0            
        35     7.75676636E+02   #  H^0            
        36     7.70498718E+02   #  A^0            
        37     7.79765015E+02   #  H^+            
   1000001     1.63067773E+03   #  dnl            
   1000002     1.62869592E+03   #  upl            
   1000003     1.63067773E+03   #  stl            
   1000004     1.62869641E+03   #  chl            
   1000005     1.54123206E+03   #  b1             
   1000006     1.41675330E+03   #  t1             
   1000011     5.33721985E+02   #  el-            
   1000012     5.24308228E+02   #  nuel           
   1000013     5.33721985E+02   #  mul-           
   1000014     5.24308228E+02   #  numl           
   1000015     2.60203400E+02   #  tau1           
   1000016     5.21300720E+02   #  nutl           
   1000021     1.20669397E+03   #  glss           
   1000022     2.12120834E+02   #  z1ss           
   1000023     4.04282532E+02   #  z2ss           
   1000024     4.04834717E+02   #  w1ss           
   1000025    -5.88877930E+02   #  z3ss           
   1000035     6.06467773E+02   #  z4ss           
   1000037     6.06392151E+02   #  w2ss           
   1000039     1.04505875E-06   #  gvss
   2000001     1.55092615E+03   #  dnr            
   2000002     1.55760840E+03   #  upr            
   2000003     1.55092615E+03   #  str            
   2000004     1.55760913E+03   #  chr            
   2000005     1.56845886E+03   #  b2             
   2000006     1.58141382E+03   #  t2             
   2000011     2.60437805E+02   #  er-            
   2000013     2.60437805E+02   #  mur-           
   2000015     5.31612732E+02   #  tau2           
Block ALPHA   # Effective Higgs mixing parameter
         -6.89145699E-02   # alpha
Block STOPMIX   # stop mixing matrix
  1  1     7.31927305E-02   # O_{11}
  1  2     9.97317791E-01   # O_{12}
  2  1    -9.97317791E-01   # O_{21}
  2  2     7.31927305E-02   # O_{22}
Block SBOTMIX   # sbottom mixing matrix
  1  1     2.91983813E-01   # O_{11}
  1  2     9.56423283E-01   # O_{12}
  2  1    -9.56423283E-01   # O_{21}
  2  2     2.91983813E-01   # O_{22}
Block STAUMIX   # stau mixing matrix
  1  1     6.82063177E-02   # O_{11}
  1  2     9.97671247E-01   # O_{12}
  2  1    -9.97671247E-01   # O_{21}
  2  2     6.82063177E-02   # O_{22}
Block NMIX   # neutralino mixing matrix
  1  1     9.95301366E-01   #
  1  2    -1.65267047E-02   #
  1  3     8.81438702E-02   #
  1  4    -3.65044884E-02   #
  2  1     4.38650511E-02   #
  2  2     9.52582121E-01   #
  2  3    -2.43164480E-01   #
  2  4     1.77579150E-01   #
  3  1     3.54977734E-02   #
  3  2    -4.91524190E-02   #
  3  3    -7.03363895E-01   #
  3  4    -7.08239377E-01   #
  4  1     7.86817744E-02   #
  4  2    -2.99830496E-01   #
  4  3    -6.62103355E-01   #
  4  4     6.82297468E-01   #
Block UMIX   # chargino U mixing matrix
  1  1    -9.40547407E-01   # U_{11}
  1  2     3.39662373E-01   # U_{12}
  2  1    -3.39662373E-01   # U_{21}
  2  2    -9.40547407E-01   # U_{22}
Block VMIX   # chargino V mixing matrix
  1  1    -9.69057024E-01   # V_{11}
  1  2     2.46836960E-01   # V_{12}
  2  1    -2.46836960E-01   # V_{21}
  2  2    -9.69057024E-01   # V_{22}
Block GAUGE Q=  1.44477710E+03   #
     1     3.57524991E-01   # g`
     2     6.52378619E-01   # g_2
     3     1.21928000E+00   # g_3
Block YU Q=  1.44477710E+03   #
  3  3     8.56151879E-01   # y_t
Block YD Q=  1.44477710E+03   #
  3  3     1.93365723E-01   # y_b
Block YE Q=  1.44477710E+03   #
  3  3     1.52594313E-01   # y_tau
Block HMIX Q=  1.44477710E+03   # Higgs mixing parameters
     1     5.80108521E+02   # mu(Q)
     2     1.44371462E+01   # tan(beta)(Q)
     3     2.51120468E+02   # Higgs vev at Q
     4     5.93668250E+05   # m_A^2(Q)
Block MSOFT Q=  1.44477710E+03   # DRbar SUSY breaking parameters
     1     2.17775024E+02   # M_1(Q)          
     2     4.05397003E+02   # M_2(Q)          
     3     1.09064551E+03   # M_3(Q)          
    21     2.45626734E+05   # MHd^2(Q)        
    22    -3.02578062E+05   # MHu^2(Q)        
    31     5.27866882E+02   # MeL(Q)          
    32     5.27866882E+02   # MmuL(Q)         
    33     5.24929504E+02   # MtauL(Q)        
    34     2.57610229E+02   # MeR(Q)          
    35     2.57610229E+02   # MmuR(Q)         
    36     2.53839172E+02   # MtauR(Q)        
    41     1.57540186E+03   # MqL1(Q)         
    42     1.57540186E+03   # MqL2(Q)         
    43     1.51619727E+03   # MqL3(Q)         
    44     1.50283862E+03   # MuR(Q)          
    45     1.50283862E+03   # McR(Q)          
    46     1.37672119E+03   # MtR(Q)          
    47     1.49542053E+03   # MdR(Q)          
    48     1.49542053E+03   # MsR(Q)          
    49     1.48986389E+03   # MbR(Q)          
Block AU Q=  1.44477710E+03   #
  1  1    -3.30153137E+02   # A_u
  2  2    -3.30153137E+02   # A_c
  3  3    -3.30153137E+02   # A_t
Block AD Q=  1.44477710E+03   #
  1  1    -3.69239410E+02   # A_d
  2  2    -3.69239410E+02   # A_s
  3  3    -3.69239410E+02   # A_b
Block AE Q=  1.44477710E+03   #
  1  1    -3.95861778E+01   # A_e
  2  2    -3.95861778E+01   # A_mu
  3  3    -3.95861778E+01   # A_tau
#  ISAJET decay tables in SUSY Les Houches accord format
#  Created by ISALHD. Last revision: C. Balazs, 2005 May 25
Block DCINFO                           # Program information
     1   ISASUGRA from ISAJET          # Spectrum Calculator
     2   7.88   02-JAN-2018 11:01:14   # Version number
#         PDG         Width
DECAY         6  1.48575687E+00   # TP    decays
#          BR          NDA       ID1       ID2       ID3       ID4
      3.33333313E-01    3            2        -1         5             # TP     -->  UP     DB     BT          
      3.33333313E-01    3            4        -3         5             # TP     -->  CH     SB     BT          
      1.11111097E-01    3          -11        12         5             # TP     -->  E+     NUE    BT          
      1.11111097E-01    3          -13        14         5             # TP     -->  MU+    NUM    BT          
      1.11111097E-01    3          -15        16         5             # TP     -->  TAU+   NUT    BT          
#         PDG         Width
DECAY   1000021  2.89846212E-02   # GLSS  decays
#          BR          NDA       ID1       ID2       ID3       ID4
      6.81992620E-02    3      1000024         1        -2             # GLSS   -->  W1SS+  DN     UB          
      6.81992620E-02    3     -1000024         2        -1             # GLSS   -->  W1SS-  UP     DB          
      6.81992024E-02    3      1000024         3        -4             # GLSS   -->  W1SS+  ST     CB          
      6.81992024E-02    3     -1000024         4        -3             # GLSS   -->  W1SS-  CH     SB          
      5.41609712E-02    3      1000024         5        -6             # GLSS   -->  W1SS+  BT     TB          
      5.41609712E-02    3     -1000024         6        -5             # GLSS   -->  W1SS-  TP     BB          
      2.79660104E-03    3      1000037         1        -2             # GLSS   -->  W2SS+  DN     UB          
      2.79660104E-03    3     -1000037         2        -1             # GLSS   -->  W2SS-  UP     DB          
      2.79659918E-03    3      1000037         3        -4             # GLSS   -->  W2SS+  ST     CB          
      2.79659918E-03    3     -1000037         4        -3             # GLSS   -->  W2SS-  CH     SB          
      7.54439682E-02    3      1000037         5        -6             # GLSS   -->  W2SS+  BT     TB          
      7.54439682E-02    3     -1000037         6        -5             # GLSS   -->  W2SS-  TP     BB          
      3.43112970E-06    2      1000022        21                       # GLSS   -->  Z1SS   GL                 
      3.98591757E-02    3      1000022         2        -2             # GLSS   -->  Z1SS   UP     UB          
      1.20224962E-02    3      1000022         1        -1             # GLSS   -->  Z1SS   DN     DB          
      1.20224962E-02    3      1000022         3        -3             # GLSS   -->  Z1SS   ST     SB          
      3.98590676E-02    3      1000022         4        -4             # GLSS   -->  Z1SS   CH     CB          
      1.31045561E-02    3      1000022         5        -5             # GLSS   -->  Z1SS   BT     BB          
      3.79431993E-02    3      1000022         6        -6             # GLSS   -->  Z1SS   TP     TB          
      6.08985778E-04    2      1000023        21                       # GLSS   -->  Z2SS   GL                 
      3.47379856E-02    3      1000023         2        -2             # GLSS   -->  Z2SS   UP     UB          
      3.33178937E-02    3      1000023         1        -1             # GLSS   -->  Z2SS   DN     DB          
      3.33178937E-02    3      1000023         3        -3             # GLSS   -->  Z2SS   ST     SB          
      3.47379297E-02    3      1000023         4        -4             # GLSS   -->  Z2SS   CH     CB          
      4.32726182E-02    3      1000023         5        -5             # GLSS   -->  Z2SS   BT     BB          
      1.50960684E-02    3      1000023         6        -6             # GLSS   -->  Z2SS   TP     TB          
      4.99100797E-03    2      1000025        21                       # GLSS   -->  Z3SS   GL                 
      2.02833580E-05    3      1000025         2        -2             # GLSS   -->  Z3SS   UP     UB          
      2.45927531E-05    3      1000025         1        -1             # GLSS   -->  Z3SS   DN     DB          
      2.45927531E-05    3      1000025         3        -3             # GLSS   -->  Z3SS   ST     SB          
      2.02833071E-05    3      1000025         4        -4             # GLSS   -->  Z3SS   CH     CB          
      4.56616702E-03    3      1000025         5        -5             # GLSS   -->  Z3SS   BT     BB          
      2.90887393E-02    3      1000025         6        -6             # GLSS   -->  Z3SS   TP     TB          
      4.40470735E-03    2      1000035        21                       # GLSS   -->  Z4SS   GL                 
      1.38745841E-03    3      1000035         2        -2             # GLSS   -->  Z4SS   UP     UB          
      1.60228217E-03    3      1000035         1        -1             # GLSS   -->  Z4SS   DN     DB          
      1.60228217E-03    3      1000035         3        -3             # GLSS   -->  Z4SS   ST     SB          
      1.38745573E-03    3      1000035         4        -4             # GLSS   -->  Z4SS   CH     CB          
      5.76041080E-03    3      1000035         5        -5             # GLSS   -->  Z4SS   BT     BB          
      5.20226397E-02    3      1000035         6        -6             # GLSS   -->  Z4SS   TP     TB          
      9.30514554E-11    2      1000039        21                       # GLSS   -->  GVSS   GL                 
#         PDG         Width
DECAY   1000002  3.75090065E+01   # UPL   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      4.85655060E-03    2      1000022         2                       # UPL    -->  Z1SS   UP                 
      1.48518354E-01    2      1000023         2                       # UPL    -->  Z2SS   UP                 
      2.51214457E-04    2      1000025         2                       # UPL    -->  Z3SS   UP                 
      1.10465297E-02    2      1000035         2                       # UPL    -->  Z4SS   UP                 
      5.16632020E-01    2      1000021         2                       # UPL    -->  GLSS   UP                 
      3.02169770E-01    2      1000024         1                       # UPL    -->  W1SS+  DN                 
      1.65255554E-02    2      1000037         1                       # UPL    -->  W2SS+  DN                 
#         PDG         Width
DECAY   1000001  3.75598946E+01   # DNL   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      6.98527927E-03    2      1000022         1                       # DNL    -->  Z1SS   DN                 
      1.43615767E-01    2      1000023         1                       # DNL    -->  Z2SS   DN                 
      4.28061001E-04    2      1000025         1                       # DNL    -->  Z3SS   DN                 
      1.33994827E-02    2      1000035         1                       # DNL    -->  Z4SS   DN                 
      5.19556582E-01    2      1000021         1                       # DNL    -->  GLSS   DN                 
      2.84702957E-01    2     -1000024         2                       # DNL    -->  W1SS-  UP                 
      3.13118994E-02    2     -1000037         2                       # DNL    -->  W2SS-  UP                 
#         PDG         Width
DECAY   1000003  3.75598755E+01   # STL   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      6.98528299E-03    2      1000022         3                       # STL    -->  Z1SS   ST                 
      1.43615842E-01    2      1000023         3                       # STL    -->  Z2SS   ST                 
      4.28061234E-04    2      1000025         3                       # STL    -->  Z3SS   ST                 
      1.33994892E-02    2      1000035         3                       # STL    -->  Z4SS   ST                 
      5.19556820E-01    2      1000021         3                       # STL    -->  GLSS   ST                 
      2.84702629E-01    2     -1000024         4                       # STL    -->  W1SS-  CH                 
      3.13118584E-02    2     -1000037         4                       # STL    -->  W2SS-  CH                 
#         PDG         Width
DECAY   1000004  3.75088997E+01   # CHL   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      4.85655898E-03    2      1000022         4                       # CHL    -->  Z1SS   CH                 
      1.48518592E-01    2      1000023         4                       # CHL    -->  Z2SS   CH                 
      2.51214835E-04    2      1000025         4                       # CHL    -->  Z3SS   CH                 
      1.10465456E-02    2      1000035         4                       # CHL    -->  Z4SS   CH                 
      5.16630769E-01    2      1000021         4                       # CHL    -->  GLSS   CH                 
      3.02170724E-01    2      1000024         3                       # CHL    -->  W1SS+  ST                 
      1.65256113E-02    2      1000037         3                       # CHL    -->  W2SS+  ST                 
#         PDG         Width
DECAY   1000005  1.87745609E+01   # BT1   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      4.59881872E-02    2      1000022         5                       # BT1    -->  Z1SS   BT                 
      4.22851183E-02    2      1000023         5                       # BT1    -->  Z2SS   BT                 
      2.00922079E-02    2      1000025         5                       # BT1    -->  Z3SS   BT                 
      8.68590642E-03    2      1000035         5                       # BT1    -->  Z4SS   BT                 
      7.28064835E-01    2      1000021         5                       # BT1    -->  GLSS   BT                 
      8.33717659E-02    2     -1000024         6                       # BT1    -->  W1SS-  TP                 
      7.14160725E-02    2     -1000037         6                       # BT1    -->  W2SS-  TP                 
      9.58872697E-05    2          -24   1000006                       # BT1    -->  W-     TP1                
#         PDG         Width
DECAY   1000006  3.33991432E+01   # TP1   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      1.13187879E-01    2      1000021         6                       # TP1    -->  GLSS   TP                 
      8.86589140E-02    2      1000022         6                       # TP1    -->  Z1SS   TP                 
      2.25969758E-02    2      1000023         6                       # TP1    -->  Z2SS   TP                 
      2.01451853E-01    2      1000025         6                       # TP1    -->  Z3SS   TP                 
      1.68596715E-01    2      1000035         6                       # TP1    -->  Z4SS   TP                 
      4.54972908E-02    2      1000024         5                       # TP1    -->  W1SS+  BT                 
      3.60010356E-01    2      1000037         5                       # TP1    -->  W2SS+  BT                 
#         PDG         Width
DECAY   2000002  1.80091648E+01   # UPR   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      1.86564982E-01    2      1000022         2                       # UPR    -->  Z1SS   UP                 
      3.27219284E-04    2      1000023         2                       # UPR    -->  Z2SS   UP                 
      1.80972347E-04    2      1000025         2                       # UPR    -->  Z3SS   UP                 
      8.71225609E-04    2      1000035         2                       # UPR    -->  Z4SS   UP                 
      8.12055588E-01    2      1000021         2                       # UPR    -->  GLSS   UP                 
#         PDG         Width
DECAY   2000001  1.50348740E+01   # DNR   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      5.56102730E-02    2      1000022         1                       # DNR    -->  Z1SS   DN                 
      9.74459035E-05    2      1000023         1                       # DNR    -->  Z2SS   DN                 
      5.38055538E-05    2      1000025         1                       # DNR    -->  Z3SS   DN                 
      2.58973829E-04    2      1000035         1                       # DNR    -->  Z4SS   DN                 
      9.43979502E-01    2      1000021         1                       # DNR    -->  GLSS   DN                 
#         PDG         Width
DECAY   2000003  1.50348740E+01   # STR   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      5.56102730E-02    2      1000022         3                       # STR    -->  Z1SS   ST                 
      9.74459035E-05    2      1000023         3                       # STR    -->  Z2SS   ST                 
      5.38055538E-05    2      1000025         3                       # STR    -->  Z3SS   ST                 
      2.58973829E-04    2      1000035         3                       # STR    -->  Z4SS   ST                 
      9.43979502E-01    2      1000021         3                       # STR    -->  GLSS   ST                 
#         PDG         Width
DECAY   2000004  1.80090694E+01   # CHR   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      1.86565772E-01    2      1000022         4                       # CHR    -->  Z1SS   CH                 
      3.27220652E-04    2      1000023         4                       # CHR    -->  Z2SS   CH                 
      1.80973060E-04    2      1000025         4                       # CHR    -->  Z3SS   CH                 
      8.71229102E-04    2      1000035         4                       # CHR    -->  Z4SS   CH                 
      8.12054813E-01    2      1000021         4                       # CHR    -->  GLSS   CH                 
#         PDG         Width
DECAY   2000005  4.63343010E+01   # BT2   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      5.09223854E-03    2      1000022         5                       # BT2    -->  Z1SS   BT                 
      9.59487781E-02    2      1000023         5                       # BT2    -->  Z2SS   BT                 
      1.10309739E-02    2      1000025         5                       # BT2    -->  Z3SS   BT                 
      2.32683122E-02    2      1000035         5                       # BT2    -->  Z4SS   BT                 
      3.28632861E-01    2      1000021         5                       # BT2    -->  GLSS   BT                 
      1.96992010E-01    2     -1000024         6                       # BT2    -->  W1SS-  TP                 
      3.38025898E-01    2     -1000037         6                       # BT2    -->  W2SS-  TP                 
      1.00899034E-03    2          -24   1000006                       # BT2    -->  W-     TP1                
#         PDG         Width
DECAY   2000006  4.73359070E+01   # TP2   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      2.76327282E-01    2      1000021         6                       # TP2    -->  GLSS   TP                 
      2.20960647E-01    2      1000024         5                       # TP2    -->  W1SS+  BT                 
      3.99595797E-02    2      1000037         5                       # TP2    -->  W2SS+  BT                 
      6.51078648E-04    2           23   1000006                       # TP2    -->  Z0     TP1                
      2.47832318E-03    2           25   1000006                       # TP2    -->  HL0    TP1                
      3.87403346E-03    2      1000022         6                       # TP2    -->  Z1SS   TP                 
      1.12571657E-01    2      1000023         6                       # TP2    -->  Z2SS   TP                 
      1.66292906E-01    2      1000025         6                       # TP2    -->  Z3SS   TP                 
      1.76884547E-01    2      1000035         6                       # TP2    -->  Z4SS   TP                 
#         PDG         Width
DECAY   1000011  1.55445719E+00   # EL-   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      2.88408488E-01    2      1000022        11                       # EL-    -->  Z1SS   E-                 
      2.50464320E-01    2      1000023        11                       # EL-    -->  Z2SS   E-                 
      4.61126924E-01    2     -1000024        12                       # EL-    -->  W1SS-  NUE                
      1.41428131E-07    3      1000015        11       -15             # EL-    -->  TAU1-  E-     TAU+        
      1.52477625E-07    3     -1000015        11        15             # EL-    -->  TAU1+  E-     TAU-        
      2.93698510E-14    2           11   1000039                       # EL-    -->  E-     GVSS               
#         PDG         Width
DECAY   1000013  1.55445707E+00   # MUL-  decays
#          BR          NDA       ID1       ID2       ID3       ID4
      2.88408488E-01    2      1000022        13                       # MUL-   -->  Z1SS   MU-                
      2.50464261E-01    2      1000023        13                       # MUL-   -->  Z2SS   MU-                
      4.61126953E-01    2     -1000024        14                       # MUL-   -->  W1SS-  NUM                
      1.41428131E-07    3      1000015        13       -15             # MUL-   -->  TAU1-  MU-    TAU+        
      1.52477639E-07    3     -1000015        13        15             # MUL-   -->  TAU1+  MU-    TAU-        
      2.93698544E-14    2           13   1000039                       # MUL-   -->  MU-    GVSS               
#         PDG         Width
DECAY   1000015  1.47312775E-01   # TAU1- decays
#          BR          NDA       ID1       ID2       ID3       ID4
      1.00000000E+00    2      1000022        15                       # TAU1-  -->  Z1SS   TAU-               
      8.53392132E-15    2           15   1000039                       # TAU1-  -->  TAU-   GVSS               
#         PDG         Width
DECAY   1000012  1.47892344E+00   # NUEL  decays
#          BR          NDA       ID1       ID2       ID3       ID4
      3.31502914E-01    2      1000022        12                       # NUEL   -->  Z1SS   NUE                
      2.11466834E-01    2      1000023        12                       # NUEL   -->  Z2SS   NUE                
      4.57028717E-01    2      1000024        11                       # NUEL   -->  W1SS+  E-                 
      2.48563623E-07    3      1000015        12       -15             # NUEL   -->  TAU1-  NUE    TAU+        
      1.87748796E-07    3     -1000015        12        15             # NUEL   -->  TAU1+  NUE    TAU-        
      1.13648616E-06    3     -1000015        11        16             # NUEL   -->  TAU1+  E-     NUT         
      2.82418250E-14    2           12   1000039                       # NUEL   -->  NUE    GVSS               
#         PDG         Width
DECAY   1000014  1.47892320E+00   # NUML  decays
#          BR          NDA       ID1       ID2       ID3       ID4
      3.31502974E-01    2      1000022        14                       # NUML   -->  Z1SS   NUM                
      2.11466864E-01    2      1000023        14                       # NUML   -->  Z2SS   NUM                
      4.57028598E-01    2      1000024        13                       # NUML   -->  W1SS+  MU-                
      2.48563680E-07    3      1000015        14       -15             # NUML   -->  TAU1-  NUM    TAU+        
      1.87748824E-07    3     -1000015        14        15             # NUML   -->  TAU1+  NUM    TAU-        
      1.13722251E-06    3     -1000015        13        16             # NUML   -->  TAU1+  MU-    NUT         
      2.82418284E-14    2           14   1000039                       # NUML   -->  NUM    GVSS               
#         PDG         Width
DECAY   1000016  1.59188509E+00   # NUTL  decays
#          BR          NDA       ID1       ID2       ID3       ID4
      3.04827154E-01    2      1000022        16                       # NUTL   -->  Z1SS   NUT                
      1.88760757E-01    2      1000023        16                       # NUTL   -->  Z2SS   NUT                
      4.09750283E-01    2      1000024        15                       # NUTL   -->  W1SS+  TAU-               
      9.66611281E-02    2           24   1000015                       # NUTL   -->  W+     TAU1-              
      4.12666878E-07    3     -1000015        16        15             # NUTL   -->  TAU1+  NUT    TAU-        
      1.64333656E-07    3      1000015        16       -15             # NUTL   -->  TAU1-  NUT    TAU+        
      2.54938198E-14    2           16   1000039                       # NUTL   -->  NUT    GVSS               
#         PDG         Width
DECAY   2000011  1.48698553E-01   # ER-   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      1.00000000E+00    2      1000022        11                       # ER-    -->  Z1SS   E-                 
      5.60248591E-28    3      1000015        12       -16             # ER-    -->  TAU1-  NUE    ANUT        
      8.49412093E-15    2           11   1000039                       # ER-    -->  E-     GVSS               
#         PDG         Width
DECAY   2000013  1.48698121E-01   # MUR-  decays
#          BR          NDA       ID1       ID2       ID3       ID4
      1.00000000E+00    2      1000022        13                       # MUR-   -->  Z1SS   MU-                
      2.38478316E-23    3      1000015        14       -16             # MUR-   -->  TAU1-  NUM    ANUT        
      8.49414211E-15    2           13   1000039                       # MUR-   -->  MU-    GVSS               
#         PDG         Width
DECAY   2000015  1.69303417E+00   # TAU2- decays
#          BR          NDA       ID1       ID2       ID3       ID4
      2.65493393E-01    2      1000022        15                       # TAU2-  -->  Z1SS   TAU-               
      2.21787930E-01    2      1000023        15                       # TAU2-  -->  Z2SS   TAU-               
      4.05920714E-01    2     -1000024        16                       # TAU2-  -->  W1SS-  NUT                
      4.87918109E-02    2           23   1000015                       # TAU2-  -->  Z0     TAU1-              
      5.80061525E-02    2           25   1000015                       # TAU2-  -->  HL0    TAU1-              
#         PDG         Width
DECAY   1000022  3.94431824E-16   # Z1SS  decays
#          BR          NDA       ID1       ID2       ID3       ID4
      8.57347310E-01    2      1000039        22                       # Z1SS   -->  GVSS   GM                 
      1.83872022E-02    3      1000039        11       -11             # Z1SS   -->  GVSS   E-     E+          
      1.24135055E-01    2      1000039        23                       # Z1SS   -->  GVSS   Z0                 
      1.30394226E-04    2      1000039        25                       # Z1SS   -->  GVSS   HL0                
#         PDG         Width
DECAY   1000023  4.29373942E-02   # Z2SS  decays
#          BR          NDA       ID1       ID2       ID3       ID4
      2.12917098E-06    2      1000022        22                       # Z2SS   -->  Z1SS   GM                 
      5.33094369E-02    2      1000022        23                       # Z2SS   -->  Z1SS   Z0                 
      2.71946504E-07    3      1000022         2        -2             # Z2SS   -->  Z1SS   UP     UB          
      3.67156133E-07    3      1000022         1        -1             # Z2SS   -->  Z1SS   DN     DB          
      3.67156133E-07    3      1000022         3        -3             # Z2SS   -->  Z1SS   ST     SB          
      2.71946107E-07    3      1000022         4        -4             # Z2SS   -->  Z1SS   CH     CB          
      1.90382138E-06    3      1000022         5        -5             # Z2SS   -->  Z1SS   BT     BB          
      1.80427989E-04    3      1000022        11       -11             # Z2SS   -->  Z1SS   E-     E+          
      1.80427989E-04    3      1000022        13       -13             # Z2SS   -->  Z1SS   MU-    MU+         
      1.82706077E-04    3      1000022        15       -15             # Z2SS   -->  Z1SS   TAU-   TAU+        
      2.06208715E-04    3      1000022        12       -12             # Z2SS   -->  Z1SS   NUE    ANUE        
      2.06208715E-04    3      1000022        14       -14             # Z2SS   -->  Z1SS   NUM    ANUM        
      2.14074622E-04    3      1000022        16       -16             # Z2SS   -->  Z1SS   NUT    ANUT        
      5.60157299E-01    2      1000022        25                       # Z2SS   -->  Z1SS   HL0                
      1.57681685E-02    2      2000011       -11                       # Z2SS   -->  ER-    E+                 
      1.57681685E-02    2     -2000011        11                       # Z2SS   -->  ER+    E-                 
      1.57681685E-02    2      2000013       -13                       # Z2SS   -->  MUR-   MU+                
      1.57681685E-02    2     -2000013        13                       # Z2SS   -->  MUR+   MU-                
      1.61142647E-01    2      1000015       -15                       # Z2SS   -->  TAU1-  TAU+               
      1.61142647E-01    2     -1000015        15                       # Z2SS   -->  TAU1+  TAU-               
      6.55648699E-14    2      1000039        22                       # Z2SS   -->  GVSS   GM                 
      1.47625031E-15    3      1000039        11       -11             # Z2SS   -->  GVSS   E-     E+          
      1.46493074E-13    2      1000039        23                       # Z2SS   -->  GVSS   Z0                 
      2.43043039E-15    2      1000039        25                       # Z2SS   -->  GVSS   HL0                
#         PDG         Width
DECAY   1000025  2.36257219E+00   # Z3SS  decays
#          BR          NDA       ID1       ID2       ID3       ID4
      4.90372010E-08    2      1000022        22                       # Z3SS   -->  Z1SS   GM                 
      4.26831633E-07    2      1000023        22                       # Z3SS   -->  Z2SS   GM                 
      2.73928016E-01    2      1000024       -24                       # Z3SS   -->  W1SS+  W-                 
      2.73928016E-01    2     -1000024        24                       # Z3SS   -->  W1SS-  W+                 
      1.34958193E-01    2      1000022        23                       # Z3SS   -->  Z1SS   Z0                 
      2.46930555E-01    2      1000023        23                       # Z3SS   -->  Z2SS   Z0                 
      7.98537458E-10    3      1000022         2        -2             # Z3SS   -->  Z1SS   UP     UB          
      2.45507031E-10    3      1000022         1        -1             # Z3SS   -->  Z1SS   DN     DB          
      2.45507031E-10    3      1000022         3        -3             # Z3SS   -->  Z1SS   ST     SB          
      7.98535960E-10    3      1000022         4        -4             # Z3SS   -->  Z1SS   CH     CB          
      1.95150960E-06    3      1000022         5        -5             # Z3SS   -->  Z1SS   BT     BB          
      3.20959487E-07    3      1000022        15       -15             # Z3SS   -->  Z1SS   TAU-   TAU+        
      1.45638751E-10    3      1000023         2        -2             # Z3SS   -->  Z2SS   UP     UB          
      2.38382453E-10    3      1000023         1        -1             # Z3SS   -->  Z2SS   DN     DB          
      2.38382453E-10    3      1000023         3        -3             # Z3SS   -->  Z2SS   ST     SB          
      1.45638571E-10    3      1000023         4        -4             # Z3SS   -->  Z2SS   CH     CB          
      2.55241190E-07    3      1000023         5        -5             # Z3SS   -->  Z2SS   BT     BB          
      3.88988397E-08    3      1000023        15       -15             # Z3SS   -->  Z2SS   TAU-   TAU+        
      2.22993959E-02    2      1000022        25                       # Z3SS   -->  Z1SS   HL0                
      5.02910931E-03    2      1000023        25                       # Z3SS   -->  Z2SS   HL0                
      1.46951388E-05    2      1000011       -11                       # Z3SS   -->  EL-    E+                 
      1.46951388E-05    2     -1000011        11                       # Z3SS   -->  EL+    E-                 
      1.46951143E-05    2      1000013       -13                       # Z3SS   -->  MUL-   MU+                
      1.46951143E-05    2     -1000013        13                       # Z3SS   -->  MUL+   MU-                
      5.16845612E-04    2      2000011       -11                       # Z3SS   -->  ER-    E+                 
      5.16845612E-04    2     -2000011        11                       # Z3SS   -->  ER+    E-                 
      5.16845612E-04    2      2000013       -13                       # Z3SS   -->  MUR-   MU+                
      5.16845612E-04    2     -2000013        13                       # Z3SS   -->  MUR+   MU-                
      1.89872906E-02    2      1000015       -15                       # Z3SS   -->  TAU1-  TAU+               
      1.89872906E-02    2     -1000015        15                       # Z3SS   -->  TAU1+  TAU-               
      1.08305865E-03    2      2000015       -15                       # Z3SS   -->  TAU2-  TAU+               
      1.08305865E-03    2     -2000015        15                       # Z3SS   -->  TAU2+  TAU-               
      1.06256768E-04    2      1000012       -12                       # Z3SS   -->  NUEL   ANUE               
      1.06256768E-04    2     -1000012        12                       # Z3SS   -->  ANUEL  NUE                
      1.06256768E-04    2      1000014       -14                       # Z3SS   -->  NUML   ANUM               
      1.06256768E-04    2     -1000014        14                       # Z3SS   -->  ANUML  NUM                
      1.15757677E-04    2      1000016       -16                       # Z3SS   -->  NUTL   ANUT               
      1.15757677E-04    2     -1000016        16                       # Z3SS   -->  ANUTL  NUT                
      1.74607301E-18    2      1000039        22                       # Z3SS   -->  GVSS   GM                 
      4.04030910E-20    3      1000039        11       -11             # Z3SS   -->  GVSS   E-     E+          
      6.34699575E-15    2      1000039        23                       # Z3SS   -->  GVSS   Z0                 
      7.70273975E-15    2      1000039        25                       # Z3SS   -->  GVSS   HL0                
#         PDG         Width
DECAY   1000035  2.90244341E+00   # Z4SS  decays
#          BR          NDA       ID1       ID2       ID3       ID4
      1.62100182E-08    2      1000022        22                       # Z4SS   -->  Z1SS   GM                 
      1.17369737E-07    2      1000023        22                       # Z4SS   -->  Z2SS   GM                 
      2.00787897E-09    2      1000025        22                       # Z4SS   -->  Z3SS   GM                 
      2.99590379E-01    2      1000024       -24                       # Z4SS   -->  W1SS+  W-                 
      2.99590379E-01    2     -1000024        24                       # Z4SS   -->  W1SS-  W+                 
      2.02624910E-02    2      1000022        23                       # Z4SS   -->  Z1SS   Z0                 
      8.55493546E-03    2      1000023        23                       # Z4SS   -->  Z2SS   Z0                 
      1.64892597E-08    3      1000022         2        -2             # Z4SS   -->  Z1SS   UP     UB          
      1.51271422E-08    3      1000022         1        -1             # Z4SS   -->  Z1SS   DN     DB          
      1.51271422E-08    3      1000022         3        -3             # Z4SS   -->  Z1SS   ST     SB          
      1.64892313E-08    3      1000022         4        -4             # Z4SS   -->  Z1SS   CH     CB          
      1.50134576E-06    3      1000022         5        -5             # Z4SS   -->  Z1SS   BT     BB          
      2.45239505E-07    3      1000022        15       -15             # Z4SS   -->  Z1SS   TAU-   TAU+        
      2.32405206E-08    3      1000023         2        -2             # Z4SS   -->  Z2SS   UP     UB          
      2.70900067E-08    3      1000023         1        -1             # Z4SS   -->  Z2SS   DN     DB          
      2.70900067E-08    3      1000023         3        -3             # Z4SS   -->  Z2SS   ST     SB          
      2.32404886E-08    3      1000023         4        -4             # Z4SS   -->  Z2SS   CH     CB          
      2.47776029E-07    3      1000023         5        -5             # Z4SS   -->  Z2SS   BT     BB          
      3.27777769E-08    3      1000023        15       -15             # Z4SS   -->  Z2SS   TAU-   TAU+        
      6.47571170E-08    3      1000025         2        -2             # Z4SS   -->  Z3SS   UP     UB          
      8.35220035E-08    3      1000025         1        -1             # Z4SS   -->  Z3SS   DN     DB          
      8.35220035E-08    3      1000025         3        -3             # Z4SS   -->  Z3SS   ST     SB          
      6.47571170E-08    3      1000025         4        -4             # Z4SS   -->  Z3SS   CH     CB          
      3.27969190E-08    3      1000025         5        -5             # Z4SS   -->  Z3SS   BT     BB          
      1.89447196E-08    3      1000025        11       -11             # Z4SS   -->  Z3SS   E-     E+          
      1.89447196E-08    3      1000025        13       -13             # Z4SS   -->  Z3SS   MU-    MU+         
      1.65280110E-08    3      1000025        15       -15             # Z4SS   -->  Z3SS   TAU-   TAU+        
      3.76940363E-08    3      1000025        12       -12             # Z4SS   -->  Z3SS   NUE    ANUE        
      3.76940363E-08    3      1000025        14       -14             # Z4SS   -->  Z3SS   NUM    ANUM        
      3.76940363E-08    3      1000025        16       -16             # Z4SS   -->  Z3SS   NUT    ANUT        
      1.01677276E-01    2      1000022        25                       # Z4SS   -->  Z1SS   HL0                
      2.01991379E-01    2      1000023        25                       # Z4SS   -->  Z2SS   HL0                
      1.47241028E-03    2      1000011       -11                       # Z4SS   -->  EL-    E+                 
      1.47241028E-03    2     -1000011        11                       # Z4SS   -->  EL+    E-                 
      1.47240877E-03    2      1000013       -13                       # Z4SS   -->  MUL-   MU+                
      1.47240877E-03    2     -1000013        13                       # Z4SS   -->  MUL+   MU-                
      2.18827138E-03    2      2000011       -11                       # Z4SS   -->  ER-    E+                 
      2.18827138E-03    2     -2000011        11                       # Z4SS   -->  ER+    E-                 
      2.18827138E-03    2      2000013       -13                       # Z4SS   -->  MUR-   MU+                
      2.18827138E-03    2     -2000013        13                       # Z4SS   -->  MUR+   MU-                
      1.38089955E-02    2      1000015       -15                       # Z4SS   -->  TAU1-  TAU+               
      1.38089955E-02    2     -1000015        15                       # Z4SS   -->  TAU1+  TAU-               
      2.89983954E-03    2      2000015       -15                       # Z4SS   -->  TAU2-  TAU+               
      2.89983954E-03    2     -2000015        15                       # Z4SS   -->  TAU2+  TAU-               
      3.30250943E-03    2      1000012       -12                       # Z4SS   -->  NUEL   ANUE               
      3.30250943E-03    2     -1000012        12                       # Z4SS   -->  ANUEL  NUE                
      3.30250943E-03    2      1000014       -14                       # Z4SS   -->  NUML   ANUM               
      3.30250943E-03    2     -1000014        14                       # Z4SS   -->  ANUML  NUM                
      3.52986553E-03    2      1000016       -16                       # Z4SS   -->  NUTL   ANUT               
      3.52986553E-03    2     -1000016        16                       # Z4SS   -->  ANUTL  NUT                
      1.69692331E-16    2      1000039        22                       # Z4SS   -->  GVSS   GM                 
      3.93485971E-18    3      1000039        11       -11             # Z4SS   -->  GVSS   E-     E+          
      9.60168428E-15    2      1000039        23                       # Z4SS   -->  GVSS   Z0                 
      5.18692399E-15    2      1000039        25                       # Z4SS   -->  GVSS   HL0                
#         PDG         Width
DECAY   1000024  3.65882814E-02   # W1SS+ decays
#          BR          NDA       ID1       ID2       ID3       ID4
      7.50197671E-07    3      1000022         2        -1             # W1SS+  -->  Z1SS   UP     DB          
      7.50197216E-07    3      1000022         4        -3             # W1SS+  -->  Z1SS   CH     SB          
      4.65550664E-04    3      1000022       -11        12             # W1SS+  -->  Z1SS   E+     NUE         
      4.65550664E-04    3      1000022       -13        14             # W1SS+  -->  Z1SS   MU+    NUM         
      4.78234637E-04    3      1000022       -15        16             # W1SS+  -->  Z1SS   TAU+   NUT         
      6.60680115E-01    2      1000022        24                       # W1SS+  -->  Z1SS   W+                 
      7.23206217E-13    3      1000023       -11        12             # W1SS+  -->  Z2SS   E+     NUE         
      7.23206217E-13    3      1000023       -13        14             # W1SS+  -->  Z2SS   MU+    NUM         
      3.37909043E-01    2     -1000015        16                       # W1SS+  -->  TAU1+  NUT                
#         PDG         Width
DECAY   1000037  2.59318376E+00   # W2SS+ decays
#          BR          NDA       ID1       ID2       ID3       ID4
      2.53174282E-08    3      1000022         2        -1             # W2SS+  -->  Z1SS   UP     DB          
      2.53174175E-08    3      1000022         4        -3             # W2SS+  -->  Z1SS   CH     SB          
      2.37249651E-07    3      1000022       -15        16             # W2SS+  -->  Z1SS   TAU+   NUT         
      1.18757665E-01    2      1000022        24                       # W2SS+  -->  Z1SS   W+                 
      2.00113295E-08    3      1000023         2        -1             # W2SS+  -->  Z2SS   UP     DB          
      2.00113188E-08    3      1000023         4        -3             # W2SS+  -->  Z2SS   CH     SB          
      6.94330566E-08    3      1000023       -15        16             # W2SS+  -->  Z2SS   TAU+   NUT         
      2.96759427E-01    2      1000023        24                       # W2SS+  -->  Z2SS   W+                 
      2.47575201E-07    3      1000025         2        -1             # W2SS+  -->  Z3SS   UP     DB          
      2.47575201E-07    3      1000025         4        -3             # W2SS+  -->  Z3SS   CH     SB          
      8.25228241E-08    3      1000025       -11        12             # W2SS+  -->  Z3SS   E+     NUE         
      8.25228241E-08    3      1000025       -13        14             # W2SS+  -->  Z3SS   MU+    NUM         
      8.25228739E-08    3      1000025       -15        16             # W2SS+  -->  Z3SS   TAU+   NUT         
      3.82075901E-03    2      1000012       -11                       # W2SS+  -->  NUEL   E+                 
      3.82075645E-03    2      1000014       -13                       # W2SS+  -->  NUML   MU+                
      7.30222231E-03    2      1000016       -15                       # W2SS+  -->  NUTL   TAU+               
      5.76530071E-03    2     -1000011        12                       # W2SS+  -->  EL+    NUE                
      5.76530071E-03    2     -1000013        14                       # W2SS+  -->  MUL+   NUM                
      2.64926776E-02    2     -1000015        16                       # W2SS+  -->  TAU1+  NUT                
      6.61414070E-03    2     -2000015        16                       # W2SS+  -->  TAU2+  NUT                
      2.80126721E-01    2      1000024        23                       # W2SS+  -->  W1SS+  Z0                 
      2.63940692E-08    3      1000024         1        -1             # W2SS+  -->  W1SS+  DN     DB          
      2.63940390E-08    3      1000024         3        -3             # W2SS+  -->  W1SS+  ST     SB          
      4.68273704E-08    3      1000024         2        -2             # W2SS+  -->  W1SS+  UP     UB          
      4.68273704E-08    3      1000024         4        -4             # W2SS+  -->  W1SS+  CH     CB          
      2.44773641E-01    2      1000024        25                       # W2SS+  -->  W1SS+  HL0                
#         PDG         Width
DECAY        25  3.10336798E-03   # HL0   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      6.76349821E-09    2           11       -11                       # HL0    -->  E-     E+                 
      2.85565649E-04    2           13       -13                       # HL0    -->  MU-    MU+                
      8.16741660E-02    2           15       -15                       # HL0    -->  TAU-   TAU+               
      8.48861418E-06    2            1        -1                       # HL0    -->  DN     DB                 
      3.42982542E-03    2            3        -3                       # HL0    -->  ST     SB                 
      7.34183073E-01    2            5        -5                       # HL0    -->  BT     BB                 
      2.53373923E-06    2            2        -2                       # HL0    -->  UP     UB                 
      4.76811305E-02    2            4        -4                       # HL0    -->  CH     CB                 
      2.42134673E-03    2           22        22                       # HL0    -->  GM     GM                 
      5.16557023E-02    2           21        21                       # HL0    -->  GL     GL                 
      4.06437693E-03    3           24        11       -12             # HL0    -->  W+     E-     ANUE        
      4.06437693E-03    3           24        13       -14             # HL0    -->  W+     MU-    ANUM        
      4.06437693E-03    3           24        15       -16             # HL0    -->  W+     TAU-   ANUT        
      1.21931303E-02    3           24        -2         1             # HL0    -->  W+     UB     DN          
      1.21931303E-02    3           24        -4         3             # HL0    -->  W+     CB     ST          
      4.06437693E-03    3          -24       -11        12             # HL0    -->  W-     E+     NUE         
      4.06437693E-03    3          -24       -13        14             # HL0    -->  W-     MU+    NUM         
      4.06437693E-03    3          -24       -15        16             # HL0    -->  W-     TAU+   NUT         
      1.21931303E-02    3          -24         2        -1             # HL0    -->  W-     UP     DB          
      1.21931303E-02    3          -24         4        -3             # HL0    -->  W-     CH     SB          
      3.76073236E-04    3           23        12       -12             # HL0    -->  Z0     NUE    ANUE        
      3.76073236E-04    3           23        14       -14             # HL0    -->  Z0     NUM    ANUM        
      3.76073236E-04    3           23        16       -16             # HL0    -->  Z0     NUT    ANUT        
      1.89273866E-04    3           23        11       -11             # HL0    -->  Z0     E-     E+          
      1.89273866E-04    3           23        13       -13             # HL0    -->  Z0     MU-    MU+         
      1.89273866E-04    3           23        15       -15             # HL0    -->  Z0     TAU-   TAU+        
      6.48436253E-04    3           23         2        -2             # HL0    -->  Z0     UP     UB          
      6.48436253E-04    3           23         4        -4             # HL0    -->  Z0     CH     CB          
      8.35345592E-04    3           23         1        -1             # HL0    -->  Z0     DN     DB          
      8.35345592E-04    3           23         3        -3             # HL0    -->  Z0     ST     SB          
      8.35345592E-04    3           23         5        -5             # HL0    -->  Z0     BT     BB          
#         PDG         Width
DECAY        35  2.82916927E+00   # HH0   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      1.04799387E-08    2           11       -11                       # HH0    -->  E-     E+                 
      4.42481862E-04    2           13       -13                       # HH0    -->  MU-    MU+                
      1.26729608E-01    2           15       -15                       # HH0    -->  TAU-   TAU+               
      1.28664215E-05    2            1        -1                       # HH0    -->  DN     DB                 
      5.19868592E-03    2            3        -3                       # HH0    -->  ST     SB                 
      8.00406814E-01    2            5        -5                       # HH0    -->  BT     BB                 
      8.71726441E-11    2            2        -2                       # HH0    -->  UP     UB                 
      1.26653276E-06    2            4        -4                       # HH0    -->  CH     CB                 
      5.16276322E-02    2            6        -6                       # HH0    -->  TP     TB                 
      3.22989422E-07    2           22        22                       # HH0    -->  GM     GM                 
      4.18609161E-05    2           21        21                       # HH0    -->  GL     GL                 
      2.77070299E-04    2           24       -24                       # HH0    -->  W+     W-                 
      1.39628508E-04    2           23        23                       # HH0    -->  Z0     Z0                 
      3.13858525E-03    2      1000022   1000022                       # HH0    -->  Z1SS   Z1SS               
      1.09394677E-02    2      1000022   1000023                       # HH0    -->  Z1SS   Z2SS               
      9.61998187E-04    2           25        25                       # HH0    -->  HL0    HL0                
      3.06083748E-05    2      2000011  -2000011                       # HH0    -->  ER-    ER+                
      3.05700305E-05    2      2000013  -2000013                       # HH0    -->  MUR-   MUR+               
      2.05029100E-05    2      1000015  -1000015                       # HH0    -->  TAU1-  TAU1+              
#         PDG         Width
DECAY        36  2.89333510E+00   # HA0   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      1.01823590E-08    2           11       -11                       # HA0    -->  E-     E+                 
      4.29917563E-04    2           13       -13                       # HA0    -->  MU-    MU+                
      1.23133682E-01    2           15       -15                       # HA0    -->  TAU-   TAU+               
      1.25018096E-05    2            1        -1                       # HA0    -->  DN     DB                 
      5.05136419E-03    2            3        -3                       # HA0    -->  ST     SB                 
      7.77785242E-01    2            5        -5                       # HA0    -->  BT     BB                 
      7.90159535E-11    2            2        -2                       # HA0    -->  UP     UB                 
      1.14896170E-06    2            4        -4                       # HA0    -->  CH     CB                 
      5.68091162E-02    2            6        -6                       # HA0    -->  TP     TB                 
      8.12995609E-07    2           22        22                       # HA0    -->  GM     GM                 
      1.06506835E-04    2           21        21                       # HA0    -->  GL     GL                 
      4.85492684E-03    2      1000022   1000022                       # HA0    -->  Z1SS   Z1SS               
      3.15583833E-02    2      1000022   1000023                       # HA0    -->  Z1SS   Z2SS               
      2.56249274E-04    2           25        23                       # HA0    -->  HL0    Z0                 
#         PDG         Width
DECAY        37  2.52115512E+00   # H+    decays
#          BR          NDA       ID1       ID2       ID3       ID4
      1.18260424E-08    2           12       -11                       # H+     -->  NUE    E+                 
      4.99316782E-04    2           14       -13                       # H+     -->  NUM    MU+                
      1.43010512E-01    2           16       -15                       # H+     -->  NUT    TAU+               
      1.33165595E-05    2            2        -1                       # H+     -->  UP     DB                 
      5.38171874E-03    2            4        -3                       # H+     -->  CH     SB                 
      8.04553807E-01    2            6        -5                       # H+     -->  TP     BB                 
      4.62366156E-02    2      1000024   1000022                       # H+     -->  W1SS+  Z1SS               
      3.04667803E-04    2           25        24                       # H+     -->  HL0    W+                 
'''


import FWCore.ParameterSet.Config as cms

from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.MCTunes2017.PythiaCP5Settings_cfi import *
from Configuration.Generator.PSweightsPythia.PythiaPSweightsSettings_cfi import *


generator = cms.EDFilter("Pythia8GeneratorFilter",
        comEnergy = cms.double(13000.0),
        pythiaHepMCVerbosity = cms.untracked.bool(False),
        pythiaPylistVerbosity = cms.untracked.int32(1),
        filterEfficiency = cms.untracked.double(1.0),
        SLHATableForPythia8 = cms.string('%s' % SLHA_TABLE),
                PythiaParameters = cms.PSet(
                pythia8CommonSettingsBlock,
                pythia8CP5SettingsBlock,
		pythia8PSweightsSettingsBlock,
                processParameters = cms.vstring(
                    'ParticleDecays:limitTau0 = off',
                    'ParticleDecays:tau0Max = 10000000',
                        'SUSY:all on',
                ),
                parameterSets = cms.vstring('pythia8CommonSettings',
                                            'pythia8CP5Settings',
                                            'pythia8PSweightsSettings',
                                            'processParameters')
   )
)

ProductionFilterSequence = cms.Sequence(generator)

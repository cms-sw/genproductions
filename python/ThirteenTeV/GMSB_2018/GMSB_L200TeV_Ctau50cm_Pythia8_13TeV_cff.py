
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
     1     2.00000000E+05   # Lambda scale of soft SSB
     2     4.00000000E+05   # M_mess overall messenger scale
     3     1.50000000E+01   # tan(beta)
     4     1.00000000E+00   # sign(mu)
     5     1.00000000E+00   # N_5 messenger index
     6     1.16903999E+02   # c_grav gravitino mass factor
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
        25     1.16879478E+02   #  h^0            
        35     1.01065356E+03   #  H^0            
        36     1.00396478E+03   #  A^0            
        37     1.01380042E+03   #  H^+            
   1000001     2.12702637E+03   #  dnl            
   1000002     2.12550732E+03   #  upl            
   1000003     2.12702637E+03   #  stl            
   1000004     2.12550781E+03   #  chl            
   1000005     2.00843982E+03   #  b1             
   1000006     1.84240845E+03   #  t1             
   1000011     7.07858704E+02   #  el-            
   1000012     6.99085388E+02   #  nuel           
   1000013     7.07858704E+02   #  mul-           
   1000014     6.99085388E+02   #  numl           
   1000015     3.47515228E+02   #  tau1           
   1000016     6.95129333E+02   #  nutl           
   1000021     1.56451611E+03   #  glss           
   1000022     2.84791931E+02   #  z1ss           
   1000023     5.44361816E+02   #  z2ss           
   1000024     5.44888550E+02   #  w1ss           
   1000025    -7.50561890E+02   #  z3ss           
   1000035     7.66168579E+02   #  z4ss           
   1000037     7.66315369E+02   #  w2ss           
   1000039     2.24981864E-06   #  gvss
   2000001     2.01961279E+03   #  dnr            
   2000002     2.02919128E+03   #  upr            
   2000003     2.01961279E+03   #  str            
   2000004     2.02919177E+03   #  chr            
   2000005     2.04543079E+03   #  b2             
   2000006     2.06019092E+03   #  t2             
   2000011     3.46890686E+02   #  er-            
   2000013     3.46890686E+02   #  mur-           
   2000015     7.04579956E+02   #  tau2           
Block ALPHA   # Effective Higgs mixing parameter
         -6.79674745E-02   # alpha
Block STOPMIX   # stop mixing matrix
  1  1     5.42840585E-02   # O_{11}
  1  2     9.98525560E-01   # O_{12}
  2  1    -9.98525560E-01   # O_{21}
  2  2     5.42840585E-02   # O_{22}
Block SBOTMIX   # sbottom mixing matrix
  1  1     2.13391557E-01   # O_{11}
  1  2     9.76966739E-01   # O_{12}
  2  1    -9.76966739E-01   # O_{21}
  2  2     2.13391557E-01   # O_{22}
Block STAUMIX   # stau mixing matrix
  1  1     4.89766859E-02   # O_{11}
  1  2     9.98799920E-01   # O_{12}
  2  1    -9.98799920E-01   # O_{21}
  2  2     4.89766859E-02   # O_{22}
Block NMIX   # neutralino mixing matrix
  1  1     9.97106194E-01   #
  1  2    -1.00962939E-02   #
  1  3     6.92395419E-02   #
  1  4    -2.97216251E-02   #
  2  1     2.95190942E-02   #
  2  2     9.62476790E-01   #
  2  3    -2.14587018E-01   #
  2  4     1.63460404E-01   #
  3  1     2.74648070E-02   #
  3  2    -3.77828740E-02   #
  3  3    -7.04857707E-01   #
  3  4    -7.07809269E-01   #
  4  1     6.44494370E-02   #
  4  2    -2.68530816E-01   #
  4  3    -6.72557950E-01   #
  4  4     6.86588228E-01   #
Block UMIX   # chargino U mixing matrix
  1  1    -9.54581201E-01   # U_{11}
  1  2     2.97950923E-01   # U_{12}
  2  1    -2.97950923E-01   # U_{21}
  2  2    -9.54581201E-01   # U_{22}
Block VMIX   # chargino V mixing matrix
  1  1    -9.74121094E-01   # V_{11}
  1  2     2.26026803E-01   # V_{12}
  2  1    -2.26026803E-01   # V_{21}
  2  2    -9.74121094E-01   # V_{22}
Block GAUGE Q=  1.88482239E+03   #
     1     3.57524991E-01   # g`
     2     6.52378619E-01   # g_2
     3     1.21928000E+00   # g_3
Block YU Q=  1.88482239E+03   #
  3  3     8.48131120E-01   # y_t
Block YD Q=  1.88482239E+03   #
  3  3     1.90079987E-01   # y_b
Block YE Q=  1.88482239E+03   #
  3  3     1.52722269E-01   # y_tau
Block HMIX Q=  1.88482239E+03   # Higgs mixing parameters
     1     7.40617249E+02   # mu(Q)
     2     1.43916941E+01   # tan(beta)(Q)
     3     2.51379364E+02   # Higgs vev at Q
     4     1.00794531E+06   # m_A^2(Q)
Block MSOFT Q=  1.88482239E+03   # DRbar SUSY breaking parameters
     1     2.91356354E+02   # M_1(Q)          
     2     5.38406189E+02   # M_2(Q)          
     3     1.41720056E+03   # M_3(Q)          
    21     4.36508125E+05   # MHd^2(Q)        
    22    -4.87825438E+05   # MHu^2(Q)        
    31     7.01099487E+02   # MeL(Q)          
    32     7.01099487E+02   # MmuL(Q)         
    33     6.97228760E+02   # MtauL(Q)        
    34     3.45036255E+02   # MeR(Q)          
    35     3.45036255E+02   # MmuR(Q)         
    36     3.39970886E+02   # MtauR(Q)        
    41     2.05611230E+03   # MqL1(Q)         
    42     2.05611230E+03   # MqL2(Q)         
    43     1.97951489E+03   # MqL3(Q)         
    44     1.95809875E+03   # MuR(Q)          
    45     1.95809875E+03   # McR(Q)          
    46     1.79465955E+03   # MtR(Q)          
    47     1.94789075E+03   # MdR(Q)          
    48     1.94789075E+03   # MsR(Q)          
    49     1.94067822E+03   # MbR(Q)          
Block AU Q=  1.88482239E+03   #
  1  1    -4.23792633E+02   # A_u
  2  2    -4.23792633E+02   # A_c
  3  3    -4.23792633E+02   # A_t
Block AD Q=  1.88482239E+03   #
  1  1    -4.73206207E+02   # A_d
  2  2    -4.73206207E+02   # A_s
  3  3    -4.73206207E+02   # A_b
Block AE Q=  1.88482239E+03   #
  1  1    -5.27718201E+01   # A_e
  2  2    -5.27718201E+01   # A_mu
  3  3    -5.27718201E+01   # A_tau
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
DECAY   1000021  3.91600803E-02   # GLSS  decays
#          BR          NDA       ID1       ID2       ID3       ID4
      5.98118640E-02    3      1000024         1        -2             # GLSS   -->  W1SS+  DN     UB          
      5.98118640E-02    3     -1000024         2        -1             # GLSS   -->  W1SS-  UP     DB          
      5.98118231E-02    3      1000024         3        -4             # GLSS   -->  W1SS+  ST     CB          
      5.98118231E-02    3     -1000024         4        -3             # GLSS   -->  W1SS-  CH     SB          
      5.52729368E-02    3      1000024         5        -6             # GLSS   -->  W1SS+  BT     TB          
      5.52729368E-02    3     -1000024         6        -5             # GLSS   -->  W1SS-  TP     BB          
      2.17750575E-03    3      1000037         1        -2             # GLSS   -->  W2SS+  DN     UB          
      2.17750575E-03    3     -1000037         2        -1             # GLSS   -->  W2SS-  UP     DB          
      2.17750436E-03    3      1000037         3        -4             # GLSS   -->  W2SS+  ST     CB          
      2.17750436E-03    3     -1000037         4        -3             # GLSS   -->  W2SS-  CH     SB          
      8.60342830E-02    3      1000037         5        -6             # GLSS   -->  W2SS+  BT     TB          
      8.60342830E-02    3     -1000037         6        -5             # GLSS   -->  W2SS-  TP     BB          
      1.14810000E-07    2      1000022        21                       # GLSS   -->  Z1SS   GL                 
      3.61842811E-02    3      1000022         2        -2             # GLSS   -->  Z1SS   UP     UB          
      1.07676769E-02    3      1000022         1        -1             # GLSS   -->  Z1SS   DN     DB          
      1.07676769E-02    3      1000022         3        -3             # GLSS   -->  Z1SS   ST     SB          
      3.61842290E-02    3      1000022         4        -4             # GLSS   -->  Z1SS   CH     CB          
      1.16079841E-02    3      1000022         5        -5             # GLSS   -->  Z1SS   BT     BB          
      4.39323559E-02    3      1000022         6        -6             # GLSS   -->  Z1SS   TP     TB          
      3.57603683E-04    2      1000023        21                       # GLSS   -->  Z2SS   GL                 
      3.02434172E-02    3      1000023         2        -2             # GLSS   -->  Z2SS   UP     UB          
      2.94371918E-02    3      1000023         1        -1             # GLSS   -->  Z2SS   DN     DB          
      2.94371918E-02    3      1000023         3        -3             # GLSS   -->  Z2SS   ST     SB          
      3.02433725E-02    3      1000023         4        -4             # GLSS   -->  Z2SS   CH     CB          
      3.78532223E-02    3      1000023         5        -5             # GLSS   -->  Z2SS   BT     BB          
      1.92275140E-02    3      1000023         6        -6             # GLSS   -->  Z2SS   TP     TB          
      3.67192086E-03    2      1000025        21                       # GLSS   -->  Z3SS   GL                 
      1.14663890E-05    3      1000025         2        -2             # GLSS   -->  Z3SS   UP     UB          
      1.39081658E-05    3      1000025         1        -1             # GLSS   -->  Z3SS   DN     DB          
      1.39081658E-05    3      1000025         3        -3             # GLSS   -->  Z3SS   ST     SB          
      1.14663708E-05    3      1000025         4        -4             # GLSS   -->  Z3SS   CH     CB          
      4.22279863E-03    3      1000025         5        -5             # GLSS   -->  Z3SS   BT     BB          
      5.01096137E-02    3      1000025         6        -6             # GLSS   -->  Z3SS   TP     TB          
      3.35715711E-03    2      1000035        21                       # GLSS   -->  Z4SS   GL                 
      1.08746300E-03    3      1000035         2        -2             # GLSS   -->  Z4SS   UP     UB          
      1.24757097E-03    3      1000035         1        -1             # GLSS   -->  Z4SS   DN     DB          
      1.24757097E-03    3      1000035         3        -3             # GLSS   -->  Z4SS   ST     SB          
      1.08746148E-03    3      1000035         4        -4             # GLSS   -->  Z4SS   CH     CB          
      5.21703064E-03    3      1000035         5        -5             # GLSS   -->  Z4SS   BT     BB          
      7.18851313E-02    3      1000035         6        -6             # GLSS   -->  Z4SS   TP     TB          
      5.44437551E-11    2      1000039        21                       # GLSS   -->  GVSS   GL                 
#         PDG         Width
DECAY   1000002  4.89757767E+01   # UPL   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      5.24737174E-03    2      1000022         2                       # UPL    -->  Z1SS   UP                 
      1.49421811E-01    2      1000023         2                       # UPL    -->  Z2SS   UP                 
      1.50129170E-04    2      1000025         2                       # UPL    -->  Z3SS   UP                 
      9.11439583E-03    2      1000035         2                       # UPL    -->  Z4SS   UP                 
      5.19313037E-01    2      1000021         2                       # UPL    -->  GLSS   UP                 
      3.02624702E-01    2      1000024         1                       # UPL    -->  W1SS+  DN                 
      1.41285537E-02    2      1000037         1                       # UPL    -->  W2SS+  DN                 
#         PDG         Width
DECAY   1000001  4.90139809E+01   # DNL   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      6.54704776E-03    2      1000022         1                       # DNL    -->  Z1SS   DN                 
      1.46120608E-01    2      1000023         1                       # DNL    -->  Z2SS   DN                 
      2.56652216E-04    2      1000025         1                       # DNL    -->  Z3SS   DN                 
      1.08722541E-02    2      1000035         1                       # DNL    -->  Z4SS   DN                 
      5.20998478E-01    2      1000021         1                       # DNL    -->  GLSS   DN                 
      2.90645182E-01    2     -1000024         2                       # DNL    -->  W1SS-  UP                 
      2.45597642E-02    2     -1000037         2                       # DNL    -->  W2SS-  UP                 
#         PDG         Width
DECAY   1000003  4.90139618E+01   # STL   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      6.54705008E-03    2      1000022         3                       # STL    -->  Z1SS   ST                 
      1.46120667E-01    2      1000023         3                       # STL    -->  Z2SS   ST                 
      2.56652333E-04    2      1000025         3                       # STL    -->  Z3SS   ST                 
      1.08722588E-02    2      1000035         3                       # STL    -->  Z4SS   ST                 
      5.20998657E-01    2      1000021         3                       # STL    -->  GLSS   ST                 
      2.90645003E-01    2     -1000024         4                       # STL    -->  W1SS-  CH                 
      2.45597493E-02    2     -1000037         4                       # STL    -->  W2SS-  CH                 
#         PDG         Width
DECAY   1000004  4.89757080E+01   # CHL   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      5.24737593E-03    2      1000022         4                       # CHL    -->  Z1SS   CH                 
      1.49421915E-01    2      1000023         4                       # CHL    -->  Z2SS   CH                 
      1.50129286E-04    2      1000025         4                       # CHL    -->  Z3SS   CH                 
      9.11440235E-03    2      1000035         4                       # CHL    -->  Z4SS   CH                 
      5.19312382E-01    2      1000021         4                       # CHL    -->  GLSS   CH                 
      3.02625239E-01    2      1000024         3                       # CHL    -->  W1SS+  ST                 
      1.41285807E-02    2      1000037         3                       # CHL    -->  W2SS+  ST                 
#         PDG         Width
DECAY   1000005  2.29433556E+01   # BT1   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      4.83673289E-02    2      1000022         5                       # BT1    -->  Z1SS   BT                 
      2.71058753E-02    2      1000023         5                       # BT1    -->  Z2SS   BT                 
      2.23622285E-02    2      1000025         5                       # BT1    -->  Z3SS   BT                 
      1.30565921E-02    2      1000035         5                       # BT1    -->  Z4SS   BT                 
      7.78688967E-01    2      1000021         5                       # BT1    -->  GLSS   BT                 
      5.36670722E-02    2     -1000024         6                       # BT1    -->  W1SS-  TP                 
      5.66698126E-02    2     -1000037         6                       # BT1    -->  W2SS-  TP                 
      8.21639696E-05    2          -24   1000006                       # BT1    -->  W-     TP1                
#         PDG         Width
DECAY   1000006  4.52941818E+01   # TP1   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      1.47185177E-01    2      1000021         6                       # TP1    -->  GLSS   TP                 
      8.59281197E-02    2      1000022         6                       # TP1    -->  Z1SS   TP                 
      1.68600678E-02    2      1000023         6                       # TP1    -->  Z2SS   TP                 
      1.92801625E-01    2      1000025         6                       # TP1    -->  Z3SS   TP                 
      1.70289561E-01    2      1000035         6                       # TP1    -->  Z4SS   TP                 
      3.31429616E-02    2      1000024         5                       # TP1    -->  W1SS+  BT                 
      3.53792548E-01    2      1000037         5                       # TP1    -->  W2SS+  BT                 
#         PDG         Width
DECAY   2000002  2.35085621E+01   # UPR   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      1.86430216E-01    2      1000022         2                       # UPR    -->  Z1SS   UP                 
      1.46435632E-04    2      1000023         2                       # UPR    -->  Z2SS   UP                 
      1.09667046E-04    2      1000025         2                       # UPR    -->  Z3SS   UP                 
      5.95877820E-04    2      1000035         2                       # UPR    -->  Z4SS   UP                 
      8.12717795E-01    2      1000021         2                       # UPR    -->  GLSS   UP                 
#         PDG         Width
DECAY   2000001  1.95927353E+01   # DNR   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      5.56373559E-02    2      1000022         1                       # DNR    -->  Z1SS   DN                 
      4.36538248E-05    2      1000023         1                       # DNR    -->  Z2SS   DN                 
      3.26424233E-05    2      1000025         1                       # DNR    -->  Z3SS   DN                 
      1.77336915E-04    2      1000035         1                       # DNR    -->  Z4SS   DN                 
      9.44109082E-01    2      1000021         1                       # DNR    -->  GLSS   DN                 
#         PDG         Width
DECAY   2000003  1.95927353E+01   # STR   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      5.56373559E-02    2      1000022         3                       # STR    -->  Z1SS   ST                 
      4.36538248E-05    2      1000023         3                       # STR    -->  Z2SS   ST                 
      3.26424233E-05    2      1000025         3                       # STR    -->  Z3SS   ST                 
      1.77336915E-04    2      1000035         3                       # STR    -->  Z4SS   ST                 
      9.44109082E-01    2      1000021         3                       # STR    -->  GLSS   ST                 
#         PDG         Width
DECAY   2000004  2.35084877E+01   # CHR   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      1.86430693E-01    2      1000022         4                       # CHR    -->  Z1SS   CH                 
      1.46435996E-04    2      1000023         4                       # CHR    -->  Z2SS   CH                 
      1.09667330E-04    2      1000025         4                       # CHR    -->  Z3SS   CH                 
      5.95879334E-04    2      1000035         4                       # CHR    -->  Z4SS   CH                 
      8.12717319E-01    2      1000021         4                       # CHR    -->  GLSS   CH                 
#         PDG         Width
DECAY   2000005  6.23189583E+01   # BT2   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      4.73209983E-03    2      1000022         5                       # BT2    -->  Z1SS   BT                 
      1.00973569E-01    2      1000023         5                       # BT2    -->  Z2SS   BT                 
      9.98083130E-03    2      1000025         5                       # BT2    -->  Z3SS   BT                 
      1.95801649E-02    2      1000035         5                       # BT2    -->  Z4SS   BT                 
      3.22045535E-01    2      1000021         5                       # BT2    -->  GLSS   BT                 
      2.08503902E-01    2     -1000024         6                       # BT2    -->  W1SS-  TP                 
      3.32878113E-01    2     -1000037         6                       # BT2    -->  W2SS-  TP                 
      1.30575988E-03    2          -24   1000006                       # BT2    -->  W-     TP1                
#         PDG         Width
DECAY   2000006  6.34859772E+01   # TP2   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      2.95760036E-01    2      1000021         6                       # TP2    -->  GLSS   TP                 
      2.18236908E-01    2      1000024         5                       # TP2    -->  W1SS+  BT                 
      3.38786766E-02    2      1000037         5                       # TP2    -->  W2SS+  BT                 
      8.01747723E-04    2           23   1000006                       # TP2    -->  Z0     TP1                
      2.80922186E-03    2           25   1000006                       # TP2    -->  HL0    TP1                
      4.01692092E-03    2      1000022         6                       # TP2    -->  Z1SS   TP                 
      1.12054899E-01    2      1000023         6                       # TP2    -->  Z2SS   TP                 
      1.63219988E-01    2      1000025         6                       # TP2    -->  Z3SS   TP                 
      1.69221684E-01    2      1000035         6                       # TP2    -->  Z4SS   TP                 
#         PDG         Width
DECAY   1000011  1.98367858E+00   # EL-   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      3.05330515E-01    2      1000022        11                       # EL-    -->  Z1SS   E-                 
      2.40204901E-01    2      1000023        11                       # EL-    -->  Z2SS   E-                 
      4.54464257E-01    2     -1000024        12                       # EL-    -->  W1SS-  NUE                
      1.85223257E-07    3      1000015        11       -15             # EL-    -->  TAU1-  E-     TAU+        
      2.04605783E-07    3     -1000015        11        15             # EL-    -->  TAU1+  E-     TAU-        
      2.03775985E-14    2           11   1000039                       # EL-    -->  E-     GVSS               
#         PDG         Width
DECAY   1000013  1.98367834E+00   # MUL-  decays
#          BR          NDA       ID1       ID2       ID3       ID4
      3.05330545E-01    2      1000022        13                       # MUL-   -->  Z1SS   MU-                
      2.40204841E-01    2      1000023        13                       # MUL-   -->  Z2SS   MU-                
      4.54464316E-01    2     -1000024        14                       # MUL-   -->  W1SS-  NUM                
      1.85223286E-07    3      1000015        13       -15             # MUL-   -->  TAU1-  MU-    TAU+        
      2.04605797E-07    3     -1000015        13        15             # MUL-   -->  TAU1+  MU-    TAU-        
      2.03776002E-14    2           13   1000039                       # MUL-   -->  MU-    GVSS               
#         PDG         Width
DECAY   1000015  1.89405933E-01   # TAU1- decays
#          BR          NDA       ID1       ID2       ID3       ID4
      1.00000000E+00    2      1000022        15                       # TAU1-  -->  Z1SS   TAU-               
      6.08585549E-15    2           15   1000039                       # TAU1-  -->  TAU-   GVSS               
#         PDG         Width
DECAY   1000012  1.90628219E+00   # NUEL  decays
#          BR          NDA       ID1       ID2       ID3       ID4
      3.34501565E-01    2      1000022        12                       # NUEL   -->  Z1SS   NUE                
      2.14198276E-01    2      1000023        12                       # NUEL   -->  Z2SS   NUE                
      4.51298177E-01    2      1000024        11                       # NUEL   -->  W1SS+  E-                 
      3.17879397E-07    3      1000015        12       -15             # NUEL   -->  TAU1-  NUE    TAU+        
      2.55577930E-07    3     -1000015        12        15             # NUEL   -->  TAU1+  NUE    TAU-        
      1.43115835E-06    3     -1000015        11        16             # NUEL   -->  TAU1+  E-     NUT         
      1.99230281E-14    2           12   1000039                       # NUEL   -->  NUE    GVSS               
#         PDG         Width
DECAY   1000014  1.90628195E+00   # NUML  decays
#          BR          NDA       ID1       ID2       ID3       ID4
      3.34501594E-01    2      1000022        14                       # NUML   -->  Z1SS   NUM                
      2.14198306E-01    2      1000023        14                       # NUML   -->  Z2SS   NUM                
      4.51298118E-01    2      1000024        13                       # NUML   -->  W1SS+  MU-                
      3.17879454E-07    3      1000015        14       -15             # NUML   -->  TAU1-  NUM    TAU+        
      2.55577959E-07    3     -1000015        14        15             # NUML   -->  TAU1+  NUM    TAU-        
      1.43258205E-06    3     -1000015        13        16             # NUML   -->  TAU1+  MU-    NUT         
      1.99230315E-14    2           14   1000039                       # NUML   -->  NUM    GVSS               
#         PDG         Width
DECAY   1000016  2.05353856E+00   # NUTL  decays
#          BR          NDA       ID1       ID2       ID3       ID4
      3.07356805E-01    2      1000022        16                       # NUTL   -->  Z1SS   NUT                
      1.90822199E-01    2      1000023        16                       # NUTL   -->  Z2SS   NUT                
      4.03485298E-01    2      1000024        15                       # NUTL   -->  W1SS+  TAU-               
      9.83347967E-02    2           24   1000015                       # NUTL   -->  W+     TAU1-              
      6.53100642E-07    3     -1000015        16        15             # NUTL   -->  TAU1+  NUT    TAU-        
      2.22272746E-07    3      1000015        16       -15             # NUTL   -->  TAU1-  NUT    TAU+        
      1.79769750E-14    2           16   1000039                       # NUTL   -->  NUT    GVSS               
#         PDG         Width
DECAY   2000011  1.86409205E-01   # ER-   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      1.00000000E+00    2      1000022        11                       # ER-    -->  Z1SS   E-                 
      6.12896608E-15    2           11   1000039                       # ER-    -->  E-     GVSS               
#         PDG         Width
DECAY   2000013  1.86408877E-01   # MUR-  decays
#          BR          NDA       ID1       ID2       ID3       ID4
      1.00000000E+00    2      1000022        13                       # MUR-   -->  Z1SS   MU-                
      6.12897582E-15    2           13   1000039                       # MUR-   -->  MU-    GVSS               
#         PDG         Width
DECAY   2000015  2.16165304E+00   # TAU2- decays
#          BR          NDA       ID1       ID2       ID3       ID4
      2.79207855E-01    2      1000022        15                       # TAU2-  -->  Z1SS   TAU-               
      2.12360233E-01    2      1000023        15                       # TAU2-  -->  Z2SS   TAU-               
      4.00010824E-01    2     -1000024        16                       # TAU2-  -->  W1SS-  NUT                
      4.94371615E-02    2           23   1000015                       # TAU2-  -->  Z0     TAU1-              
      5.89839406E-02    2           25   1000015                       # TAU2-  -->  HL0    TAU1-              
#         PDG         Width
DECAY   1000022  3.95189177E-16   # Z1SS  decays
#          BR          NDA       ID1       ID2       ID3       ID4
      8.14181864E-01    2      1000039        22                       # Z1SS   -->  GVSS   GM                 
      1.78591069E-02    3      1000039        11       -11             # Z1SS   -->  GVSS   E-     E+          
      1.67798549E-01    2      1000039        23                       # Z1SS   -->  GVSS   Z0                 
      1.60490788E-04    2      1000039        25                       # Z1SS   -->  GVSS   HL0                
#         PDG         Width
DECAY   1000023  4.54013087E-02   # Z2SS  decays
#          BR          NDA       ID1       ID2       ID3       ID4
      2.27506553E-06    2      1000022        22                       # Z2SS   -->  Z1SS   GM                 
      5.14371544E-02    2      1000022        23                       # Z2SS   -->  Z1SS   Z0                 
      4.32506482E-07    3      1000022         2        -2             # Z2SS   -->  Z1SS   UP     UB          
      5.20717947E-07    3      1000022         1        -1             # Z2SS   -->  Z1SS   DN     DB          
      5.20717947E-07    3      1000022         3        -3             # Z2SS   -->  Z1SS   ST     SB          
      4.32506056E-07    3      1000022         4        -4             # Z2SS   -->  Z1SS   CH     CB          
      2.12307373E-06    3      1000022         5        -5             # Z2SS   -->  Z1SS   BT     BB          
      2.64065806E-04    3      1000022        11       -11             # Z2SS   -->  Z1SS   E-     E+          
      2.64065806E-04    3      1000022        13       -13             # Z2SS   -->  Z1SS   MU-    MU+         
      2.70124525E-04    3      1000022        15       -15             # Z2SS   -->  Z1SS   TAU-   TAU+        
      2.88196199E-04    3      1000022        12       -12             # Z2SS   -->  Z1SS   NUE    ANUE        
      2.88196199E-04    3      1000022        14       -14             # Z2SS   -->  Z1SS   NUM    ANUM        
      2.99187202E-04    3      1000022        16       -16             # Z2SS   -->  Z1SS   NUT    ANUT        
      6.38653159E-01    2      1000022        25                       # Z2SS   -->  Z1SS   HL0                
      9.37242899E-03    2      2000011       -11                       # Z2SS   -->  ER-    E+                 
      9.37242899E-03    2     -2000011        11                       # Z2SS   -->  ER+    E-                 
      9.37242806E-03    2      2000013       -13                       # Z2SS   -->  MUR-   MU+                
      9.37242806E-03    2     -2000013        13                       # Z2SS   -->  MUR+   MU-                
      1.35369882E-01    2      1000015       -15                       # Z2SS   -->  TAU1-  TAU+               
      1.35369882E-01    2     -1000015        15                       # Z2SS   -->  TAU1+  TAU-               
      5.73713033E-14    2      1000039        22                       # Z2SS   -->  GVSS   GM                 
      1.32006157E-15    3      1000039        11       -11             # Z2SS   -->  GVSS   E-     E+          
      1.50326434E-13    2      1000039        23                       # Z2SS   -->  GVSS   Z0                 
      2.18650142E-15    2      1000039        25                       # Z2SS   -->  GVSS   HL0                
#         PDG         Width
DECAY   1000025  2.99020267E+00   # Z3SS  decays
#          BR          NDA       ID1       ID2       ID3       ID4
      7.58441630E-08    2      1000022        22                       # Z3SS   -->  Z1SS   GM                 
      3.51678636E-07    2      1000023        22                       # Z3SS   -->  Z2SS   GM                 
      2.70944983E-01    2      1000024       -24                       # Z3SS   -->  W1SS+  W-                 
      2.70944983E-01    2     -1000024        24                       # Z3SS   -->  W1SS-  W+                 
      1.36083215E-01    2      1000022        23                       # Z3SS   -->  Z1SS   Z0                 
      2.57172257E-01    2      1000023        23                       # Z3SS   -->  Z2SS   Z0                 
      3.94170030E-10    3      1000022         2        -2             # Z3SS   -->  Z1SS   UP     UB          
      1.12787710E-10    3      1000022         1        -1             # Z3SS   -->  Z1SS   DN     DB          
      1.12787710E-10    3      1000022         3        -3             # Z3SS   -->  Z1SS   ST     SB          
      3.94169614E-10    3      1000022         4        -4             # Z3SS   -->  Z1SS   CH     CB          
      1.52702194E-06    3      1000022         5        -5             # Z3SS   -->  Z1SS   BT     BB          
      2.59261867E-07    3      1000022        15       -15             # Z3SS   -->  Z1SS   TAU-   TAU+        
      4.39280938E-11    3      1000023         2        -2             # Z3SS   -->  Z2SS   UP     UB          
      7.31549127E-11    3      1000023         1        -1             # Z3SS   -->  Z2SS   DN     DB          
      7.31549127E-11    3      1000023         3        -3             # Z3SS   -->  Z2SS   ST     SB          
      4.39280452E-11    3      1000023         4        -4             # Z3SS   -->  Z2SS   CH     CB          
      1.32311584E-07    3      1000023         5        -5             # Z3SS   -->  Z2SS   BT     BB          
      2.09020463E-08    3      1000023        15       -15             # Z3SS   -->  Z2SS   TAU-   TAU+        
      2.13980302E-02    2      1000022        25                       # Z3SS   -->  Z1SS   HL0                
      4.20774845E-03    2      1000023        25                       # Z3SS   -->  Z2SS   HL0                
      3.32342984E-06    2      1000011       -11                       # Z3SS   -->  EL-    E+                 
      3.32342984E-06    2     -1000011        11                       # Z3SS   -->  EL+    E-                 
      3.32342029E-06    2      1000013       -13                       # Z3SS   -->  MUL-   MU+                
      3.32342029E-06    2     -1000013        13                       # Z3SS   -->  MUL+   MU-                
      2.97775405E-04    2      2000011       -11                       # Z3SS   -->  ER-    E+                 
      2.97775405E-04    2     -2000011        11                       # Z3SS   -->  ER+    E-                 
      2.97775405E-04    2      2000013       -13                       # Z3SS   -->  MUR-   MU+                
      2.97775405E-04    2     -2000013        13                       # Z3SS   -->  MUR+   MU-                
      1.84967816E-02    2      1000015       -15                       # Z3SS   -->  TAU1-  TAU+               
      1.84967816E-02    2     -1000015        15                       # Z3SS   -->  TAU1+  TAU-               
      4.42433491E-04    2      2000015       -15                       # Z3SS   -->  TAU2-  TAU+               
      4.42433491E-04    2     -2000015        15                       # Z3SS   -->  TAU2+  TAU-               
      2.59188855E-05    2      1000012       -12                       # Z3SS   -->  NUEL   ANUE               
      2.59188855E-05    2     -1000012        12                       # Z3SS   -->  ANUEL  NUE                
      2.59188855E-05    2      1000014       -14                       # Z3SS   -->  NUML   ANUM               
      2.59188855E-05    2     -1000014        14                       # Z3SS   -->  ANUML  NUM                
      2.98919713E-05    2      1000016       -16                       # Z3SS   -->  NUTL   ANUT               
      2.98919713E-05    2     -1000016        16                       # Z3SS   -->  ANUTL  NUT                
      6.24374952E-19    2      1000039        22                       # Z3SS   -->  GVSS   GM                 
      1.46987911E-20    3      1000039        11       -11             # Z3SS   -->  GVSS   E-     E+          
      3.74781967E-15    2      1000039        23                       # Z3SS   -->  GVSS   Z0                 
      4.66921110E-15    2      1000039        25                       # Z3SS   -->  GVSS   HL0                
#         PDG         Width
DECAY   1000035  3.54212141E+00   # Z4SS  decays
#          BR          NDA       ID1       ID2       ID3       ID4
      2.50445886E-08    2      1000022        22                       # Z4SS   -->  Z1SS   GM                 
      9.50156789E-08    2      1000023        22                       # Z4SS   -->  Z2SS   GM                 
      9.44028744E-10    2      1000025        22                       # Z4SS   -->  Z3SS   GM                 
      2.96072900E-01    2      1000024       -24                       # Z4SS   -->  W1SS+  W-                 
      2.96072900E-01    2     -1000024        24                       # Z4SS   -->  W1SS-  W+                 
      1.90828275E-02    2      1000022        23                       # Z4SS   -->  Z1SS   Z0                 
      6.33901684E-03    2      1000023        23                       # Z4SS   -->  Z2SS   Z0                 
      1.04468612E-08    3      1000022         2        -2             # Z4SS   -->  Z1SS   UP     UB          
      9.18590537E-09    3      1000022         1        -1             # Z4SS   -->  Z1SS   DN     DB          
      9.18590537E-09    3      1000022         3        -3             # Z4SS   -->  Z1SS   ST     SB          
      1.04468505E-08    3      1000022         4        -4             # Z4SS   -->  Z1SS   CH     CB          
      1.22467100E-06    3      1000022         5        -5             # Z4SS   -->  Z1SS   BT     BB          
      2.06618338E-07    3      1000022        15       -15             # Z4SS   -->  Z1SS   TAU-   TAU+        
      9.30616295E-09    3      1000023         2        -2             # Z4SS   -->  Z2SS   UP     UB          
      1.08159064E-08    3      1000023         1        -1             # Z4SS   -->  Z2SS   DN     DB          
      1.08159064E-08    3      1000023         3        -3             # Z4SS   -->  Z2SS   ST     SB          
      9.30615407E-09    3      1000023         4        -4             # Z4SS   -->  Z2SS   CH     CB          
      1.29187910E-07    3      1000023         5        -5             # Z4SS   -->  Z2SS   BT     BB          
      1.83275510E-08    3      1000023        15       -15             # Z4SS   -->  Z2SS   TAU-   TAU+        
      3.01180911E-08    3      1000025         2        -2             # Z4SS   -->  Z3SS   UP     UB          
      3.88406534E-08    3      1000025         1        -1             # Z4SS   -->  Z3SS   DN     DB          
      3.88406534E-08    3      1000025         3        -3             # Z4SS   -->  Z3SS   ST     SB          
      3.01180911E-08    3      1000025         4        -4             # Z4SS   -->  Z3SS   CH     CB          
      1.13218155E-08    3      1000025         5        -5             # Z4SS   -->  Z3SS   BT     BB          
      8.81057716E-09    3      1000025        11       -11             # Z4SS   -->  Z3SS   E-     E+          
      8.81057716E-09    3      1000025        13       -13             # Z4SS   -->  Z3SS   MU-    MU+         
      7.44355377E-09    3      1000025        15       -15             # Z4SS   -->  Z3SS   TAU-   TAU+        
      1.75302777E-08    3      1000025        12       -12             # Z4SS   -->  Z3SS   NUE    ANUE        
      1.75302777E-08    3      1000025        14       -14             # Z4SS   -->  Z3SS   NUM    ANUM        
      1.75302777E-08    3      1000025        16       -16             # Z4SS   -->  Z3SS   NUT    ANUT        
      1.10988349E-01    2      1000022        25                       # Z4SS   -->  Z1SS   HL0                
      2.25041777E-01    2      1000023        25                       # Z4SS   -->  Z2SS   HL0                
      5.30341640E-04    2      1000011       -11                       # Z4SS   -->  EL-    E+                 
      5.30341640E-04    2     -1000011        11                       # Z4SS   -->  EL+    E-                 
      5.30340825E-04    2      1000013       -13                       # Z4SS   -->  MUL-   MU+                
      5.30340825E-04    2     -1000013        13                       # Z4SS   -->  MUL+   MU-                
      1.44414615E-03    2      2000011       -11                       # Z4SS   -->  ER-    E+                 
      1.44414615E-03    2     -2000011        11                       # Z4SS   -->  ER+    E-                 
      1.44414615E-03    2      2000013       -13                       # Z4SS   -->  MUR-   MU+                
      1.44414615E-03    2     -2000013        13                       # Z4SS   -->  MUR+   MU-                
      1.43908560E-02    2      1000015       -15                       # Z4SS   -->  TAU1-  TAU+               
      1.43908560E-02    2     -1000015        15                       # Z4SS   -->  TAU1+  TAU-               
      1.18654128E-03    2      2000015       -15                       # Z4SS   -->  TAU2-  TAU+               
      1.18654128E-03    2     -2000015        15                       # Z4SS   -->  TAU2+  TAU-               
      1.17927114E-03    2      1000012       -12                       # Z4SS   -->  NUEL   ANUE               
      1.17927114E-03    2     -1000012        12                       # Z4SS   -->  ANUEL  NUE                
      1.17927114E-03    2      1000014       -14                       # Z4SS   -->  NUML   ANUM               
      1.17927114E-03    2     -1000014        14                       # Z4SS   -->  ANUML  NUM                
      1.31532981E-03    2      1000016       -16                       # Z4SS   -->  NUTL   ANUT               
      1.31532981E-03    2     -1000016        16                       # Z4SS   -->  ANUTL  NUT                
      8.99991962E-17    2      1000039        22                       # Z4SS   -->  GVSS   GM                 
      2.12179623E-18    3      1000039        11       -11             # Z4SS   -->  GVSS   E-     E+          
      5.40061346E-15    2      1000039        23                       # Z4SS   -->  GVSS   Z0                 
      3.15325908E-15    2      1000039        25                       # Z4SS   -->  GVSS   HL0                
#         PDG         Width
DECAY   1000024  3.99731398E-02   # W1SS+ decays
#          BR          NDA       ID1       ID2       ID3       ID4
      1.08873951E-06    3      1000022         2        -1             # W1SS+  -->  Z1SS   UP     DB          
      1.08873905E-06    3      1000022         4        -3             # W1SS+  -->  Z1SS   CH     SB          
      6.38210797E-04    3      1000022       -11        12             # W1SS+  -->  Z1SS   E+     NUE         
      6.38210797E-04    3      1000022       -13        14             # W1SS+  -->  Z1SS   MU+    NUM         
      6.58287550E-04    3      1000022       -15        16             # W1SS+  -->  Z1SS   TAU+   NUT         
      7.16747820E-01    2      1000022        24                       # W1SS+  -->  Z1SS   W+                 
      5.37192415E-13    3      1000023       -11        12             # W1SS+  -->  Z2SS   E+     NUE         
      5.37192415E-13    3      1000023       -13        14             # W1SS+  -->  Z2SS   MU+    NUM         
      2.81315386E-01    2     -1000015        16                       # W1SS+  -->  TAU1+  NUT                
#         PDG         Width
DECAY   1000037  3.19218683E+00   # W2SS+ decays
#          BR          NDA       ID1       ID2       ID3       ID4
      1.60169780E-08    3      1000022         2        -1             # W2SS+  -->  Z1SS   UP     DB          
      1.60169709E-08    3      1000022         4        -3             # W2SS+  -->  Z1SS   CH     SB          
      2.11274482E-07    3      1000022       -15        16             # W2SS+  -->  Z1SS   TAU+   NUT         
      1.28629684E-01    2      1000022        24                       # W2SS+  -->  Z1SS   W+                 
      7.72390685E-09    3      1000023         2        -1             # W2SS+  -->  Z2SS   UP     DB          
      7.72390507E-09    3      1000023         4        -3             # W2SS+  -->  Z2SS   CH     SB          
      3.28424115E-08    3      1000023       -15        16             # W2SS+  -->  Z2SS   TAU+   NUT         
      2.85681278E-01    2      1000023        24                       # W2SS+  -->  Z2SS   W+                 
      1.21280181E-07    3      1000025         2        -1             # W2SS+  -->  Z3SS   UP     DB          
      1.21280181E-07    3      1000025         4        -3             # W2SS+  -->  Z3SS   CH     SB          
      4.04263325E-08    3      1000025       -11        12             # W2SS+  -->  Z3SS   E+     NUE         
      4.04263325E-08    3      1000025       -13        14             # W2SS+  -->  Z3SS   MU+    NUM         
      4.04263361E-08    3      1000025       -15        16             # W2SS+  -->  Z3SS   TAU+   NUT         
      1.45298115E-03    2      1000012       -11                       # W2SS+  -->  NUEL   E+                 
      1.45297952E-03    2      1000014       -13                       # W2SS+  -->  NUML   MU+                
      3.19128926E-03    2      1000016       -15                       # W2SS+  -->  NUTL   TAU+               
      1.93177443E-03    2     -1000011        12                       # W2SS+  -->  EL+    NUE                
      1.93177443E-03    2     -1000013        14                       # W2SS+  -->  MUL+   NUM                
      2.91353315E-02    2     -1000015        16                       # W2SS+  -->  TAU1+  NUT                
      2.30389484E-03    2     -2000015        16                       # W2SS+  -->  TAU2+  NUT                
      2.79571980E-01    2      1000024        23                       # W2SS+  -->  W1SS+  Z0                 
      1.08339462E-08    3      1000024         1        -1             # W2SS+  -->  W1SS+  DN     DB          
      1.08339346E-08    3      1000024         3        -3             # W2SS+  -->  W1SS+  ST     SB          
      1.80211313E-08    3      1000024         2        -2             # W2SS+  -->  W1SS+  UP     UB          
      1.80211313E-08    3      1000024         4        -4             # W2SS+  -->  W1SS+  CH     CB          
      2.64716387E-01    2      1000024        25                       # W2SS+  -->  W1SS+  HL0                
#         PDG         Width
DECAY        25  3.14017083E-03   # HL0   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      6.59355415E-09    2           11       -11                       # HL0    -->  E-     E+                 
      2.78390391E-04    2           13       -13                       # HL0    -->  MU-    MU+                
      7.96250999E-02    2           15       -15                       # HL0    -->  TAU-   TAU+               
      8.27358690E-06    2            1        -1                       # HL0    -->  DN     DB                 
      3.34294373E-03    2            3        -3                       # HL0    -->  ST     SB                 
      7.15708613E-01    2            5        -5                       # HL0    -->  BT     BB                 
      2.53908001E-06    2            2        -2                       # HL0    -->  UP     UB                 
      4.76793051E-02    2            4        -4                       # HL0    -->  CH     CB                 
      2.52863858E-03    2           22        22                       # HL0    -->  GM     GM                 
      5.34284301E-02    2           21        21                       # HL0    -->  GL     GL                 
      5.00069093E-03    3           24        11       -12             # HL0    -->  W+     E-     ANUE        
      5.00069093E-03    3           24        13       -14             # HL0    -->  W+     MU-    ANUM        
      5.00069093E-03    3           24        15       -16             # HL0    -->  W+     TAU-   ANUT        
      1.50020737E-02    3           24        -2         1             # HL0    -->  W+     UB     DN          
      1.50020737E-02    3           24        -4         3             # HL0    -->  W+     CB     ST          
      5.00069093E-03    3          -24       -11        12             # HL0    -->  W-     E+     NUE         
      5.00069093E-03    3          -24       -13        14             # HL0    -->  W-     MU+    NUM         
      5.00069093E-03    3          -24       -15        16             # HL0    -->  W-     TAU+   NUT         
      1.50020737E-02    3          -24         2        -1             # HL0    -->  W-     UP     DB          
      1.50020737E-02    3          -24         4        -3             # HL0    -->  W-     CH     SB          
      5.05073462E-04    3           23        12       -12             # HL0    -->  Z0     NUE    ANUE        
      5.05073462E-04    3           23        14       -14             # HL0    -->  Z0     NUM    ANUM        
      5.05073462E-04    3           23        16       -16             # HL0    -->  Z0     NUT    ANUT        
      2.54198414E-04    3           23        11       -11             # HL0    -->  Z0     E-     E+          
      2.54198414E-04    3           23        13       -13             # HL0    -->  Z0     MU-    MU+         
      2.54198414E-04    3           23        15       -15             # HL0    -->  Z0     TAU-   TAU+        
      8.70862161E-04    3           23         2        -2             # HL0    -->  Z0     UP     UB          
      8.70862161E-04    3           23         4        -4             # HL0    -->  Z0     CH     CB          
      1.12188503E-03    3           23         1        -1             # HL0    -->  Z0     DN     DB          
      1.12188503E-03    3           23         3        -3             # HL0    -->  Z0     ST     SB          
      1.12188503E-03    3           23         5        -5             # HL0    -->  Z0     BT     BB          
#         PDG         Width
DECAY        35  3.58167863E+00   # HH0   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      1.07872076E-08    2           11       -11                       # HH0    -->  E-     E+                 
      4.55455360E-04    2           13       -13                       # HH0    -->  MU-    MU+                
      1.30446985E-01    2           15       -15                       # HH0    -->  TAU-   TAU+               
      1.32139103E-05    2            1        -1                       # HH0    -->  DN     DB                 
      5.33908838E-03    2            3        -3                       # HH0    -->  ST     SB                 
      7.97870457E-01    2            5        -5                       # HH0    -->  BT     BB                 
      8.70755620E-11    2            2        -2                       # HH0    -->  UP     UB                 
      1.22571839E-06    2            4        -4                       # HH0    -->  CH     CB                 
      5.63391373E-02    2            6        -6                       # HH0    -->  TP     TB                 
      2.16847027E-07    2           22        22                       # HH0    -->  GM     GM                 
      2.91987399E-05    2           21        21                       # HH0    -->  GL     GL                 
      1.76783476E-04    2           24       -24                       # HH0    -->  W+     W-                 
      8.97635182E-05    2           23        23                       # HH0    -->  Z0     Z0                 
      1.87586993E-03    2      1000022   1000022                       # HH0    -->  Z1SS   Z1SS               
      6.70346431E-03    2      1000022   1000023                       # HH0    -->  Z1SS   Z2SS               
      6.10868097E-04    2           25        25                       # HH0    -->  HL0    HL0                
      1.79576709E-05    2      2000011  -2000011                       # HH0    -->  ER-    ER+                
      1.79350172E-05    2      2000013  -2000013                       # HH0    -->  MUR-   MUR+               
      1.23920390E-05    2      1000015  -1000015                       # HH0    -->  TAU1-  TAU1+              
#         PDG         Width
DECAY        36  3.62998915E+00   # HA0   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      1.05751958E-08    2           11       -11                       # HA0    -->  E-     E+                 
      4.46503807E-04    2           13       -13                       # HA0    -->  MU-    MU+                
      1.27884746E-01    2           15       -15                       # HA0    -->  TAU-   TAU+               
      1.29549153E-05    2            1        -1                       # HA0    -->  DN     DB                 
      5.23444172E-03    2            3        -3                       # HA0    -->  ST     SB                 
      7.82266855E-01    2            5        -5                       # HA0    -->  BT     BB                 
      8.18797460E-11    2            2        -2                       # HA0    -->  UP     UB                 
      1.15348530E-06    2            4        -4                       # HA0    -->  CH     CB                 
      5.88839948E-02    2            6        -6                       # HA0    -->  TP     TB                 
      6.40334463E-07    2           22        22                       # HA0    -->  GM     GM                 
      8.55969120E-05    2           21        21                       # HA0    -->  GL     GL                 
      3.01625533E-03    2      1000022   1000022                       # HA0    -->  Z1SS   Z1SS               
      2.19985284E-02    2      1000022   1000023                       # HA0    -->  Z1SS   Z2SS               
      1.68323386E-04    2           25        23                       # HA0    -->  HL0    Z0                 
#         PDG         Width
DECAY        37  3.26158667E+00   # H+    decays
#          BR          NDA       ID1       ID2       ID3       ID4
      1.18849890E-08    2           12       -11                       # H+     -->  NUE    E+                 
      5.01805684E-04    2           14       -13                       # H+     -->  NUM    MU+                
      1.43723965E-01    2           16       -15                       # H+     -->  NUT    TAU+               
      1.33829353E-05    2            2        -1                       # H+     -->  UP     DB                 
      5.40851988E-03    2            4        -3                       # H+     -->  CH     SB                 
      8.19453299E-01    2            6        -5                       # H+     -->  TP     BB                 
      3.07072420E-02    2      1000024   1000022                       # H+     -->  W1SS+  Z1SS               
      1.91713276E-04    2           25        24                       # H+     -->  HL0    W+                 
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

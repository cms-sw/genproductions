
SLHA_TABLE = '''
#  ISAJET SUSY parameters in SUSY Les Houches Accord 2 format
#  Created by ISALHA 2.0 Last revision: H Baer 27 May 2014
Block SPINFO   # Program information
     1   ISASUGRA/ISASUSY from ISAJET   # Spectrum Calculator
     2   7.87   18-JUL-2017 13:55:55    # Version number
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
     1     4.00000000E+05   # Lambda scale of soft SSB
     2     8.00000000E+05   # M_mess overall messenger scale
     3     1.50000000E+01   # tan(beta)
     4     1.00000000E+00   # sign(mu)
     5     1.00000000E+00   # N_5 messenger index
     6     3.52756989E+02   # c_grav gravitino mass factor
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
        25     1.20076019E+02   #  h^0            
        35     1.90696191E+03   #  H^0            
        36     1.89445374E+03   #  A^0            
        37     1.90863733E+03   #  H^+            
   1000001     4.04528125E+03   #  dnl            
   1000002     4.04448267E+03   #  upl            
   1000003     4.04528125E+03   #  stl            
   1000004     4.04448291E+03   #  chl            
   1000005     3.80677075E+03   #  b1             
   1000006     3.48825659E+03   #  t1             
   1000011     1.40081824E+03   #  el-            
   1000012     1.39110950E+03   #  nuel           
   1000013     1.40081824E+03   #  mul-           
   1000014     1.39110950E+03   #  numl           
   1000015     6.99892212E+02   #  tau1           
   1000016     1.38343250E+03   #  nutl           
   1000021     2.93448633E+03   #  glss           
   1000022     5.76420471E+02   #  z1ss           
   1000023     1.09620862E+03   #  z2ss           
   1000024     1.09650537E+03   #  w1ss           
   1000025    -1.33980164E+03   #  z3ss           
   1000035     1.35235107E+03   #  z4ss           
   1000037     1.35329163E+03   #  w2ss           
   1000039     2.71552453E-05   #  gvss
   2000001     3.82419287E+03   #  dnr            
   2000002     3.84560840E+03   #  upr            
   2000003     3.82419287E+03   #  str            
   2000004     3.84560864E+03   #  chr            
   2000005     3.89245093E+03   #  b2             
   2000006     3.91647046E+03   #  t2             
   2000011     6.97886292E+02   #  er-            
   2000013     6.97886292E+02   #  mur-           
   2000015     1.39374268E+03   #  tau2           
Block ALPHA   # Effective Higgs mixing parameter
         -6.69743344E-02   # alpha
Block STOPMIX   # stop mixing matrix
  1  1     2.58747824E-02   # O_{11}
  1  2     9.99665201E-01   # O_{12}
  2  1    -9.99665201E-01   # O_{21}
  2  2     2.58747824E-02   # O_{22}
Block SBOTMIX   # sbottom mixing matrix
  1  1     9.74870622E-02   # O_{11}
  1  2     9.95236814E-01   # O_{12}
  2  1    -9.95236814E-01   # O_{21}
  2  2     9.74870622E-02   # O_{22}
Block STAUMIX   # stau mixing matrix
  1  1     2.11179815E-02   # O_{11}
  1  2     9.99777019E-01   # O_{12}
  2  1    -9.99777019E-01   # O_{21}
  2  2     2.11179815E-02   # O_{22}
Block NMIX   # neutralino mixing matrix
  1  1     9.99031484E-01   #
  1  2    -3.02635832E-03   #
  1  3     3.97553295E-02   #
  1  4    -1.86151862E-02   #
  2  1    -1.24208257E-02   #
  2  2    -9.74752247E-01   #
  2  3     1.70876071E-01   #
  2  4    -1.43195763E-01   #
  3  1     1.48709267E-02   #
  3  2    -2.01186221E-02   #
  3  3    -7.06436872E-01   #
  3  4    -7.07333803E-01   #
  4  1    -3.95062752E-02   #
  4  2     2.22360089E-01   #
  4  3     6.85688019E-01   #
  4  4    -6.91973627E-01   #
Block UMIX   # chargino U mixing matrix
  1  1    -9.72058535E-01   # U_{11}
  1  2     2.34738559E-01   # U_{12}
  2  1    -2.34738559E-01   # U_{21}
  2  2    -9.72058535E-01   # U_{22}
Block VMIX   # chargino V mixing matrix
  1  1    -9.80569005E-01   # V_{11}
  1  2     1.96174517E-01   # V_{12}
  2  1    -1.96174517E-01   # V_{21}
  2  2    -9.80569005E-01   # V_{22}
Block GAUGE Q=  3.58497461E+03   #
     1     3.57515812E-01   # g`
     2     6.52411342E-01   # g_2
     3     1.21975732E+00   # g_3
Block YU Q=  3.58497461E+03   #
  3  3     8.29156935E-01   # y_t
Block YD Q=  3.58497461E+03   #
  3  3     1.82276025E-01   # y_b
Block YE Q=  3.58497461E+03   #
  3  3     1.53062671E-01   # y_tau
Block HMIX Q=  3.58497461E+03   # Higgs mixing parameters
     1     1.32352466E+03   # mu(Q)
     2     1.42893572E+01   # tan(beta)(Q)
     3     2.52003204E+02   # Higgs vev at Q
     4     3.58895500E+06   # m_A^2(Q)
Block MSOFT Q=  3.58497461E+03   # DRbar SUSY breaking parameters
     1     5.87416443E+02   # M_1(Q)          
     2     1.06661206E+03   # M_2(Q)          
     3     2.67127808E+03   # M_3(Q)          
    21     1.74093962E+06   # MHd^2(Q)        
    22    -1.52932100E+06   # MHu^2(Q)        
    31     1.38928906E+03   # MeL(Q)          
    32     1.38928906E+03   # MmuL(Q)         
    33     1.38178503E+03   # MtauL(Q)        
    34     6.97541687E+02   # MeR(Q)          
    35     6.97541687E+02   # MmuR(Q)         
    36     6.87265747E+02   # MtauR(Q)        
    41     3.91412061E+03   # MqL1(Q)         
    42     3.91412061E+03   # MqL2(Q)         
    43     3.77201123E+03   # MqL3(Q)         
    44     3.71239600E+03   # MuR(Q)          
    45     3.71239600E+03   # McR(Q)          
    46     3.40721240E+03   # MtR(Q)          
    47     3.69041504E+03   # MdR(Q)          
    48     3.69041504E+03   # MsR(Q)          
    49     3.67758057E+03   # MbR(Q)          
Block AU Q=  3.58497461E+03   #
  1  1    -7.77896240E+02   # A_u
  2  2    -7.77896240E+02   # A_c
  3  3    -7.77896240E+02   # A_t
Block AD Q=  3.58497461E+03   #
  1  1    -8.65318970E+02   # A_d
  2  2    -8.65318970E+02   # A_s
  3  3    -8.65318970E+02   # A_b
Block AE Q=  3.58497461E+03   #
  1  1    -1.05560417E+02   # A_e
  2  2    -1.05560417E+02   # A_mu
  3  3    -1.05560417E+02   # A_tau
#  ISAJET decay tables in SUSY Les Houches accord format
#  Created by ISALHD. Last revision: C. Balazs, 2005 May 25
Block DCINFO                           # Program information
     1   ISASUGRA from ISAJET          # Spectrum Calculator
     2   7.87   18-JUL-2017 13:55:55   # Version number
#         PDG         Width
DECAY         6  1.48575687E+00   # TP    decays
#          BR          NDA       ID1       ID2       ID3       ID4
      3.33333313E-01    3            2        -1         5             # TP     -->  UP     DB     BT          
      3.33333313E-01    3            4        -3         5             # TP     -->  CH     SB     BT          
      1.11111097E-01    3          -11        12         5             # TP     -->  E+     NUE    BT          
      1.11111097E-01    3          -13        14         5             # TP     -->  MU+    NUM    BT          
      1.11111097E-01    3          -15        16         5             # TP     -->  TAU+   NUT    BT          
#         PDG         Width
DECAY   1000021  7.26931021E-02   # GLSS  decays
#          BR          NDA       ID1       ID2       ID3       ID4
      4.85481881E-02    3      1000024         1        -2             # GLSS   -->  W1SS+  DN     UB          
      4.85481881E-02    3     -1000024         2        -1             # GLSS   -->  W1SS-  UP     DB          
      4.85481806E-02    3      1000024         3        -4             # GLSS   -->  W1SS+  ST     CB          
      4.85481806E-02    3     -1000024         4        -3             # GLSS   -->  W1SS-  CH     SB          
      5.45948446E-02    3      1000024         5        -6             # GLSS   -->  W1SS+  BT     TB          
      5.45948446E-02    3     -1000024         6        -5             # GLSS   -->  W1SS-  TP     BB          
      1.52783410E-03    3      1000037         1        -2             # GLSS   -->  W2SS+  DN     UB          
      1.52783410E-03    3     -1000037         2        -1             # GLSS   -->  W2SS-  UP     DB          
      1.52783399E-03    3      1000037         3        -4             # GLSS   -->  W2SS+  ST     CB          
      1.52783399E-03    3     -1000037         4        -3             # GLSS   -->  W2SS-  CH     SB          
      1.00879602E-01    3      1000037         5        -6             # GLSS   -->  W2SS+  BT     TB          
      1.00879602E-01    3     -1000037         6        -5             # GLSS   -->  W2SS-  TP     BB          
      9.78349453E-06    2      1000022        21                       # GLSS   -->  Z1SS   GL                 
      3.19657661E-02    3      1000022         2        -2             # GLSS   -->  Z1SS   UP     UB          
      9.38073918E-03    3      1000022         1        -1             # GLSS   -->  Z1SS   DN     DB          
      9.38073918E-03    3      1000022         3        -3             # GLSS   -->  Z1SS   ST     SB          
      3.19657549E-02    3      1000022         4        -4             # GLSS   -->  Z1SS   CH     CB          
      9.96896438E-03    3      1000022         5        -5             # GLSS   -->  Z1SS   BT     BB          
      5.04293256E-02    3      1000022         6        -6             # GLSS   -->  Z1SS   TP     TB          
      1.14745220E-04    2      1000023        21                       # GLSS   -->  Z2SS   GL                 
      2.43362747E-02    3      1000023         2        -2             # GLSS   -->  Z2SS   UP     UB          
      2.40810979E-02    3      1000023         1        -1             # GLSS   -->  Z2SS   DN     DB          
      2.40810979E-02    3      1000023         3        -3             # GLSS   -->  Z2SS   ST     SB          
      2.43362654E-02    3      1000023         4        -4             # GLSS   -->  Z2SS   CH     CB          
      3.04553993E-02    3      1000023         5        -5             # GLSS   -->  Z2SS   BT     BB          
      2.43968405E-02    3      1000023         6        -6             # GLSS   -->  Z2SS   TP     TB          
      1.72032171E-03    2      1000025        21                       # GLSS   -->  Z3SS   GL                 
      3.36169637E-06    3      1000025         2        -2             # GLSS   -->  Z3SS   UP     UB          
      4.06357822E-06    3      1000025         1        -1             # GLSS   -->  Z3SS   DN     DB          
      4.06357822E-06    3      1000025         3        -3             # GLSS   -->  Z3SS   ST     SB          
      3.36169501E-06    3      1000025         4        -4             # GLSS   -->  Z3SS   CH     CB          
      3.97158600E-03    3      1000025         5        -5             # GLSS   -->  Z3SS   BT     BB          
      8.44595060E-02    3      1000025         6        -6             # GLSS   -->  Z3SS   TP     TB          
      1.63433875E-03    2      1000035        21                       # GLSS   -->  Z4SS   GL                 
      7.79527763E-04    3      1000035         2        -2             # GLSS   -->  Z4SS   UP     UB          
      8.70542426E-04    3      1000035         1        -1             # GLSS   -->  Z4SS   DN     DB          
      8.70542426E-04    3      1000035         3        -3             # GLSS   -->  Z4SS   ST     SB          
      7.79527414E-04    3      1000035         4        -4             # GLSS   -->  Z4SS   CH     CB          
      4.74894140E-03    3      1000035         5        -5             # GLSS   -->  Z4SS   BT     BB          
      9.39944163E-02    3      1000035         6        -6             # GLSS   -->  Z4SS   TP     TB          
      4.67354194E-12    2      1000039        21                       # GLSS   -->  GVSS   GL                 
#         PDG         Width
DECAY   1000002  9.29547501E+01   # UPL   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      5.69560053E-03    2      1000022         2                       # UPL    -->  Z1SS   UP                 
      1.50087401E-01    2      1000023         2                       # UPL    -->  Z2SS   UP                 
      4.39184332E-05    2      1000025         2                       # UPL    -->  Z3SS   UP                 
      6.68640854E-03    2      1000035         2                       # UPL    -->  Z4SS   UP                 
      5.24041116E-01    2      1000021         2                       # UPL    -->  GLSS   UP                 
      3.02328646E-01    2      1000024         1                       # UPL    -->  W1SS+  DN                 
      1.11169387E-02    2      1000037         1                       # UPL    -->  W2SS+  DN                 
#         PDG         Width
DECAY   1000001  9.29743042E+01   # DNL   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      6.08509872E-03    2      1000022         1                       # DNL    -->  Z1SS   DN                 
      1.48699775E-01    2      1000023         1                       # DNL    -->  Z2SS   DN                 
      7.57505113E-05    2      1000025         1                       # DNL    -->  Z3SS   DN                 
      7.61720631E-03    2      1000035         1                       # DNL    -->  Z4SS   DN                 
      5.24485171E-01    2      1000021         1                       # DNL    -->  GLSS   DN                 
      2.97118336E-01    2     -1000024         2                       # DNL    -->  W1SS-  UP                 
      1.59186609E-02    2     -1000037         2                       # DNL    -->  W2SS-  UP                 
#         PDG         Width
DECAY   1000003  9.29742889E+01   # STL   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      6.08509965E-03    2      1000022         3                       # STL    -->  Z1SS   ST                 
      1.48699805E-01    2      1000023         3                       # STL    -->  Z2SS   ST                 
      7.57505186E-05    2      1000025         3                       # STL    -->  Z3SS   ST                 
      7.61720724E-03    2      1000035         3                       # STL    -->  Z4SS   ST                 
      5.24485230E-01    2      1000021         3                       # STL    -->  GLSS   ST                 
      2.97118306E-01    2     -1000024         4                       # STL    -->  W1SS-  CH                 
      1.59186590E-02    2     -1000037         4                       # STL    -->  W2SS-  CH                 
#         PDG         Width
DECAY   1000004  9.29547119E+01   # CHL   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      5.69560146E-03    2      1000022         4                       # CHL    -->  Z1SS   CH                 
      1.50087416E-01    2      1000023         4                       # CHL    -->  Z2SS   CH                 
      4.39184405E-05    2      1000025         4                       # CHL    -->  Z3SS   CH                 
      6.68641040E-03    2      1000035         4                       # CHL    -->  Z4SS   CH                 
      5.24040937E-01    2      1000021         4                       # CHL    -->  GLSS   CH                 
      3.02328765E-01    2      1000024         3                       # CHL    -->  W1SS+  ST                 
      1.11169452E-02    2      1000037         3                       # CHL    -->  W2SS+  ST                 
#         PDG         Width
DECAY   1000005  4.06664047E+01   # BT1   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      5.07951044E-02    2      1000022         5                       # BT1    -->  Z1SS   BT                 
      8.89076944E-03    2      1000023         5                       # BT1    -->  Z2SS   BT                 
      2.43703406E-02    2      1000025         5                       # BT1    -->  Z3SS   BT                 
      1.95183791E-02    2      1000035         5                       # BT1    -->  Z4SS   BT                 
      8.33049595E-01    2      1000021         5                       # BT1    -->  GLSS   BT                 
      1.74844228E-02    2     -1000024         6                       # BT1    -->  W1SS-  TP                 
      4.58703972E-02    2     -1000037         6                       # BT1    -->  W2SS-  TP                 
      2.09732825E-05    2          -24   1000006                       # BT1    -->  W-     TP1                
#         PDG         Width
DECAY   1000006  8.86201019E+01   # TP1   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      1.74909577E-01    2      1000021         6                       # TP1    -->  GLSS   TP                 
      8.37388411E-02    2      1000022         6                       # TP1    -->  Z1SS   TP                 
      1.02620665E-02    2      1000023         6                       # TP1    -->  Z2SS   TP                 
      1.85647875E-01    2      1000025         6                       # TP1    -->  Z3SS   TP                 
      1.73988372E-01    2      1000035         6                       # TP1    -->  Z4SS   TP                 
      1.94890276E-02    2      1000024         5                       # TP1    -->  W1SS+  BT                 
      3.51964265E-01    2      1000037         5                       # TP1    -->  W2SS+  BT                 
#         PDG         Width
DECAY   2000002  4.45031891E+01   # UPR   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      1.86299309E-01    2      1000022         2                       # UPR    -->  Z1SS   UP                 
      2.54378283E-05    2      1000023         2                       # UPR    -->  Z2SS   UP                 
      3.33478165E-05    2      1000025         2                       # UPR    -->  Z3SS   UP                 
      2.34132749E-04    2      1000035         2                       # UPR    -->  Z4SS   UP                 
      8.13407719E-01    2      1000021         2                       # UPR    -->  GLSS   UP                 
#         PDG         Width
DECAY   2000001  3.69602280E+01   # DNR   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      5.57388812E-02    2      1000022         1                       # DNR    -->  Z1SS   DN                 
      7.59955219E-06    2      1000023         1                       # DNR    -->  Z2SS   DN                 
      9.95153005E-06    2      1000025         1                       # DNR    -->  Z3SS   DN                 
      6.98643562E-05    2      1000035         1                       # DNR    -->  Z4SS   DN                 
      9.44173634E-01    2      1000021         1                       # DNR    -->  GLSS   DN                 
#         PDG         Width
DECAY   2000003  3.69602280E+01   # STR   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      5.57388812E-02    2      1000022         3                       # STR    -->  Z1SS   ST                 
      7.59955219E-06    2      1000023         3                       # STR    -->  Z2SS   ST                 
      9.95153005E-06    2      1000025         3                       # STR    -->  Z3SS   ST                 
      6.98643562E-05    2      1000035         3                       # STR    -->  Z4SS   ST                 
      9.44173634E-01    2      1000021         3                       # STR    -->  GLSS   ST                 
#         PDG         Width
DECAY   2000004  4.45031548E+01   # CHR   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      1.86299413E-01    2      1000022         4                       # CHR    -->  Z1SS   CH                 
      2.54378410E-05    2      1000023         4                       # CHR    -->  Z2SS   CH                 
      3.33478347E-05    2      1000025         4                       # CHR    -->  Z3SS   CH                 
      2.34132836E-04    2      1000035         4                       # CHR    -->  Z4SS   CH                 
      8.13407660E-01    2      1000021         4                       # CHR    -->  GLSS   CH                 
#         PDG         Width
DECAY   2000005  1.21681343E+02   # BT2   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      4.39198641E-03    2      1000022         5                       # BT2    -->  Z1SS   BT                 
      1.05967246E-01    2      1000023         5                       # BT2    -->  Z2SS   BT                 
      8.80608428E-03    2      1000025         5                       # BT2    -->  Z3SS   BT                 
      1.48746474E-02    2      1000035         5                       # BT2    -->  Z4SS   BT                 
      3.20898354E-01    2      1000021         5                       # BT2    -->  GLSS   BT                 
      2.20246717E-01    2     -1000024         6                       # BT2    -->  W1SS-  TP                 
      3.23312372E-01    2     -1000037         6                       # BT2    -->  W2SS-  TP                 
      1.50257780E-03    2          -24   1000006                       # BT2    -->  W-     TP1                
#         PDG         Width
DECAY   2000006  1.23881271E+02   # TP2   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      3.18363786E-01    2      1000021         6                       # TP2    -->  GLSS   TP                 
      2.15431333E-01    2      1000024         5                       # TP2    -->  W1SS+  BT                 
      2.67519783E-02    2      1000037         5                       # TP2    -->  W2SS+  BT                 
      8.81211308E-04    2           23   1000006                       # TP2    -->  Z0     TP1                
      2.98987888E-03    2           25   1000006                       # TP2    -->  HL0    TP1                
      1.58864175E-04    2           24   1000005                       # TP2    -->  W+     BT1                
      4.17865533E-03    2      1000022         6                       # TP2    -->  Z1SS   TP                 
      1.11570880E-01    2      1000023         6                       # TP2    -->  Z2SS   TP                 
      1.59269124E-01    2      1000025         6                       # TP2    -->  Z3SS   TP                 
      1.60404205E-01    2      1000035         6                       # TP2    -->  Z4SS   TP                 
#         PDG         Width
DECAY   1000011  3.74218583E+00   # EL-   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      3.24203581E-01    2      1000022        11                       # EL-    -->  Z1SS   E-                 
      2.28100121E-01    2      1000023        11                       # EL-    -->  Z2SS   E-                 
      1.63277241E-06    2      1000025        11                       # EL-    -->  Z3SS   E-                 
      2.93336809E-04    2      1000035        11                       # EL-    -->  Z4SS   E-                 
      4.46628690E-01    2     -1000024        12                       # EL-    -->  W1SS-  NUE                
      7.72638363E-04    2     -1000037        12                       # EL-    -->  W2SS-  NUE                
      2.25041450E-15    2           11   1000039                       # EL-    -->  E-     GVSS               
#         PDG         Width
DECAY   1000013  3.74218583E+00   # MUL-  decays
#          BR          NDA       ID1       ID2       ID3       ID4
      3.24203581E-01    2      1000022        13                       # MUL-   -->  Z1SS   MU-                
      2.28100121E-01    2      1000023        13                       # MUL-   -->  Z2SS   MU-                
      1.63276979E-06    2      1000025        13                       # MUL-   -->  Z3SS   MU-                
      2.93336023E-04    2      1000035        13                       # MUL-   -->  Z4SS   MU-                
      4.46628690E-01    2     -1000024        14                       # MUL-   -->  W1SS-  NUM                
      7.72638363E-04    2     -1000037        14                       # MUL-   -->  W2SS-  NUM                
      2.25041450E-15    2           13   1000039                       # MUL-   -->  MU-    GVSS               
#         PDG         Width
DECAY   1000015  3.67681801E-01   # TAU1- decays
#          BR          NDA       ID1       ID2       ID3       ID4
      1.00000000E+00    2      1000022        15                       # TAU1-  -->  Z1SS   TAU-               
      7.13101421E-16    2           15   1000039                       # TAU1-  -->  TAU-   GVSS               
#         PDG         Width
DECAY   1000012  3.63015914E+00   # NUEL  decays
#          BR          NDA       ID1       ID2       ID3       ID4
      3.37355763E-01    2      1000022        12                       # NUEL   -->  Z1SS   NUE                
      2.17120662E-01    2      1000023        12                       # NUEL   -->  Z2SS   NUE                
      6.76881382E-06    2      1000025        12                       # NUEL   -->  Z3SS   NUE                
      2.90117430E-04    2      1000035        12                       # NUEL   -->  Z4SS   NUE                
      4.44869637E-01    2      1000024        11                       # NUEL   -->  W1SS+  E-                 
      3.57113837E-04    2      1000037        11                       # NUEL   -->  W2SS+  E-                 
      2.24057663E-15    2           12   1000039                       # NUEL   -->  NUE    GVSS               
#         PDG         Width
DECAY   1000014  3.63015890E+00   # NUML  decays
#          BR          NDA       ID1       ID2       ID3       ID4
      3.37355793E-01    2      1000022        14                       # NUML   -->  Z1SS   NUM                
      2.17120677E-01    2      1000023        14                       # NUML   -->  Z2SS   NUM                
      6.76881427E-06    2      1000025        14                       # NUML   -->  Z3SS   NUM                
      2.90117430E-04    2      1000035        14                       # NUML   -->  Z4SS   NUM                
      4.44869608E-01    2      1000024        13                       # NUML   -->  W1SS+  MU-                
      3.57112498E-04    2      1000037        13                       # NUML   -->  W2SS+  MU-                
      2.24057685E-15    2           14   1000039                       # NUML   -->  NUM    GVSS               
#         PDG         Width
DECAY   1000016  3.83199096E+00   # NUTL  decays
#          BR          NDA       ID1       ID2       ID3       ID4
      3.16358775E-01    2      1000022        16                       # NUTL   -->  Z1SS   NUT                
      1.97158828E-01    2      1000023        16                       # NUTL   -->  Z2SS   NUT                
      4.68812459E-06    2      1000025        16                       # NUTL   -->  Z3SS   NUT                
      1.78698712E-04    2      1000035        16                       # NUTL   -->  Z4SS   NUT                
      4.05025393E-01    2      1000024        15                       # NUTL   -->  W1SS+  TAU-               
      5.55934268E-04    2      1000037        15                       # NUTL   -->  W2SS+  TAU-               
      8.07177052E-02    2           24   1000015                       # NUTL   -->  W+     TAU1-              
      2.06463984E-15    2           16   1000039                       # NUTL   -->  NUT    GVSS               
#         PDG         Width
DECAY   2000011  3.57815623E-01   # ER-   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      1.00000000E+00    2      1000022        11                       # ER-    -->  Z1SS   E-                 
      7.22342127E-16    2           11   1000039                       # ER-    -->  E-     GVSS               
#         PDG         Width
DECAY   2000013  3.57815444E-01   # MUR-  decays
#          BR          NDA       ID1       ID2       ID3       ID4
      1.00000000E+00    2      1000022        13                       # MUR-   -->  Z1SS   MU-                
      7.22342498E-16    2           13   1000039                       # MUR-   -->  MU-    GVSS               
#         PDG         Width
DECAY   2000015  4.01367950E+00   # TAU2- decays
#          BR          NDA       ID1       ID2       ID3       ID4
      2.99759030E-01    2      1000022        15                       # TAU2-  -->  Z1SS   TAU-               
      2.04836190E-01    2      1000023        15                       # TAU2-  -->  Z2SS   TAU-               
      4.88678343E-04    2      1000025        15                       # TAU2-  -->  Z3SS   TAU-               
      5.06315148E-04    2      1000035        15                       # TAU2-  -->  Z4SS   TAU-               
      3.99914265E-01    2     -1000024        16                       # TAU2-  -->  W1SS-  NUT                
      5.49315824E-04    2     -1000037        16                       # TAU2-  -->  W2SS-  NUT                
      4.02768999E-02    2           23   1000015                       # TAU2-  -->  Z0     TAU1-              
      5.36692105E-02    2           25   1000015                       # TAU2-  -->  HL0    TAU1-              
#         PDG         Width
DECAY   1000022  9.87031876E-17   # Z1SS  decays
#          BR          NDA       ID1       ID2       ID3       ID4
      7.68983364E-01    2      1000039        22                       # Z1SS   -->  GVSS   GM                 
      1.77665576E-02    3      1000039        11       -11             # Z1SS   -->  GVSS   E-     E+          
      2.13143364E-01    2      1000039        23                       # Z1SS   -->  GVSS   Z0                 
      1.06719221E-04    2      1000039        25                       # Z1SS   -->  GVSS   HL0                
#         PDG         Width
DECAY   1000023  5.59686348E-02   # Z2SS  decays
#          BR          NDA       ID1       ID2       ID3       ID4
      1.89166849E-06    2      1000022        22                       # Z2SS   -->  Z1SS   GM                 
      5.20928539E-02    2      1000022        23                       # Z2SS   -->  Z1SS   Z0                 
      9.62325203E-07    3      1000022         2        -2             # Z2SS   -->  Z1SS   UP     UB          
      1.01604053E-06    3      1000022         1        -1             # Z2SS   -->  Z1SS   DN     DB          
      1.01604053E-06    3      1000022         3        -3             # Z2SS   -->  Z1SS   ST     SB          
      9.62324975E-07    3      1000022         4        -4             # Z2SS   -->  Z1SS   CH     CB          
      2.67794940E-06    3      1000022         5        -5             # Z2SS   -->  Z1SS   BT     BB          
      4.91894840E-04    3      1000022        11       -11             # Z2SS   -->  Z1SS   E-     E+          
      4.91894840E-04    3      1000022        13       -13             # Z2SS   -->  Z1SS   MU-    MU+         
      5.08024881E-04    3      1000022        15       -15             # Z2SS   -->  Z1SS   TAU-   TAU+        
      5.12153085E-04    3      1000022        12       -12             # Z2SS   -->  Z1SS   NUE    ANUE        
      5.12153085E-04    3      1000022        14       -14             # Z2SS   -->  Z1SS   NUM    ANUM        
      5.31574595E-04    3      1000022        16       -16             # Z2SS   -->  Z1SS   NUT    ANUT        
      7.49669969E-01    2      1000022        25                       # Z2SS   -->  Z1SS   HL0                
      2.71772523E-03    2      2000011       -11                       # Z2SS   -->  ER-    E+                 
      2.71772523E-03    2     -2000011        11                       # Z2SS   -->  ER+    E-                 
      2.71772500E-03    2      2000013       -13                       # Z2SS   -->  MUR-   MU+                
      2.71772500E-03    2     -2000013        13                       # Z2SS   -->  MUR+   MU-                
      9.21549946E-02    2      1000015       -15                       # Z2SS   -->  TAU1-  TAU+               
      9.21549946E-02    2     -1000015        15                       # Z2SS   -->  TAU1+  TAU-               
      1.01902400E-14    2      1000039        22                       # Z2SS   -->  GVSS   GM                 
      2.46293941E-16    3      1000039        11       -11             # Z2SS   -->  GVSS   E-     E+          
      3.14129758E-14    2      1000039        23                       # Z2SS   -->  GVSS   Z0                 
      3.63453579E-16    2      1000039        25                       # Z2SS   -->  GVSS   HL0                
#         PDG         Width
DECAY   1000025  4.35374546E+00   # Z3SS  decays
#          BR          NDA       ID1       ID2       ID3       ID4
      1.08589489E-07    2      1000022        22                       # Z3SS   -->  Z1SS   GM                 
      1.87881966E-07    2      1000023        22                       # Z3SS   -->  Z2SS   GM                 
      2.56211907E-01    2      1000024       -24                       # Z3SS   -->  W1SS+  W-                 
      2.56211907E-01    2     -1000024        24                       # Z3SS   -->  W1SS-  W+                 
      1.64698869E-01    2      1000022        23                       # Z3SS   -->  Z1SS   Z0                 
      2.59101093E-01    2      1000023        23                       # Z3SS   -->  Z2SS   Z0                 
      7.93883084E-11    3      1000022         2        -2             # Z3SS   -->  Z1SS   UP     UB          
      2.06473068E-11    3      1000022         1        -1             # Z3SS   -->  Z1SS   DN     DB          
      2.06473068E-11    3      1000022         3        -3             # Z3SS   -->  Z1SS   ST     SB          
      7.93882945E-11    3      1000022         4        -4             # Z3SS   -->  Z1SS   CH     CB          
      9.61276783E-07    3      1000022         5        -5             # Z3SS   -->  Z1SS   BT     BB          
      5.57890223E-09    3      1000022        11       -11             # Z3SS   -->  Z1SS   E-     E+          
      5.57890223E-09    3      1000022        13       -13             # Z3SS   -->  Z1SS   MU-    MU+         
      2.85296619E-06    3      1000022        15       -15             # Z3SS   -->  Z1SS   TAU-   TAU+        
      3.53591538E-08    3      1000022        12       -12             # Z3SS   -->  Z1SS   NUE    ANUE        
      3.53591538E-08    3      1000022        14       -14             # Z3SS   -->  Z1SS   NUM    ANUM        
      3.84935142E-08    3      1000022        16       -16             # Z3SS   -->  Z1SS   NUT    ANUT        
      1.75501389E-12    3      1000023         2        -2             # Z3SS   -->  Z2SS   UP     UB          
      2.99593054E-12    3      1000023         1        -1             # Z3SS   -->  Z2SS   DN     DB          
      2.99593054E-12    3      1000023         3        -3             # Z3SS   -->  Z2SS   ST     SB          
      1.75501367E-12    3      1000023         4        -4             # Z3SS   -->  Z2SS   CH     CB          
      1.86692546E-08    3      1000023         5        -5             # Z3SS   -->  Z2SS   BT     BB          
      3.90550203E-10    3      1000023        11       -11             # Z3SS   -->  Z2SS   E-     E+          
      3.90550203E-10    3      1000023        13       -13             # Z3SS   -->  Z2SS   MU-    MU+         
      3.37682906E-07    3      1000023        15       -15             # Z3SS   -->  Z2SS   TAU-   TAU+        
      2.51746424E-09    3      1000023        12       -12             # Z3SS   -->  Z2SS   NUE    ANUE        
      2.51746424E-09    3      1000023        14       -14             # Z3SS   -->  Z2SS   NUM    ANUM        
      2.91080759E-09    3      1000023        16       -16             # Z3SS   -->  Z2SS   NUT    ANUT        
      2.18402948E-02    2      1000022        25                       # Z3SS   -->  Z1SS   HL0                
      1.91726722E-03    2      1000023        25                       # Z3SS   -->  Z2SS   HL0                
      9.18948499E-05    2      2000011       -11                       # Z3SS   -->  ER-    E+                 
      9.18948499E-05    2     -2000011        11                       # Z3SS   -->  ER+    E-                 
      9.18948499E-05    2      2000013       -13                       # Z3SS   -->  MUR-   MU+                
      9.18948499E-05    2     -2000013        13                       # Z3SS   -->  MUR+   MU-                
      1.98233426E-02    2      1000015       -15                       # Z3SS   -->  TAU1-  TAU+               
      1.98233426E-02    2     -1000015        15                       # Z3SS   -->  TAU1+  TAU-               
      1.72895434E-20    2      1000039        22                       # Z3SS   -->  GVSS   GM                 
      4.23632947E-22    3      1000039        11       -11             # Z3SS   -->  GVSS   E-     E+          
      3.30696380E-16    2      1000039        23                       # Z3SS   -->  GVSS   Z0                 
      4.25009237E-16    2      1000039        25                       # Z3SS   -->  GVSS   HL0                
#         PDG         Width
DECAY   1000035  5.13637924E+00   # Z4SS  decays
#          BR          NDA       ID1       ID2       ID3       ID4
      1.53954200E-08    2      1000022        22                       # Z4SS   -->  Z1SS   GM                 
      3.71390207E-08    2      1000023        22                       # Z4SS   -->  Z2SS   GM                 
      1.83132939E-10    2      1000025        22                       # Z4SS   -->  Z3SS   GM                 
      2.74568677E-01    2      1000024       -24                       # Z4SS   -->  W1SS+  W-                 
      2.74568677E-01    2     -1000024        24                       # Z4SS   -->  W1SS-  W+                 
      1.82929952E-02    2      1000022        23                       # Z4SS   -->  Z1SS   Z0                 
      2.44639977E-03    2      1000023        23                       # Z4SS   -->  Z2SS   Z0                 
      4.14799750E-09    3      1000022         2        -2             # Z4SS   -->  Z1SS   UP     UB          
      3.74928089E-09    3      1000022         1        -1             # Z4SS   -->  Z1SS   DN     DB          
      3.74928089E-09    3      1000022         3        -3             # Z4SS   -->  Z1SS   ST     SB          
      4.14799617E-09    3      1000022         4        -4             # Z4SS   -->  Z1SS   CH     CB          
      7.76635261E-07    3      1000022         5        -5             # Z4SS   -->  Z1SS   BT     BB          
      2.73228375E-06    3      1000022        11       -11             # Z4SS   -->  Z1SS   E-     E+          
      2.73228375E-06    3      1000022        13       -13             # Z4SS   -->  Z1SS   MU-    MU+         
      5.79136713E-06    3      1000022        15       -15             # Z4SS   -->  Z1SS   TAU-   TAU+        
      4.52109634E-06    3      1000022        12       -12             # Z4SS   -->  Z1SS   NUE    ANUE        
      4.52109634E-06    3      1000022        14       -14             # Z4SS   -->  Z1SS   NUM    ANUM        
      4.88255228E-06    3      1000022        16       -16             # Z4SS   -->  Z1SS   NUT    ANUT        
      8.56527016E-10    3      1000023         2        -2             # Z4SS   -->  Z2SS   UP     UB          
      9.65766800E-10    3      1000023         1        -1             # Z4SS   -->  Z2SS   DN     DB          
      9.65766800E-10    3      1000023         3        -3             # Z4SS   -->  Z2SS   ST     SB          
      8.56526794E-10    3      1000023         4        -4             # Z4SS   -->  Z2SS   CH     CB          
      1.76223054E-08    3      1000023         5        -5             # Z4SS   -->  Z2SS   BT     BB          
      3.20250365E-07    3      1000023        11       -11             # Z4SS   -->  Z2SS   E-     E+          
      3.20250365E-07    3      1000023        13       -13             # Z4SS   -->  Z2SS   MU-    MU+         
      7.74417913E-07    3      1000023        15       -15             # Z4SS   -->  Z2SS   TAU-   TAU+        
      5.39020220E-07    3      1000023        12       -12             # Z4SS   -->  Z2SS   NUE    ANUE        
      5.39020220E-07    3      1000023        14       -14             # Z4SS   -->  Z2SS   NUM    ANUM        
      6.18095783E-07    3      1000023        16       -16             # Z4SS   -->  Z2SS   NUT    ANUT        
      7.25339255E-09    3      1000025         2        -2             # Z4SS   -->  Z3SS   UP     UB          
      9.35338829E-09    3      1000025         1        -1             # Z4SS   -->  Z3SS   DN     DB          
      9.35338829E-09    3      1000025         3        -3             # Z4SS   -->  Z3SS   ST     SB          
      7.25339255E-09    3      1000025         4        -4             # Z4SS   -->  Z3SS   CH     CB          
      1.00527764E-09    3      1000025         5        -5             # Z4SS   -->  Z3SS   BT     BB          
      2.12227214E-09    3      1000025        11       -11             # Z4SS   -->  Z3SS   E-     E+          
      2.12227214E-09    3      1000025        13       -13             # Z4SS   -->  Z3SS   MU-    MU+         
      1.66298098E-09    3      1000025        15       -15             # Z4SS   -->  Z3SS   TAU-   TAU+        
      4.21861657E-09    3      1000025        12       -12             # Z4SS   -->  Z3SS   NUE    ANUE        
      4.21861657E-09    3      1000025        14       -14             # Z4SS   -->  Z3SS   NUM    ANUM        
      4.21796420E-09    3      1000025        16       -16             # Z4SS   -->  Z3SS   NUT    ANUT        
      1.45937398E-01    2      1000022        25                       # Z4SS   -->  Z1SS   HL0                
      2.49841303E-01    2      1000023        25                       # Z4SS   -->  Z2SS   HL0                
      5.62543864E-04    2      2000011       -11                       # Z4SS   -->  ER-    E+                 
      5.62543864E-04    2     -2000011        11                       # Z4SS   -->  ER+    E-                 
      5.62543864E-04    2      2000013       -13                       # Z4SS   -->  MUR-   MU+                
      5.62543864E-04    2     -2000013        13                       # Z4SS   -->  MUR+   MU-                
      1.60325579E-02    2      1000015       -15                       # Z4SS   -->  TAU1-  TAU+               
      1.60325579E-02    2     -1000015        15                       # Z4SS   -->  TAU1+  TAU-               
      7.22304202E-18    2      1000039        22                       # Z4SS   -->  GVSS   GM                 
      1.77092519E-19    3      1000039        11       -11             # Z4SS   -->  GVSS   E-     E+          
      4.27482600E-16    2      1000039        23                       # Z4SS   -->  GVSS   Z0                 
      2.76681168E-16    2      1000039        25                       # Z4SS   -->  GVSS   HL0                
#         PDG         Width
DECAY   1000024  4.82888818E-02   # W1SS+ decays
#          BR          NDA       ID1       ID2       ID3       ID4
      2.30363798E-06    3      1000022         2        -1             # W1SS+  -->  Z1SS   UP     DB          
      2.30363753E-06    3      1000022         4        -3             # W1SS+  -->  Z1SS   CH     SB          
      1.17161695E-03    3      1000022       -11        12             # W1SS+  -->  Z1SS   E+     NUE         
      1.17161695E-03    3      1000022       -13        14             # W1SS+  -->  Z1SS   MU+    NUM         
      1.21327909E-03    3      1000022       -15        16             # W1SS+  -->  Z1SS   TAU+   NUT         
      7.97187209E-01    2      1000022        24                       # W1SS+  -->  Z1SS   W+                 
      1.05112501E-12    3      1000023       -11        12             # W1SS+  -->  Z2SS   E+     NUE         
      1.05112501E-12    3      1000023       -13        14             # W1SS+  -->  Z2SS   MU+    NUM         
      1.99251637E-01    2     -1000015        16                       # W1SS+  -->  TAU1+  NUT                
#         PDG         Width
DECAY   1000037  4.62920284E+00   # W2SS+ decays
#          BR          NDA       ID1       ID2       ID3       ID4
      7.04698788E-09    3      1000022         2        -1             # W2SS+  -->  Z1SS   UP     DB          
      7.04698744E-09    3      1000022         4        -3             # W2SS+  -->  Z1SS   CH     SB          
      7.45588568E-06    3      1000022       -11        12             # W2SS+  -->  Z1SS   E+     NUE         
      7.45588568E-06    3      1000022       -13        14             # W2SS+  -->  Z1SS   MU+    NUM         
      1.17310510E-05    3      1000022       -15        16             # W2SS+  -->  Z1SS   TAU+   NUT         
      1.62668422E-01    2      1000022        24                       # W2SS+  -->  Z1SS   W+                 
      6.67117195E-10    3      1000023         2        -1             # W2SS+  -->  Z2SS   UP     DB          
      6.67117195E-10    3      1000023         4        -3             # W2SS+  -->  Z2SS   CH     SB          
      3.93803788E-07    3      1000023       -11        12             # W2SS+  -->  Z2SS   E+     NUE         
      3.93803788E-07    3      1000023       -13        14             # W2SS+  -->  Z2SS   MU+    NUM         
      9.70736323E-07    3      1000023       -15        16             # W2SS+  -->  Z2SS   TAU+   NUT         
      2.54251957E-01    2      1000023        24                       # W2SS+  -->  Z2SS   W+                 
      3.96343367E-08    3      1000025         2        -1             # W2SS+  -->  Z3SS   UP     DB          
      3.96343367E-08    3      1000025         4        -3             # W2SS+  -->  Z3SS   CH     SB          
      1.32091662E-08    3      1000025       -11        12             # W2SS+  -->  Z3SS   E+     NUE         
      1.32091662E-08    3      1000025       -13        14             # W2SS+  -->  Z3SS   MU+    NUM         
      1.32095943E-08    3      1000025       -15        16             # W2SS+  -->  Z3SS   TAU+   NUT         
      1.52175771E-14    3      1000035       -11        12             # W2SS+  -->  Z4SS   E+     NUE         
      1.52175771E-14    3      1000035       -13        14             # W2SS+  -->  Z4SS   MU+    NUM         
      3.46563086E-02    2     -1000015        16                       # W2SS+  -->  TAU1+  NUT                
      2.61637360E-01    2      1000024        23                       # W2SS+  -->  W1SS+  Z0                 
      1.07829146E-09    3      1000024         1        -1             # W2SS+  -->  W1SS+  DN     DB          
      1.07829101E-09    3      1000024         3        -3             # W2SS+  -->  W1SS+  ST     SB          
      1.51590007E-09    3      1000024         2        -2             # W2SS+  -->  W1SS+  UP     UB          
      1.51590007E-09    3      1000024         4        -4             # W2SS+  -->  W1SS+  CH     CB          
      6.97645476E-07    3      1000024        12       -12             # W2SS+  -->  W1SS+  NUE    ANUE        
      6.97645476E-07    3      1000024        14       -14             # W2SS+  -->  W1SS+  NUM    ANUM        
      5.86531883E-07    3      1000024        11       -11             # W2SS+  -->  W1SS+  E-     E+          
      5.86531883E-07    3      1000024        13       -13             # W2SS+  -->  W1SS+  MU-    MU+         
      6.80183803E-07    3      1000024        15       -15             # W2SS+  -->  W1SS+  TAU-   TAU+        
      2.86754102E-01    2      1000024        25                       # W2SS+  -->  W1SS+  HL0                
#         PDG         Width
DECAY        25  3.31367785E-03   # HL0   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      6.23324947E-09    2           11       -11                       # HL0    -->  E-     E+                 
      2.63177848E-04    2           13       -13                       # HL0    -->  MU-    MU+                
      7.52794817E-02    2           15       -15                       # HL0    -->  TAU-   TAU+               
      7.81832478E-06    2            1        -1                       # HL0    -->  DN     DB                 
      3.15899542E-03    2            3        -3                       # HL0    -->  ST     SB                 
      6.76541567E-01    2            5        -5                       # HL0    -->  BT     BB                 
      2.47127218E-06    2            2        -2                       # HL0    -->  UP     UB                 
      4.62148637E-02    2            4        -4                       # HL0    -->  CH     CB                 
      2.66524591E-03    2           22        22                       # HL0    -->  GM     GM                 
      5.53607084E-02    2           21        21                       # HL0    -->  GL     GL                 
      7.13083800E-03    3           24        11       -12             # HL0    -->  W+     E-     ANUE        
      7.13083800E-03    3           24        13       -14             # HL0    -->  W+     MU-    ANUM        
      7.13083800E-03    3           24        15       -16             # HL0    -->  W+     TAU-   ANUT        
      2.13925149E-02    3           24        -2         1             # HL0    -->  W+     UB     DN          
      2.13925149E-02    3           24        -4         3             # HL0    -->  W+     CB     ST          
      7.13083800E-03    3          -24       -11        12             # HL0    -->  W-     E+     NUE         
      7.13083800E-03    3          -24       -13        14             # HL0    -->  W-     MU+    NUM         
      7.13083800E-03    3          -24       -15        16             # HL0    -->  W-     TAU+   NUT         
      2.13925149E-02    3          -24         2        -1             # HL0    -->  W-     UP     DB          
      2.13925149E-02    3          -24         4        -3             # HL0    -->  W-     CH     SB          
      8.30986130E-04    3           23        12       -12             # HL0    -->  Z0     NUE    ANUE        
      8.30986130E-04    3           23        14       -14             # HL0    -->  Z0     NUM    ANUM        
      8.30986130E-04    3           23        16       -16             # HL0    -->  Z0     NUT    ANUT        
      4.18226962E-04    3           23        11       -11             # HL0    -->  Z0     E-     E+          
      4.18226962E-04    3           23        13       -13             # HL0    -->  Z0     MU-    MU+         
      4.18226962E-04    3           23        15       -15             # HL0    -->  Z0     TAU-   TAU+        
      1.43281010E-03    3           23         2        -2             # HL0    -->  Z0     UP     UB          
      1.43281010E-03    3           23         4        -4             # HL0    -->  Z0     CH     CB          
      1.84581243E-03    3           23         1        -1             # HL0    -->  Z0     DN     DB          
      1.84581243E-03    3           23         3        -3             # HL0    -->  Z0     ST     SB          
      1.84581243E-03    3           23         5        -5             # HL0    -->  Z0     BT     BB          
#         PDG         Width
DECAY        35  6.33136272E+00   # HH0   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      1.15158620E-08    2           11       -11                       # HH0    -->  E-     E+                 
      4.86220466E-04    2           13       -13                       # HH0    -->  MU-    MU+                
      1.39260277E-01    2           15       -15                       # HH0    -->  TAU-   TAU+               
      1.40368693E-05    2            1        -1                       # HH0    -->  DN     DB                 
      5.67160593E-03    2            3        -3                       # HH0    -->  ST     SB                 
      7.91340232E-01    2            5        -5                       # HH0    -->  BT     BB                 
      8.98071340E-11    2            2        -2                       # HH0    -->  UP     UB                 
      1.17572495E-06    2            4        -4                       # HH0    -->  CH     CB                 
      6.01865575E-02    2            6        -6                       # HH0    -->  TP     TB                 
      1.03284499E-07    2           22        22                       # HH0    -->  GM     GM                 
      1.38485584E-05    2           21        21                       # HH0    -->  GL     GL                 
      5.81608037E-05    2           24       -24                       # HH0    -->  W+     W-                 
      2.97625265E-05    2           23        23                       # HH0    -->  Z0     Z0                 
      5.76693972E-04    2      1000022   1000022                       # HH0    -->  Z1SS   Z1SS               
      2.14637630E-03    2      1000022   1000023                       # HH0    -->  Z1SS   Z2SS               
      2.01456744E-04    2           25        25                       # HH0    -->  HL0    HL0                
      4.97116753E-06    2      2000011  -2000011                       # HH0    -->  ER-    ER+                
      4.96484972E-06    2      2000013  -2000013                       # HH0    -->  MUR-   MUR+               
      3.61611546E-06    2      1000015  -1000015                       # HH0    -->  TAU1-  TAU1+              
#         PDG         Width
DECAY        36  6.34839916E+00   # HA0   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      1.14102461E-08    2           11       -11                       # HA0    -->  E-     E+                 
      4.81761177E-04    2           13       -13                       # HA0    -->  MU-    MU+                
      1.37983546E-01    2           15       -15                       # HA0    -->  TAU-   TAU+               
      1.39088052E-05    2            1        -1                       # HA0    -->  DN     DB                 
      5.61986072E-03    2            3        -3                       # HA0    -->  ST     SB                 
      7.84129560E-01    2            5        -5                       # HA0    -->  BT     BB                 
      8.79086734E-11    2            2        -2                       # HA0    -->  UP     UB                 
      1.15171019E-06    2            4        -4                       # HA0    -->  CH     CB                 
      6.05200790E-02    2            6        -6                       # HA0    -->  TP     TB                 
      3.46615820E-07    2           22        22                       # HA0    -->  GM     GM                 
      4.42210003E-05    2           21        21                       # HA0    -->  GL     GL                 
      1.01682742E-03    2      1000022   1000022                       # HA0    -->  Z1SS   Z1SS               
      1.01314485E-02    2      1000022   1000023                       # HA0    -->  Z1SS   Z2SS               
      5.71369201E-05    2           25        23                       # HA0    -->  HL0    Z0                 
#         PDG         Width
DECAY        37  5.90398693E+00   # H+    decays
#          BR          NDA       ID1       ID2       ID3       ID4
      1.23609905E-08    2           12       -11                       # H+     -->  NUE    E+                 
      5.21903334E-04    2           14       -13                       # H+     -->  NUM    MU+                
      1.49480850E-01    2           16       -15                       # H+     -->  NUT    TAU+               
      1.39189306E-05    2            2        -1                       # H+     -->  UP     DB                 
      5.62506262E-03    2            4        -3                       # H+     -->  CH     SB                 
      8.31422091E-01    2            6        -5                       # H+     -->  TP     BB                 
      1.28740361E-02    2      1000024   1000022                       # H+     -->  W1SS+  Z1SS               
      6.21227955E-05    2           25        24                       # H+     -->  HL0    W+                 
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

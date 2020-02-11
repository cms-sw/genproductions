
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
     1     3.00000000E+05   # Lambda scale of soft SSB
     2     6.00000000E+05   # M_mess overall messenger scale
     3     1.50000000E+01   # tan(beta)
     4     1.00000000E+00   # sign(mu)
     5     1.00000000E+00   # N_5 messenger index
     6     5.98981018E+02   # c_grav gravitino mass factor
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
        25     1.18836548E+02   #  h^0            
        35     1.46498315E+03   #  H^0            
        36     1.45535034E+03   #  A^0            
        37     1.46716040E+03   #  H^+            
   1000001     3.09674976E+03   #  dnl            
   1000002     3.09570679E+03   #  upl            
   1000003     3.09674976E+03   #  stl            
   1000004     3.09570703E+03   #  chl            
   1000005     2.91869434E+03   #  b1             
   1000006     2.67476221E+03   #  t1             
   1000011     1.05499146E+03   #  el-            
   1000012     1.04608923E+03   #  nuel           
   1000013     1.05499146E+03   #  mul-           
   1000014     1.04608923E+03   #  numl           
   1000015     5.23228699E+02   #  tau1           
   1000016     1.04025134E+03   #  nutl           
   1000021     2.25964966E+03   #  glss           
   1000022     4.30405304E+02   #  z1ss           
   1000023     8.21464722E+02   #  z2ss           
   1000024     8.21884216E+02   #  w1ss           
   1000025    -1.05370386E+03   #  z3ss           
   1000035     1.06723499E+03   #  z4ss           
   1000037     1.06779834E+03   #  w2ss           
   1000039     2.59366388E-05   #  gvss
   2000001     2.93295679E+03   #  dnr            
   2000002     2.94838989E+03   #  upr            
   2000003     2.93295679E+03   #  str            
   2000004     2.94839014E+03   #  chr            
   2000005     2.97879907E+03   #  b2             
   2000006     2.99800195E+03   #  t2             
   2000011     5.21689331E+02   #  er-            
   2000013     5.21689331E+02   #  mur-           
   2000015     1.04971423E+03   #  tau2           
Block ALPHA   # Effective Higgs mixing parameter
         -6.72466829E-02   # alpha
Block STOPMIX   # stop mixing matrix
  1  1     3.52726281E-02   # O_{11}
  1  2     9.99377728E-01   # O_{12}
  2  1    -9.99377728E-01   # O_{21}
  2  2     3.52726281E-02   # O_{22}
Block SBOTMIX   # sbottom mixing matrix
  1  1     1.33988157E-01   # O_{11}
  1  2     9.90982950E-01   # O_{12}
  2  1    -9.90982950E-01   # O_{21}
  2  2     1.33988157E-01   # O_{22}
Block STAUMIX   # stau mixing matrix
  1  1     3.01738027E-02   # O_{11}
  1  2     9.99544680E-01   # O_{12}
  2  1    -9.99544680E-01   # O_{21}
  2  2     3.01738027E-02   # O_{22}
Block NMIX   # neutralino mixing matrix
  1  1     9.98486757E-01   #
  1  2    -5.03810775E-03   #
  1  3     4.99034375E-02   #
  1  4    -2.25488320E-02   #
  2  1     1.74927488E-02   #
  2  2     9.71300423E-01   #
  2  3    -1.84668168E-01   #
  2  4     1.48885846E-01   #
  3  1     1.91765428E-02   #
  3  2    -2.61292439E-02   #
  3  3    -7.06001401E-01   #
  3  4    -7.07468569E-01   #
  4  1     4.84813154E-02   #
  4  2    -2.36362427E-01   #
  4  3    -6.81886911E-01   #
  4  4     6.90516412E-01   #
Block UMIX   # chargino U mixing matrix
  1  1    -9.66983736E-01   # U_{11}
  1  2     2.54838169E-01   # U_{12}
  2  1    -2.54838169E-01   # U_{21}
  2  2    -9.66983736E-01   # U_{22}
Block VMIX   # chargino V mixing matrix
  1  1    -9.78801012E-01   # V_{11}
  1  2     2.04813465E-01   # V_{12}
  2  1    -2.04813465E-01   # V_{21}
  2  2    -9.78801012E-01   # V_{22}
Block GAUGE Q=  2.74450342E+03   #
     1     3.57524991E-01   # g`
     2     6.52378619E-01   # g_2
     3     1.21928000E+00   # g_3
Block YU Q=  2.74450342E+03   #
  3  3     8.36716533E-01   # y_t
Block YD Q=  2.74450342E+03   #
  3  3     1.85510069E-01   # y_b
Block YE Q=  2.74450342E+03   #
  3  3     1.52883455E-01   # y_tau
Block HMIX Q=  2.74450342E+03   # Higgs mixing parameters
     1     1.04073755E+03   # mu(Q)
     2     1.43309488E+01   # tan(beta)(Q)
     3     2.51744278E+02   # Higgs vev at Q
     4     2.11804450E+06   # m_A^2(Q)
Block MSOFT Q=  2.74450342E+03   # DRbar SUSY breaking parameters
     1     4.39109375E+02   # M_1(Q)          
     2     8.03122803E+02   # M_2(Q)          
     3     2.05296167E+03   # M_3(Q)          
    21     9.80736938E+05   # MHd^2(Q)        
    22    -9.52130375E+05   # MHu^2(Q)        
    31     1.04595557E+03   # MeL(Q)          
    32     1.04595557E+03   # MmuL(Q)         
    33     1.04024353E+03   # MtauL(Q)        
    34     5.20835388E+02   # MeR(Q)          
    35     5.20835388E+02   # MmuR(Q)         
    36     5.13172119E+02   # MtauR(Q)        
    41     2.99528491E+03   # MqL1(Q)         
    42     2.99528491E+03   # MqL2(Q)         
    43     2.88543506E+03   # MqL3(Q)         
    44     2.84571924E+03   # MuR(Q)          
    45     2.84571924E+03   # McR(Q)          
    46     2.61045532E+03   # MtR(Q)          
    47     2.82972583E+03   # MdR(Q)          
    48     2.82972583E+03   # MsR(Q)          
    49     2.81962012E+03   # MbR(Q)          
Block AU Q=  2.74450342E+03   #
  1  1    -6.04030090E+02   # A_u
  2  2    -6.04030090E+02   # A_c
  3  3    -6.04030090E+02   # A_t
Block AD Q=  2.74450342E+03   #
  1  1    -6.72904663E+02   # A_d
  2  2    -6.72904663E+02   # A_s
  3  3    -6.72904663E+02   # A_b
Block AE Q=  2.74450342E+03   #
  1  1    -7.91490021E+01   # A_e
  2  2    -7.91490021E+01   # A_mu
  3  3    -7.91490021E+01   # A_tau
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
DECAY   1000021  5.71590774E-02   # GLSS  decays
#          BR          NDA       ID1       ID2       ID3       ID4
      5.21740429E-02    3      1000024         1        -2             # GLSS   -->  W1SS+  DN     UB          
      5.21740429E-02    3     -1000024         2        -1             # GLSS   -->  W1SS-  UP     DB          
      5.21740355E-02    3      1000024         3        -4             # GLSS   -->  W1SS+  ST     CB          
      5.21740355E-02    3     -1000024         4        -3             # GLSS   -->  W1SS-  CH     SB          
      5.52133545E-02    3      1000024         5        -6             # GLSS   -->  W1SS+  BT     TB          
      5.52133545E-02    3     -1000024         6        -5             # GLSS   -->  W1SS-  TP     BB          
      1.69150706E-03    3      1000037         1        -2             # GLSS   -->  W2SS+  DN     UB          
      1.69150706E-03    3     -1000037         2        -1             # GLSS   -->  W2SS-  UP     DB          
      1.69150671E-03    3      1000037         3        -4             # GLSS   -->  W2SS+  ST     CB          
      1.69150671E-03    3     -1000037         4        -3             # GLSS   -->  W2SS-  CH     SB          
      9.58305895E-02    3      1000037         5        -6             # GLSS   -->  W2SS+  BT     TB          
      9.58305895E-02    3     -1000037         6        -5             # GLSS   -->  W2SS-  TP     BB          
      5.16256841E-06    2      1000022        21                       # GLSS   -->  Z1SS   GL                 
      3.31376009E-02    3      1000022         2        -2             # GLSS   -->  Z1SS   UP     UB          
      9.76133067E-03    3      1000022         1        -1             # GLSS   -->  Z1SS   DN     DB          
      9.76133067E-03    3      1000022         3        -3             # GLSS   -->  Z1SS   ST     SB          
      3.31375860E-02    3      1000022         4        -4             # GLSS   -->  Z1SS   CH     CB          
      1.04202526E-02    3      1000022         5        -5             # GLSS   -->  Z1SS   BT     BB          
      4.86235954E-02    3      1000022         6        -6             # GLSS   -->  Z1SS   TP     TB          
      1.77759488E-04    2      1000023        21                       # GLSS   -->  Z2SS   GL                 
      2.62239855E-02    3      1000023         2        -2             # GLSS   -->  Z2SS   UP     UB          
      2.58264039E-02    3      1000023         1        -1             # GLSS   -->  Z2SS   DN     DB          
      2.58264039E-02    3      1000023         3        -3             # GLSS   -->  Z2SS   ST     SB          
      2.62239762E-02    3      1000023         4        -4             # GLSS   -->  Z2SS   CH     CB          
      3.28489691E-02    3      1000023         5        -5             # GLSS   -->  Z2SS   BT     BB          
      2.29215138E-02    3      1000023         6        -6             # GLSS   -->  Z2SS   TP     TB          
      2.36667530E-03    2      1000025        21                       # GLSS   -->  Z3SS   GL                 
      5.49822289E-06    3      1000025         2        -2             # GLSS   -->  Z3SS   UP     UB          
      6.65889092E-06    3      1000025         1        -1             # GLSS   -->  Z3SS   DN     DB          
      6.65889092E-06    3      1000025         3        -3             # GLSS   -->  Z3SS   ST     SB          
      5.49822107E-06    3      1000025         4        -4             # GLSS   -->  Z3SS   CH     CB          
      4.01079701E-03    3      1000025         5        -5             # GLSS   -->  Z3SS   BT     BB          
      7.30174482E-02    3      1000025         6        -6             # GLSS   -->  Z3SS   TP     TB          
      2.22639157E-03    2      1000035        21                       # GLSS   -->  Z4SS   GL                 
      8.53760459E-04    3      1000035         2        -2             # GLSS   -->  Z4SS   UP     UB          
      9.65886924E-04    3      1000035         1        -1             # GLSS   -->  Z4SS   DN     DB          
      9.65886924E-04    3      1000035         3        -3             # GLSS   -->  Z4SS   ST     SB          
      8.53759877E-04    3      1000035         4        -4             # GLSS   -->  Z4SS   CH     CB          
      4.83832881E-03    3      1000035         5        -5             # GLSS   -->  Z4SS   BT     BB          
      8.74309912E-02    3      1000035         6        -6             # GLSS   -->  Z4SS   TP     TB          
      1.76392180E-12    2      1000039        21                       # GLSS   -->  GVSS   GL                 
#         PDG         Width
DECAY   1000002  7.12206268E+01   # UPL   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      5.56830410E-03    2      1000022         2                       # UPL    -->  Z1SS   UP                 
      1.50145978E-01    2      1000023         2                       # UPL    -->  Z2SS   UP                 
      7.31547916E-05    2      1000025         2                       # UPL    -->  Z3SS   UP                 
      7.35111395E-03    2      1000035         2                       # UPL    -->  Z4SS   UP                 
      5.22048056E-01    2      1000021         2                       # UPL    -->  GLSS   UP                 
      3.02898407E-01    2      1000024         1                       # UPL    -->  W1SS+  DN                 
      1.19148549E-02    2      1000037         1                       # UPL    -->  W2SS+  DN                 
#         PDG         Width
DECAY   1000001  7.12462616E+01   # DNL   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      6.21699542E-03    2      1000022         1                       # DNL    -->  Z1SS   DN                 
      1.48189068E-01    2      1000023         1                       # DNL    -->  Z2SS   DN                 
      1.25692488E-04    2      1000025         1                       # DNL    -->  Z3SS   DN                 
      8.54541454E-03    2      1000035         1                       # DNL    -->  Z4SS   DN                 
      5.22821903E-01    2      1000021         1                       # DNL    -->  GLSS   DN                 
      2.95652062E-01    2     -1000024         2                       # DNL    -->  W1SS-  UP                 
      1.84488669E-02    2     -1000037         2                       # DNL    -->  W2SS-  UP                 
#         PDG         Width
DECAY   1000003  7.12462540E+01   # STL   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      6.21699588E-03    2      1000022         3                       # STL    -->  Z1SS   ST                 
      1.48189098E-01    2      1000023         3                       # STL    -->  Z2SS   ST                 
      1.25692502E-04    2      1000025         3                       # STL    -->  Z3SS   ST                 
      8.54541548E-03    2      1000035         3                       # STL    -->  Z4SS   ST                 
      5.22821963E-01    2      1000021         3                       # STL    -->  GLSS   ST                 
      2.95651972E-01    2     -1000024         4                       # STL    -->  W1SS-  CH                 
      1.84488613E-02    2     -1000037         4                       # STL    -->  W2SS-  CH                 
#         PDG         Width
DECAY   1000004  7.12205658E+01   # CHL   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      5.56830736E-03    2      1000022         4                       # CHL    -->  Z1SS   CH                 
      1.50146097E-01    2      1000023         4                       # CHL    -->  Z2SS   CH                 
      7.31548280E-05    2      1000025         4                       # CHL    -->  Z3SS   CH                 
      7.35111767E-03    2      1000035         4                       # CHL    -->  Z4SS   CH                 
      5.22047698E-01    2      1000021         4                       # CHL    -->  GLSS   CH                 
      3.02898765E-01    2      1000024         3                       # CHL    -->  W1SS+  ST                 
      1.19148670E-02    2      1000037         3                       # CHL    -->  W2SS+  ST                 
#         PDG         Width
DECAY   1000005  3.17001801E+01   # BT1   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      5.02609164E-02    2      1000022         5                       # BT1    -->  Z1SS   BT                 
      1.38338571E-02    2      1000023         5                       # BT1    -->  Z2SS   BT                 
      2.40229927E-02    2      1000025         5                       # BT1    -->  Z3SS   BT                 
      1.76709406E-02    2      1000035         5                       # BT1    -->  Z4SS   BT                 
      8.18909109E-01    2      1000021         5                       # BT1    -->  GLSS   BT                 
      2.73416098E-02    2     -1000024         6                       # BT1    -->  W1SS-  TP                 
      4.79211435E-02    2     -1000037         6                       # BT1    -->  W2SS-  TP                 
      3.94078561E-05    2          -24   1000006                       # BT1    -->  W-     TP1                
#         PDG         Width
DECAY   1000006  6.73550415E+01   # TP1   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      1.66940197E-01    2      1000021         6                       # TP1    -->  GLSS   TP                 
      8.44082534E-02    2      1000022         6                       # TP1    -->  Z1SS   TP                 
      1.21216178E-02    2      1000023         6                       # TP1    -->  Z2SS   TP                 
      1.87668234E-01    2      1000025         6                       # TP1    -->  Z3SS   TP                 
      1.73057526E-01    2      1000035         6                       # TP1    -->  Z4SS   TP                 
      2.33180802E-02    2      1000024         5                       # TP1    -->  W1SS+  BT                 
      3.52486074E-01    2      1000037         5                       # TP1    -->  W2SS+  BT                 
#         PDG         Width
DECAY   2000002  3.41262741E+01   # UPR   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      1.86503589E-01    2      1000022         2                       # UPR    -->  Z1SS   UP                 
      5.08443118E-05    2      1000023         2                       # UPR    -->  Z2SS   UP                 
      5.46464798E-05    2      1000025         2                       # UPR    -->  Z3SS   UP                 
      3.46638524E-04    2      1000035         2                       # UPR    -->  Z4SS   UP                 
      8.13044250E-01    2      1000021         2                       # UPR    -->  GLSS   UP                 
#         PDG         Width
DECAY   2000001  2.83769627E+01   # DNR   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      5.57534024E-02    2      1000022         1                       # DNR    -->  Z1SS   DN                 
      1.51793956E-05    2      1000023         1                       # DNR    -->  Z2SS   DN                 
      1.62930701E-05    2      1000025         1                       # DNR    -->  Z3SS   DN                 
      1.03342172E-04    2      1000035         1                       # DNR    -->  Z4SS   DN                 
      9.44111824E-01    2      1000021         1                       # DNR    -->  GLSS   DN                 
#         PDG         Width
DECAY   2000003  2.83769627E+01   # STR   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      5.57534024E-02    2      1000022         3                       # STR    -->  Z1SS   ST                 
      1.51793956E-05    2      1000023         3                       # STR    -->  Z2SS   ST                 
      1.62930701E-05    2      1000025         3                       # STR    -->  Z3SS   ST                 
      1.03342172E-04    2      1000035         3                       # STR    -->  Z4SS   ST                 
      9.44111824E-01    2      1000021         3                       # STR    -->  GLSS   ST                 
#         PDG         Width
DECAY   2000004  3.41262207E+01   # CHR   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      1.86503798E-01    2      1000022         4                       # CHR    -->  Z1SS   CH                 
      5.08443700E-05    2      1000023         4                       # CHR    -->  Z2SS   CH                 
      5.46465453E-05    2      1000025         4                       # CHR    -->  Z3SS   CH                 
      3.46638903E-04    2      1000035         4                       # CHR    -->  Z4SS   CH                 
      8.13044071E-01    2      1000021         4                       # CHR    -->  GLSS   CH                 
#         PDG         Width
DECAY   2000005  9.25828094E+01   # BT2   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      4.47979150E-03    2      1000022         5                       # BT2    -->  Z1SS   BT                 
      1.04816586E-01    2      1000023         5                       # BT2    -->  Z2SS   BT                 
      9.15781315E-03    2      1000025         5                       # BT2    -->  Z3SS   BT                 
      1.62506253E-02    2      1000035         5                       # BT2    -->  Z4SS   BT                 
      3.19720715E-01    2      1000021         5                       # BT2    -->  GLSS   BT                 
      2.17313960E-01    2     -1000024         6                       # BT2    -->  W1SS-  TP                 
      3.26780885E-01    2     -1000037         6                       # BT2    -->  W2SS-  TP                 
      1.47966889E-03    2          -24   1000006                       # BT2    -->  W-     TP1                
#         PDG         Width
DECAY   2000006  9.42061920E+01   # TP2   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      3.11333627E-01    2      1000021         6                       # TP2    -->  GLSS   TP                 
      2.16450304E-01    2      1000024         5                       # TP2    -->  W1SS+  BT                 
      2.88134944E-02    2      1000037         5                       # TP2    -->  W2SS+  BT                 
      8.78345571E-04    2           23   1000006                       # TP2    -->  Z0     TP1                
      2.96648918E-03    2           25   1000006                       # TP2    -->  HL0    TP1                
      4.13414231E-03    2      1000022         6                       # TP2    -->  Z1SS   TP                 
      1.11768000E-01    2      1000023         6                       # TP2    -->  Z2SS   TP                 
      1.60578758E-01    2      1000025         6                       # TP2    -->  Z3SS   TP                 
      1.63076922E-01    2      1000035         6                       # TP2    -->  Z4SS   TP                 
#         PDG         Width
DECAY   1000011  2.85789704E+00   # EL-   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      3.19219112E-01    2      1000022        11                       # EL-    -->  Z1SS   E-                 
      2.31756195E-01    2      1000023        11                       # EL-    -->  Z2SS   E-                 
      2.24728325E-09    2      1000025        11                       # EL-    -->  Z3SS   E-                 
      4.49023962E-01    2     -1000024        12                       # EL-    -->  W1SS-  NUE                
      3.19504892E-07    3      1000015        11       -15             # EL-    -->  TAU1-  E-     TAU+        
      4.54994648E-07    3     -1000015        11        15             # EL-    -->  TAU1+  E-     TAU-        
      7.82631126E-16    2           11   1000039                       # EL-    -->  E-     GVSS               
#         PDG         Width
DECAY   1000013  2.85789704E+00   # MUL-  decays
#          BR          NDA       ID1       ID2       ID3       ID4
      3.19219112E-01    2      1000022        13                       # MUL-   -->  Z1SS   MU-                
      2.31756195E-01    2      1000023        13                       # MUL-   -->  Z2SS   MU-                
      2.23978969E-09    2      1000025        13                       # MUL-   -->  Z3SS   MU-                
      4.49023962E-01    2     -1000024        14                       # MUL-   -->  W1SS-  NUM                
      3.19504892E-07    3      1000015        13       -15             # MUL-   -->  TAU1-  MU-    TAU+        
      4.54994648E-07    3     -1000015        13        15             # MUL-   -->  TAU1+  MU-    TAU-        
      7.82631126E-16    2           13   1000039                       # MUL-   -->  MU-    GVSS               
#         PDG         Width
DECAY   1000015  2.77327836E-01   # TAU1- decays
#          BR          NDA       ID1       ID2       ID3       ID4
      1.00000000E+00    2      1000022        15                       # TAU1-  -->  Z1SS   TAU-               
      2.41993316E-16    2           15   1000039                       # TAU1-  -->  TAU-   GVSS               
#         PDG         Width
DECAY   1000012  2.76632929E+00   # NUEL  decays
#          BR          NDA       ID1       ID2       ID3       ID4
      3.36922705E-01    2      1000022        12                       # NUEL   -->  Z1SS   NUE                
      2.16336161E-01    2      1000023        12                       # NUEL   -->  Z2SS   NUE                
      4.46737885E-01    2      1000024        11                       # NUEL   -->  W1SS+  E-                 
      5.73750356E-07    3      1000015        12       -15             # NUEL   -->  TAU1-  NUE    TAU+        
      4.47537303E-07    3     -1000015        12        15             # NUEL   -->  TAU1+  NUE    TAU-        
      2.27530336E-06    3     -1000015        11        16             # NUEL   -->  TAU1+  E-     NUT         
      7.74994701E-16    2           12   1000039                       # NUEL   -->  NUE    GVSS               
#         PDG         Width
DECAY   1000014  2.76632905E+00   # NUML  decays
#          BR          NDA       ID1       ID2       ID3       ID4
      3.36922735E-01    2      1000022        14                       # NUML   -->  Z1SS   NUM                
      2.16336176E-01    2      1000023        14                       # NUML   -->  Z2SS   NUM                
      4.46737856E-01    2      1000024        13                       # NUML   -->  W1SS+  MU-                
      5.73750413E-07    3      1000015        14       -15             # NUML   -->  TAU1-  NUM    TAU+        
      4.47537360E-07    3     -1000015        14        15             # NUML   -->  TAU1+  NUM    TAU-        
      2.27927580E-06    3     -1000015        13        16             # NUML   -->  TAU1+  MU-    NUT         
      7.74994807E-16    2           14   1000039                       # NUML   -->  NUM    GVSS               
#         PDG         Width
DECAY   1000016  2.95153761E+00   # NUTL  decays
#          BR          NDA       ID1       ID2       ID3       ID4
      3.12579751E-01    2      1000022        16                       # NUTL   -->  Z1SS   NUT                
      1.94394514E-01    2      1000023        16                       # NUTL   -->  Z2SS   NUT                
      4.02581275E-01    2      1000024        15                       # NUTL   -->  W1SS+  TAU-               
      9.04424489E-02    2           24   1000015                       # NUTL   -->  W+     TAU1-              
      1.51673032E-06    3     -1000015        16        15             # NUTL   -->  TAU1+  NUT    TAU-        
      3.82441641E-07    3      1000015        16       -15             # NUTL   -->  TAU1-  NUT    TAU+        
      7.06320975E-16    2           16   1000039                       # NUTL   -->  NUT    GVSS               
#         PDG         Width
DECAY   2000011  2.69772321E-01   # ER-   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      1.00000000E+00    2      1000022        11                       # ER-    -->  Z1SS   E-                 
      2.45144067E-16    2           11   1000039                       # ER-    -->  E-     GVSS               
#         PDG         Width
DECAY   2000013  2.69772112E-01   # MUR-  decays
#          BR          NDA       ID1       ID2       ID3       ID4
      1.00000000E+00    2      1000022        13                       # MUR-   -->  Z1SS   MU-                
      2.45144252E-16    2           13   1000039                       # MUR-   -->  MU-    GVSS               
#         PDG         Width
DECAY   2000015  3.09001446E+00   # TAU2- decays
#          BR          NDA       ID1       ID2       ID3       ID4
      2.93093234E-01    2      1000022        15                       # TAU2-  -->  Z1SS   TAU-               
      2.06391022E-01    2      1000023        15                       # TAU2-  -->  Z2SS   TAU-               
      3.98562700E-01    2     -1000024        16                       # TAU2-  -->  W1SS-  NUT                
      4.52981256E-02    2           23   1000015                       # TAU2-  -->  Z0     TAU1-              
      5.66548854E-02    2           25   1000015                       # TAU2-  -->  HL0    TAU1-              
#         PDG         Width
DECAY   1000022  2.46492891E-17   # Z1SS  decays
#          BR          NDA       ID1       ID2       ID3       ID4
      7.80865729E-01    2      1000039        22                       # Z1SS   -->  GVSS   GM                 
      1.76629294E-02    3      1000039        11       -11             # Z1SS   -->  GVSS   E-     E+          
      2.01334491E-01    2      1000039        23                       # Z1SS   -->  GVSS   Z0                 
      1.36852352E-04    2      1000039        25                       # Z1SS   -->  GVSS   HL0                
#         PDG         Width
DECAY   1000023  4.99238446E-02   # Z2SS  decays
#          BR          NDA       ID1       ID2       ID3       ID4
      2.19435606E-06    2      1000022        22                       # Z2SS   -->  Z1SS   GM                 
      5.13119772E-02    2      1000022        23                       # Z2SS   -->  Z1SS   Z0                 
      7.31253579E-07    3      1000022         2        -2             # Z2SS   -->  Z1SS   UP     UB          
      8.01891133E-07    3      1000022         1        -1             # Z2SS   -->  Z1SS   DN     DB          
      8.01891133E-07    3      1000022         3        -3             # Z2SS   -->  Z1SS   ST     SB          
      7.31253124E-07    3      1000022         4        -4             # Z2SS   -->  Z1SS   CH     CB          
      2.46355239E-06    3      1000022         5        -5             # Z2SS   -->  Z1SS   BT     BB          
      4.01117781E-04    3      1000022        11       -11             # Z2SS   -->  Z1SS   E-     E+          
      4.01117781E-04    3      1000022        13       -13             # Z2SS   -->  Z1SS   MU-    MU+         
      4.13314701E-04    3      1000022        15       -15             # Z2SS   -->  Z1SS   TAU-   TAU+        
      4.23015590E-04    3      1000022        12       -12             # Z2SS   -->  Z1SS   NUE    ANUE        
      4.23015590E-04    3      1000022        14       -14             # Z2SS   -->  Z1SS   NUM    ANUM        
      4.39134921E-04    3      1000022        16       -16             # Z2SS   -->  Z1SS   NUT    ANUT        
      7.12738454E-01    2      1000022        25                       # Z2SS   -->  Z1SS   HL0                
      4.55883378E-03    2      2000011       -11                       # Z2SS   -->  ER-    E+                 
      4.55883378E-03    2     -2000011        11                       # Z2SS   -->  ER+    E-                 
      4.55883332E-03    2      2000013       -13                       # Z2SS   -->  MUR-   MU+                
      4.55883332E-03    2     -2000013        13                       # Z2SS   -->  MUR+   MU-                
      1.07602961E-01    2      1000015       -15                       # Z2SS   -->  TAU1-  TAU+               
      1.07602961E-01    2     -1000015        15                       # Z2SS   -->  TAU1+  TAU-               
      2.99361349E-15    2      1000039        22                       # Z2SS   -->  GVSS   GM                 
      7.09224657E-17    3      1000039        11       -11             # Z2SS   -->  GVSS   E-     E+          
      8.82532013E-15    2      1000039        23                       # Z2SS   -->  GVSS   Z0                 
      1.09193922E-16    2      1000039        25                       # Z2SS   -->  GVSS   HL0                
#         PDG         Width
DECAY   1000025  3.85491228E+00   # Z3SS  decays
#          BR          NDA       ID1       ID2       ID3       ID4
      9.87010367E-08    2      1000022        22                       # Z3SS   -->  Z1SS   GM                 
      2.37921284E-07    2      1000023        22                       # Z3SS   -->  Z2SS   GM                 
      2.64207363E-01    2      1000024       -24                       # Z3SS   -->  W1SS+  W-                 
      2.64207363E-01    2     -1000024        24                       # Z3SS   -->  W1SS-  W+                 
      1.47592753E-01    2      1000022        23                       # Z3SS   -->  Z1SS   Z0                 
      2.61895150E-01    2      1000023        23                       # Z3SS   -->  Z2SS   Z0                 
      1.51130344E-10    3      1000022         2        -2             # Z3SS   -->  Z1SS   UP     UB          
      4.05469443E-11    3      1000022         1        -1             # Z3SS   -->  Z1SS   DN     DB          
      4.05469443E-11    3      1000022         3        -3             # Z3SS   -->  Z1SS   ST     SB          
      1.51130289E-10    3      1000022         4        -4             # Z3SS   -->  Z1SS   CH     CB          
      1.13756653E-06    3      1000022         5        -5             # Z3SS   -->  Z1SS   BT     BB          
      2.18004121E-08    3      1000022        11       -11             # Z3SS   -->  Z1SS   E-     E+          
      2.18004121E-08    3      1000022        13       -13             # Z3SS   -->  Z1SS   MU-    MU+         
      2.02450309E-07    3      1000022        15       -15             # Z3SS   -->  Z1SS   TAU-   TAU+        
      7.21519545E-12    3      1000023         2        -2             # Z3SS   -->  Z2SS   UP     UB          
      1.22133857E-11    3      1000023         1        -1             # Z3SS   -->  Z2SS   DN     DB          
      1.22133857E-11    3      1000023         3        -3             # Z3SS   -->  Z2SS   ST     SB          
      7.21519285E-12    3      1000023         4        -4             # Z3SS   -->  Z2SS   CH     CB          
      4.57161029E-08    3      1000023         5        -5             # Z3SS   -->  Z2SS   BT     BB          
      5.34895817E-09    3      1000023        11       -11             # Z3SS   -->  Z2SS   E-     E+          
      5.34895817E-09    3      1000023        13       -13             # Z3SS   -->  Z2SS   MU-    MU+         
      7.61349916E-09    3      1000023        15       -15             # Z3SS   -->  Z2SS   TAU-   TAU+        
      2.11383123E-02    2      1000022        25                       # Z3SS   -->  Z1SS   HL0                
      2.83373240E-03    2      1000023        25                       # Z3SS   -->  Z2SS   HL0                
      1.45668004E-04    2      2000011       -11                       # Z3SS   -->  ER-    E+                 
      1.45668004E-04    2     -2000011        11                       # Z3SS   -->  ER+    E-                 
      1.45668004E-04    2      2000013       -13                       # Z3SS   -->  MUR-   MU+                
      1.45668004E-04    2     -2000013        13                       # Z3SS   -->  MUR+   MU-                
      1.87678169E-02    2      1000015       -15                       # Z3SS   -->  TAU1-  TAU+               
      1.87678169E-02    2     -1000015        15                       # Z3SS   -->  TAU1+  TAU-               
      1.86017178E-06    2      2000015       -15                       # Z3SS   -->  TAU2-  TAU+               
      1.86017178E-06    2     -2000015        15                       # Z3SS   -->  TAU2+  TAU-               
      1.60423482E-07    2      1000012       -12                       # Z3SS   -->  NUEL   ANUE               
      1.60423482E-07    2     -1000012        12                       # Z3SS   -->  ANUEL  NUE                
      1.60423482E-07    2      1000014       -14                       # Z3SS   -->  NUML   ANUM               
      1.60423482E-07    2     -1000014        14                       # Z3SS   -->  ANUML  NUM                
      4.97914243E-07    2      1000016       -16                       # Z3SS   -->  NUTL   ANUT               
      4.97914243E-07    2     -1000016        16                       # Z3SS   -->  ANUTL  NUT                
      1.02696694E-20    2      1000039        22                       # Z3SS   -->  GVSS   GM                 
      2.47540387E-22    3      1000039        11       -11             # Z3SS   -->  GVSS   E-     E+          
      1.22068843E-16    2      1000039        23                       # Z3SS   -->  GVSS   Z0                 
      1.55459596E-16    2      1000039        25                       # Z3SS   -->  GVSS   HL0                
#         PDG         Width
DECAY   1000035  4.50388145E+00   # Z4SS  decays
#          BR          NDA       ID1       ID2       ID3       ID4
      3.71623550E-08    2      1000022        22                       # Z4SS   -->  Z1SS   GM                 
      6.58787869E-08    2      1000023        22                       # Z4SS   -->  Z2SS   GM                 
      3.53536922E-10    2      1000025        22                       # Z4SS   -->  Z3SS   GM                 
      2.85611838E-01    2      1000024       -24                       # Z4SS   -->  W1SS+  W-                 
      2.85611838E-01    2     -1000024        24                       # Z4SS   -->  W1SS-  W+                 
      1.82368867E-02    2      1000022        23                       # Z4SS   -->  Z1SS   Z0                 
      3.79769108E-03    2      1000023        23                       # Z4SS   -->  Z2SS   Z0                 
      5.81389870E-09    3      1000022         2        -2             # Z4SS   -->  Z1SS   UP     UB          
      5.11945020E-09    3      1000022         1        -1             # Z4SS   -->  Z1SS   DN     DB          
      5.11945020E-09    3      1000022         3        -3             # Z4SS   -->  Z1SS   ST     SB          
      5.81389736E-09    3      1000022         4        -4             # Z4SS   -->  Z1SS   CH     CB          
      9.27930159E-07    3      1000022         5        -5             # Z4SS   -->  Z1SS   BT     BB          
      1.64139152E-07    3      1000022        15       -15             # Z4SS   -->  Z1SS   TAU-   TAU+        
      2.40243203E-09    3      1000023         2        -2             # Z4SS   -->  Z2SS   UP     UB          
      2.75158296E-09    3      1000023         1        -1             # Z4SS   -->  Z2SS   DN     DB          
      2.75158296E-09    3      1000023         3        -3             # Z4SS   -->  Z2SS   ST     SB          
      2.40243136E-09    3      1000023         4        -4             # Z4SS   -->  Z2SS   CH     CB          
      4.39978507E-08    3      1000023         5        -5             # Z4SS   -->  Z2SS   BT     BB          
      6.76169032E-09    3      1000023        15       -15             # Z4SS   -->  Z2SS   TAU-   TAU+        
      1.19642687E-08    3      1000025         2        -2             # Z4SS   -->  Z3SS   UP     UB          
      1.54283537E-08    3      1000025         1        -1             # Z4SS   -->  Z3SS   DN     DB          
      1.54283537E-08    3      1000025         3        -3             # Z4SS   -->  Z3SS   ST     SB          
      1.19642687E-08    3      1000025         4        -4             # Z4SS   -->  Z3SS   CH     CB          
      2.58783661E-09    3      1000025         5        -5             # Z4SS   -->  Z3SS   BT     BB          
      3.49986995E-09    3      1000025        11       -11             # Z4SS   -->  Z3SS   E-     E+          
      3.49986995E-09    3      1000025        13       -13             # Z4SS   -->  Z3SS   MU-    MU+         
      2.80951151E-09    3      1000025        15       -15             # Z4SS   -->  Z3SS   TAU-   TAU+        
      6.96364033E-09    3      1000025        12       -12             # Z4SS   -->  Z3SS   NUE    ANUE        
      6.96364033E-09    3      1000025        14       -14             # Z4SS   -->  Z3SS   NUM    ANUM        
      6.96364033E-09    3      1000025        16       -16             # Z4SS   -->  Z3SS   NUT    ANUT        
      1.28136724E-01    2      1000022        25                       # Z4SS   -->  Z1SS   HL0                
      2.44419828E-01    2      1000023        25                       # Z4SS   -->  Z2SS   HL0                
      1.14149416E-05    2      1000011       -11                       # Z4SS   -->  EL-    E+                 
      1.14149416E-05    2     -1000011        11                       # Z4SS   -->  EL+    E-                 
      1.14145269E-05    2      1000013       -13                       # Z4SS   -->  MUL-   MU+                
      1.14145269E-05    2     -1000013        13                       # Z4SS   -->  MUL+   MU-                
      8.20387620E-04    2      2000011       -11                       # Z4SS   -->  ER-    E+                 
      8.20387620E-04    2     -2000011        11                       # Z4SS   -->  ER+    E-                 
      8.20387620E-04    2      2000013       -13                       # Z4SS   -->  MUR-   MU+                
      8.20387620E-04    2     -2000013        13                       # Z4SS   -->  MUR+   MU-                
      1.51881007E-02    2      1000015       -15                       # Z4SS   -->  TAU1-  TAU+               
      1.51881007E-02    2     -1000015        15                       # Z4SS   -->  TAU1+  TAU-               
      4.78583315E-05    2      2000015       -15                       # Z4SS   -->  TAU2-  TAU+               
      4.78583315E-05    2     -2000015        15                       # Z4SS   -->  TAU2+  TAU-               
      5.31049373E-05    2      1000012       -12                       # Z4SS   -->  NUEL   ANUE               
      5.31049373E-05    2     -1000012        12                       # Z4SS   -->  ANUEL  NUE                
      5.31049373E-05    2      1000014       -14                       # Z4SS   -->  NUML   ANUM               
      5.31049373E-05    2     -1000014        14                       # Z4SS   -->  ANUML  NUM                
      8.59974680E-05    2      1000016       -16                       # Z4SS   -->  NUTL   ANUT               
      8.59974680E-05    2     -1000016        16                       # Z4SS   -->  ANUTL  NUT                
      2.67908146E-18    2      1000039        22                       # Z4SS   -->  GVSS   GM                 
      6.46333275E-20    3      1000039        11       -11             # Z4SS   -->  GVSS   E-     E+          
      1.64901274E-16    2      1000039        23                       # Z4SS   -->  GVSS   Z0                 
      1.03507989E-16    2      1000039        25                       # Z4SS   -->  GVSS   HL0                
#         PDG         Width
DECAY   1000024  4.41587307E-02   # W1SS+ decays
#          BR          NDA       ID1       ID2       ID3       ID4
      1.74320053E-06    3      1000022         2        -1             # W1SS+  -->  Z1SS   UP     DB          
      1.74320019E-06    3      1000022         4        -3             # W1SS+  -->  Z1SS   CH     SB          
      9.41047270E-04    3      1000022       -11        12             # W1SS+  -->  Z1SS   E+     NUE         
      9.41047270E-04    3      1000022       -13        14             # W1SS+  -->  Z1SS   MU+    NUM         
      9.73634713E-04    3      1000022       -15        16             # W1SS+  -->  Z1SS   TAU+   NUT         
      7.70857930E-01    2      1000022        24                       # W1SS+  -->  Z1SS   W+                 
      2.79819733E-13    3      1000023       -11        12             # W1SS+  -->  Z2SS   E+     NUE         
      2.79819733E-13    3      1000023       -13        14             # W1SS+  -->  Z2SS   MU+    NUM         
      2.26282910E-01    2     -1000015        16                       # W1SS+  -->  TAU1+  NUT                
#         PDG         Width
DECAY   1000037  4.07450581E+00   # W2SS+ decays
#          BR          NDA       ID1       ID2       ID3       ID4
      9.35153910E-09    3      1000022         2        -1             # W2SS+  -->  Z1SS   UP     DB          
      9.35153732E-09    3      1000022         4        -3             # W2SS+  -->  Z1SS   CH     SB          
      1.75370317E-07    3      1000022       -15        16             # W2SS+  -->  Z1SS   TAU+   NUT         
      1.45316586E-01    2      1000022        24                       # W2SS+  -->  Z1SS   W+                 
      1.91295579E-09    3      1000023         2        -1             # W2SS+  -->  Z2SS   UP     DB          
      1.91295557E-09    3      1000023         4        -3             # W2SS+  -->  Z2SS   CH     SB          
      1.05761169E-08    3      1000023       -15        16             # W2SS+  -->  Z2SS   TAU+   NUT         
      2.68850327E-01    2      1000023        24                       # W2SS+  -->  Z2SS   W+                 
      5.57163240E-08    3      1000025         2        -1             # W2SS+  -->  Z3SS   UP     DB          
      5.57163240E-08    3      1000025         4        -3             # W2SS+  -->  Z3SS   CH     SB          
      1.85720630E-08    3      1000025       -11        12             # W2SS+  -->  Z3SS   E+     NUE         
      1.85720630E-08    3      1000025       -13        14             # W2SS+  -->  Z3SS   MU+    NUM         
      1.85720630E-08    3      1000025       -15        16             # W2SS+  -->  Z3SS   TAU+   NUT         
      3.39105045E-15    3      1000035       -11        12             # W2SS+  -->  Z4SS   E+     NUE         
      3.39105045E-15    3      1000035       -13        14             # W2SS+  -->  Z4SS   MU+    NUM         
      7.49608298E-05    2      1000012       -11                       # W2SS+  -->  NUEL   E+                 
      7.49599640E-05    2      1000014       -13                       # W2SS+  -->  NUML   MU+                
      2.56119936E-04    2      1000016       -15                       # W2SS+  -->  NUTL   TAU+               
      4.07283660E-05    2     -1000011        12                       # W2SS+  -->  EL+    NUE                
      4.07283660E-05    2     -1000013        14                       # W2SS+  -->  MUL+   NUM                
      3.21037285E-02    2     -1000015        16                       # W2SS+  -->  TAU1+  NUT                
      8.52364319E-05    2     -2000015        16                       # W2SS+  -->  TAU2+  NUT                
      2.71845132E-01    2      1000024        23                       # W2SS+  -->  W1SS+  Z0                 
      2.92100255E-09    3      1000024         1        -1             # W2SS+  -->  W1SS+  DN     DB          
      2.92100144E-09    3      1000024         3        -3             # W2SS+  -->  W1SS+  ST     SB          
      4.40704317E-09    3      1000024         2        -2             # W2SS+  -->  W1SS+  UP     UB          
      4.40704317E-09    3      1000024         4        -4             # W2SS+  -->  W1SS+  CH     CB          
      2.81311125E-01    2      1000024        25                       # W2SS+  -->  W1SS+  HL0                
#         PDG         Width
DECAY        25  3.23155220E-03   # HL0   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      6.37715525E-09    2           11       -11                       # HL0    -->  E-     E+                 
      2.69253738E-04    2           13       -13                       # HL0    -->  MU-    MU+                
      7.70153254E-02    2           15       -15                       # HL0    -->  TAU-   TAU+               
      8.00006273E-06    2            1        -1                       # HL0    -->  DN     DB                 
      3.23242624E-03    2            3        -3                       # HL0    -->  ST     SB                 
      6.92184567E-01    2            5        -5                       # HL0    -->  BT     BB                 
      2.50821472E-06    2            2        -2                       # HL0    -->  UP     UB                 
      4.69801426E-02    2            4        -4                       # HL0    -->  CH     CB                 
      2.62318249E-03    2           22        22                       # HL0    -->  GM     GM                 
      5.48412949E-02    2           21        21                       # HL0    -->  GL     GL                 
      6.26152987E-03    3           24        11       -12             # HL0    -->  W+     E-     ANUE        
      6.26152987E-03    3           24        13       -14             # HL0    -->  W+     MU-    ANUM        
      6.26152987E-03    3           24        15       -16             # HL0    -->  W+     TAU-   ANUT        
      1.87845882E-02    3           24        -2         1             # HL0    -->  W+     UB     DN          
      1.87845882E-02    3           24        -4         3             # HL0    -->  W+     CB     ST          
      6.26152987E-03    3          -24       -11        12             # HL0    -->  W-     E+     NUE         
      6.26152987E-03    3          -24       -13        14             # HL0    -->  W-     MU+    NUM         
      6.26152987E-03    3          -24       -15        16             # HL0    -->  W-     TAU+   NUT         
      1.87845882E-02    3          -24         2        -1             # HL0    -->  W-     UP     DB          
      1.87845882E-02    3          -24         4        -3             # HL0    -->  W-     CH     SB          
      6.93201146E-04    3           23        12       -12             # HL0    -->  Z0     NUE    ANUE        
      6.93201146E-04    3           23        14       -14             # HL0    -->  Z0     NUM    ANUM        
      6.93201146E-04    3           23        16       -16             # HL0    -->  Z0     NUT    ANUT        
      3.48881178E-04    3           23        11       -11             # HL0    -->  Z0     E-     E+          
      3.48881178E-04    3           23        13       -13             # HL0    -->  Z0     MU-    MU+         
      3.48881178E-04    3           23        15       -15             # HL0    -->  Z0     TAU-   TAU+        
      1.19523727E-03    3           23         2        -2             # HL0    -->  Z0     UP     UB          
      1.19523727E-03    3           23         4        -4             # HL0    -->  Z0     CH     CB          
      1.53976004E-03    3           23         1        -1             # HL0    -->  Z0     DN     DB          
      1.53976004E-03    3           23         3        -3             # HL0    -->  Z0     ST     SB          
      1.53976004E-03    3           23         5        -5             # HL0    -->  Z0     BT     BB          
#         PDG         Width
DECAY        35  4.99426079E+00   # HH0   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      1.12149454E-08    2           11       -11                       # HH0    -->  E-     E+                 
      4.73515218E-04    2           13       -13                       # HH0    -->  MU-    MU+                
      1.35620832E-01    2           15       -15                       # HH0    -->  TAU-   TAU+               
      1.36971985E-05    2            1        -1                       # HH0    -->  DN     DB                 
      5.53436158E-03    2            3        -3                       # HH0    -->  ST     SB                 
      7.94179201E-01    2            5        -5                       # HH0    -->  BT     BB                 
      8.83502785E-11    2            2        -2                       # HH0    -->  UP     UB                 
      1.19135780E-06    2            4        -4                       # HH0    -->  CH     CB                 
      5.93337379E-02    2            6        -6                       # HH0    -->  TP     TB                 
      1.36583935E-07    2           22        22                       # HH0    -->  GM     GM                 
      1.85972840E-05    2           21        21                       # HH0    -->  GL     GL                 
      9.26100984E-05    2           24       -24                       # HH0    -->  W+     W-                 
      4.72911961E-05    2           23        23                       # HH0    -->  Z0     Z0                 
      9.35202290E-04    2      1000022   1000022                       # HH0    -->  Z1SS   Z1SS               
      3.40623013E-03    2      1000022   1000023                       # HH0    -->  Z1SS   Z2SS               
      3.20555584E-04    2           25        25                       # HH0    -->  HL0    HL0                
      8.48559557E-06    2      2000011  -2000011                       # HH0    -->  ER-    ER+                
      8.47483443E-06    2      2000013  -2000013                       # HH0    -->  MUR-   MUR+               
      6.06012782E-06    2      1000015  -1000015                       # HH0    -->  TAU1-  TAU1+              
#         PDG         Width
DECAY        36  5.02350760E+00   # HA0   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      1.10773470E-08    2           11       -11                       # HA0    -->  E-     E+                 
      4.67705569E-04    2           13       -13                       # HA0    -->  MU-    MU+                
      1.33957639E-01    2           15       -15                       # HA0    -->  TAU-   TAU+               
      1.35298324E-05    2            1        -1                       # HA0    -->  DN     DB                 
      5.46673732E-03    2            3        -3                       # HA0    -->  ST     SB                 
      7.84490883E-01    2            5        -5                       # HA0    -->  BT     BB                 
      8.55134158E-11    2            2        -2                       # HA0    -->  UP     UB                 
      1.15397017E-06    2            4        -4                       # HA0    -->  CH     CB                 
      6.01902716E-02    2            6        -6                       # HA0    -->  TP     TB                 
      4.52201959E-07    2           22        22                       # HA0    -->  GM     GM                 
      5.92119286E-05    2           21        21                       # HA0    -->  GL     GL                 
      1.58698275E-03    2      1000022   1000022                       # HA0    -->  Z1SS   Z1SS               
      1.36752231E-02    2      1000022   1000023                       # HA0    -->  Z1SS   Z2SS               
      9.01845415E-05    2           25        23                       # HA0    -->  HL0    Z0                 
#         PDG         Width
DECAY        37  4.62536287E+00   # H+    decays
#          BR          NDA       ID1       ID2       ID3       ID4
      1.21284982E-08    2           12       -11                       # H+     -->  NUE    E+                 
      5.12087077E-04    2           14       -13                       # H+     -->  NUM    MU+                
      1.46669164E-01    2           16       -15                       # H+     -->  NUT    TAU+               
      1.36571362E-05    2            2        -1                       # H+     -->  UP     DB                 
      5.51929325E-03    2            4        -3                       # H+     -->  CH     SB                 
      8.29154849E-01    2            6        -5                       # H+     -->  TP     BB                 
      1.80316325E-02    2      1000024   1000022                       # H+     -->  W1SS+  Z1SS               
      9.93565409E-05    2           25        24                       # H+     -->  HL0    W+                 
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

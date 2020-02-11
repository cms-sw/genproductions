
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
     1     4.50000000E+05   # Lambda scale of soft SSB
     2     9.00000000E+05   # M_mess overall messenger scale
     3     1.50000000E+01   # tan(beta)
     4     1.00000000E+00   # sign(mu)
     5     1.00000000E+00   # N_5 messenger index
     6     9.21830994E+02   # c_grav gravitino mass factor
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
        25     1.20551857E+02   #  h^0            
        35     2.12422729E+03   #  H^0            
        36     2.11030371E+03   #  A^0            
        37     2.12573218E+03   #  H^+            
   1000001     4.51387402E+03   #  dnl            
   1000002     4.51315869E+03   #  upl            
   1000003     4.51387402E+03   #  stl            
   1000004     4.51315869E+03   #  chl            
   1000005     4.24461133E+03   #  b1             
   1000006     3.88973877E+03   #  t1             
   1000011     1.57330334E+03   #  el-            
   1000012     1.56308667E+03   #  nuel           
   1000013     1.57330334E+03   #  mul-           
   1000014     1.56308667E+03   #  numl           
   1000015     7.88488892E+02   #  tau1           
   1000016     1.55446924E+03   #  nutl           
   1000021     3.26672119E+03   #  glss           
   1000022     6.49607178E+02   #  z1ss           
   1000023     1.23292749E+03   #  z2ss           
   1000024     1.23316174E+03   #  w1ss           
   1000025    -1.47747803E+03   #  z3ss           
   1000035     1.48980066E+03   #  z4ss           
   1000037     1.49091223E+03   #  w2ss           
   1000039     8.98120197E-05   #  gvss
   2000001     4.26391797E+03   #  dnr            
   2000002     4.28836816E+03   #  upr            
   2000003     4.26391797E+03   #  str            
   2000004     4.28836816E+03   #  chr            
   2000005     4.34371777E+03   #  b2             
   2000006     4.37019678E+03   #  t2             
   2000011     7.86332153E+02   #  er-            
   2000013     7.86332153E+02   #  mur-           
   2000015     1.56533508E+03   #  tau2           
Block ALPHA   # Effective Higgs mixing parameter
         -6.68973103E-02   # alpha
Block STOPMIX   # stop mixing matrix
  1  1     2.27555223E-02   # O_{11}
  1  2     9.99741077E-01   # O_{12}
  2  1    -9.99741077E-01   # O_{21}
  2  2     2.27555223E-02   # O_{22}
Block SBOTMIX   # sbottom mixing matrix
  1  1     8.59610587E-02   # O_{11}
  1  2     9.96298492E-01   # O_{12}
  2  1    -9.96298492E-01   # O_{21}
  2  2     8.59610587E-02   # O_{22}
Block STAUMIX   # stau mixing matrix
  1  1     1.81620419E-02   # O_{11}
  1  2     9.99835074E-01   # O_{12}
  2  1    -9.99835074E-01   # O_{21}
  2  2     1.81620419E-02   # O_{22}
Block NMIX   # neutralino mixing matrix
  1  1     9.99189913E-01   #
  1  2    -2.43789563E-03   #
  1  3     3.62787545E-02   #
  1  4    -1.72411501E-02   #
  2  1    -1.09320292E-02   #
  2  2    -9.75383639E-01   #
  2  3     1.67687014E-01   #
  2  4    -1.42787591E-01   #
  3  1     1.34054422E-02   #
  3  2    -1.80797763E-02   #
  3  3    -7.06560373E-01   #
  3  4    -7.07294643E-01   #
  4  1    -3.63336578E-02   #
  4  2     2.19758555E-01   #
  4  3     6.86540365E-01   #
  4  4    -6.92133248E-01   #
Block UMIX   # chargino U mixing matrix
  1  1    -9.73248541E-01   # U_{11}
  1  2     2.29754820E-01   # U_{12}
  2  1    -2.29754820E-01   # U_{21}
  2  2    -9.73248541E-01   # U_{22}
Block VMIX   # chargino V mixing matrix
  1  1    -9.80782092E-01   # V_{11}
  1  2     1.95106342E-01   # V_{12}
  2  1    -1.95106342E-01   # V_{21}
  2  2    -9.80782092E-01   # V_{22}
Block GAUGE Q=  3.99986890E+03   #
     1     3.57524991E-01   # g`
     2     6.52378619E-01   # g_2
     3     1.21928000E+00   # g_3
Block YU Q=  3.99986890E+03   #
  3  3     8.26207161E-01   # y_t
Block YD Q=  3.99986890E+03   #
  3  3     1.80895194E-01   # y_b
Block YE Q=  3.99986890E+03   #
  3  3     1.53140575E-01   # y_tau
Block HMIX Q=  3.99986890E+03   # Higgs mixing parameters
     1     1.45950818E+03   # mu(Q)
     2     1.42720318E+01   # tan(beta)(Q)
     3     2.52109192E+02   # Higgs vev at Q
     4     4.45338200E+06   # m_A^2(Q)
Block MSOFT Q=  3.99986890E+03   # DRbar SUSY breaking parameters
     1     6.61775879E+02   # M_1(Q)          
     2     1.19799866E+03   # M_2(Q)          
     3     2.97602686E+03   # M_3(Q)          
    21     2.20187325E+06   # MHd^2(Q)        
    22    -1.85443900E+06   # MHu^2(Q)        
    31     1.56049634E+03   # MeL(Q)          
    32     1.56049634E+03   # MmuL(Q)         
    33     1.55207776E+03   # MtauL(Q)        
    34     7.86154358E+02   # MeR(Q)          
    35     7.86154358E+02   # MmuR(Q)         
    36     7.74571350E+02   # MtauR(Q)        
    41     4.36809131E+03   # MqL1(Q)         
    42     4.36809131E+03   # MqL2(Q)         
    43     4.20987500E+03   # MqL3(Q)         
    44     4.14014160E+03   # MuR(Q)          
    45     4.14014160E+03   # McR(Q)          
    46     3.80033887E+03   # MtR(Q)          
    47     4.11510986E+03   # MdR(Q)          
    48     4.11510986E+03   # MsR(Q)          
    49     4.10062988E+03   # MbR(Q)          
Block AU Q=  3.99986890E+03   #
  1  1    -8.62451172E+02   # A_u
  2  2    -8.62451172E+02   # A_c
  3  3    -8.62451172E+02   # A_t
Block AD Q=  3.99986890E+03   #
  1  1    -9.58785889E+02   # A_d
  2  2    -9.58785889E+02   # A_s
  3  3    -9.58785889E+02   # A_b
Block AE Q=  3.99986890E+03   #
  1  1    -1.18693092E+02   # A_e
  2  2    -1.18693092E+02   # A_mu
  3  3    -1.18693092E+02   # A_tau
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
DECAY   1000021  7.99780861E-02   # GLSS  decays
#          BR          NDA       ID1       ID2       ID3       ID4
      4.72715348E-02    3      1000024         1        -2             # GLSS   -->  W1SS+  DN     UB          
      4.72715348E-02    3     -1000024         2        -1             # GLSS   -->  W1SS-  UP     DB          
      4.72715348E-02    3      1000024         3        -4             # GLSS   -->  W1SS+  ST     CB          
      4.72715348E-02    3     -1000024         4        -3             # GLSS   -->  W1SS-  CH     SB          
      5.42371161E-02    3      1000024         5        -6             # GLSS   -->  W1SS+  BT     TB          
      5.42371161E-02    3     -1000024         6        -5             # GLSS   -->  W1SS-  TP     BB          
      1.50775688E-03    3      1000037         1        -2             # GLSS   -->  W2SS+  DN     UB          
      1.50775688E-03    3     -1000037         2        -1             # GLSS   -->  W2SS-  UP     DB          
      1.50775688E-03    3      1000037         3        -4             # GLSS   -->  W2SS+  ST     CB          
      1.50775688E-03    3     -1000037         4        -3             # GLSS   -->  W2SS-  CH     SB          
      1.02795750E-01    3      1000037         5        -6             # GLSS   -->  W2SS+  BT     TB          
      1.02795750E-01    3     -1000037         6        -5             # GLSS   -->  W2SS-  TP     BB          
      1.14910990E-05    2      1000022        21                       # GLSS   -->  Z1SS   GL                 
      3.16029377E-02    3      1000022         2        -2             # GLSS   -->  Z1SS   UP     UB          
      9.26471874E-03    3      1000022         1        -1             # GLSS   -->  Z1SS   DN     DB          
      9.26471874E-03    3      1000022         3        -3             # GLSS   -->  Z1SS   ST     SB          
      3.16029377E-02    3      1000022         4        -4             # GLSS   -->  Z1SS   CH     CB          
      9.83522926E-03    3      1000022         5        -5             # GLSS   -->  Z1SS   BT     BB          
      5.09000644E-02    3      1000022         6        -6             # GLSS   -->  Z1SS   TP     TB          
      9.82445272E-05    2      1000023        21                       # GLSS   -->  Z2SS   GL                 
      2.36736257E-02    3      1000023         2        -2             # GLSS   -->  Z2SS   UP     UB          
      2.34574024E-02    3      1000023         1        -1             # GLSS   -->  Z2SS   DN     DB          
      2.34574024E-02    3      1000023         3        -3             # GLSS   -->  Z2SS   ST     SB          
      2.36736257E-02    3      1000023         4        -4             # GLSS   -->  Z2SS   CH     CB          
      2.96215620E-02    3      1000023         5        -5             # GLSS   -->  Z2SS   BT     BB          
      2.48120874E-02    3      1000023         6        -6             # GLSS   -->  Z2SS   TP     TB          
      1.50513893E-03    2      1000025        21                       # GLSS   -->  Z3SS   GL                 
      2.76464721E-06    3      1000025         2        -2             # GLSS   -->  Z3SS   UP     UB          
      3.33884441E-06    3      1000025         1        -1             # GLSS   -->  Z3SS   DN     DB          
      3.33884441E-06    3      1000025         3        -3             # GLSS   -->  Z3SS   ST     SB          
      2.76464721E-06    3      1000025         4        -4             # GLSS   -->  Z3SS   CH     CB          
      3.97400465E-03    3      1000025         5        -5             # GLSS   -->  Z3SS   BT     BB          
      8.83785486E-02    3      1000025         6        -6             # GLSS   -->  Z3SS   TP     TB          
      1.43218471E-03    2      1000035        21                       # GLSS   -->  Z4SS   GL                 
      7.74438493E-04    3      1000035         2        -2             # GLSS   -->  Z4SS   UP     UB          
      8.59454332E-04    3      1000035         1        -1             # GLSS   -->  Z4SS   DN     DB          
      8.59454332E-04    3      1000035         3        -3             # GLSS   -->  Z4SS   ST     SB          
      7.74438493E-04    3      1000035         4        -4             # GLSS   -->  Z4SS   CH     CB          
      4.75393143E-03    3      1000035         5        -5             # GLSS   -->  Z4SS   BT     BB          
      9.62170735E-02    3      1000035         6        -6             # GLSS   -->  Z4SS   TP     TB          
      6.63906321E-13    2      1000039        21                       # GLSS   -->  GVSS   GL                 
#         PDG         Width
DECAY   1000002  1.03665054E+02   # UPL   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      5.73338615E-03    2      1000022         2                       # UPL    -->  Z1SS   UP                 
      1.49907604E-01    2      1000023         2                       # UPL    -->  Z2SS   UP                 
      3.56590426E-05    2      1000025         2                       # UPL    -->  Z3SS   UP                 
      6.60728244E-03    2      1000035         2                       # UPL    -->  Z4SS   UP                 
      5.24759591E-01    2      1000021         2                       # UPL    -->  GLSS   UP                 
      3.01883578E-01    2      1000024         1                       # UPL    -->  W1SS+  DN                 
      1.10729188E-02    2      1000037         1                       # UPL    -->  W2SS+  DN                 
#         PDG         Width
DECAY   1000001  1.03682556E+02   # DNL   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      6.04712451E-03    2      1000022         1                       # DNL    -->  Z1SS   DN                 
      1.48687482E-01    2      1000023         1                       # DNL    -->  Z2SS   DN                 
      6.16092002E-05    2      1000025         1                       # DNL    -->  Z3SS   DN                 
      7.45912595E-03    2      1000035         1                       # DNL    -->  Z4SS   DN                 
      5.25112748E-01    2      1000021         1                       # DNL    -->  GLSS   DN                 
      2.97275901E-01    2     -1000024         2                       # DNL    -->  W1SS-  UP                 
      1.53559893E-02    2     -1000037         2                       # DNL    -->  W2SS-  UP                 
#         PDG         Width
DECAY   1000003  1.03682549E+02   # STL   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      6.04712497E-03    2      1000022         3                       # STL    -->  Z1SS   ST                 
      1.48687497E-01    2      1000023         3                       # STL    -->  Z2SS   ST                 
      6.16092075E-05    2      1000025         3                       # STL    -->  Z3SS   ST                 
      7.45912641E-03    2      1000035         3                       # STL    -->  Z4SS   ST                 
      5.25112808E-01    2      1000021         3                       # STL    -->  GLSS   ST                 
      2.97275841E-01    2     -1000024         4                       # STL    -->  W1SS-  CH                 
      1.53559865E-02    2     -1000037         4                       # STL    -->  W2SS-  CH                 
#         PDG         Width
DECAY   1000004  1.03665001E+02   # CHL   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      5.73338801E-03    2      1000022         4                       # CHL    -->  Z1SS   CH                 
      1.49907634E-01    2      1000023         4                       # CHL    -->  Z2SS   CH                 
      3.56590499E-05    2      1000025         4                       # CHL    -->  Z3SS   CH                 
      6.60728384E-03    2      1000035         4                       # CHL    -->  Z4SS   CH                 
      5.24759412E-01    2      1000021         4                       # CHL    -->  GLSS   CH                 
      3.01883757E-01    2      1000024         3                       # CHL    -->  W1SS+  ST                 
      1.10729244E-02    2      1000037         3                       # CHL    -->  W2SS+  ST                 
#         PDG         Width
DECAY   1000005  4.51121483E+01   # BT1   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      5.09510525E-02    2      1000022         5                       # BT1    -->  Z1SS   BT                 
      7.56776705E-03    2      1000023         5                       # BT1    -->  Z2SS   BT                 
      2.44032871E-02    2      1000025         5                       # BT1    -->  Z3SS   BT                 
      1.99898053E-02    2      1000035         5                       # BT1    -->  Z4SS   BT                 
      8.36858988E-01    2      1000021         5                       # BT1    -->  GLSS   BT                 
      1.48335407E-02    2     -1000024         6                       # BT1    -->  W1SS-  TP                 
      4.53794859E-02    2     -1000037         6                       # BT1    -->  W2SS-  TP                 
      1.60397121E-05    2          -24   1000006                       # BT1    -->  W-     TP1                
#         PDG         Width
DECAY   1000006  9.90844727E+01   # TP1   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      1.77284166E-01    2      1000021         6                       # TP1    -->  GLSS   TP                 
      8.35026801E-02    2      1000022         6                       # TP1    -->  Z1SS   TP                 
      9.85625014E-03    2      1000023         6                       # TP1    -->  Z2SS   TP                 
      1.85061902E-01    2      1000025         6                       # TP1    -->  Z3SS   TP                 
      1.74101427E-01    2      1000035         6                       # TP1    -->  Z4SS   TP                 
      1.85970347E-02    2      1000024         5                       # TP1    -->  W1SS+  BT                 
      3.51596534E-01    2      1000037         5                       # TP1    -->  W2SS+  BT                 
#         PDG         Width
DECAY   2000002  4.96095047E+01   # UPR   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      1.86241403E-01    2      1000022         2                       # UPR    -->  Z1SS   UP                 
      1.96519904E-05    2      1000023         2                       # UPR    -->  Z2SS   UP                 
      2.72741527E-05    2      1000025         2                       # UPR    -->  Z3SS   UP                 
      1.99455244E-04    2      1000035         2                       # UPR    -->  Z4SS   UP                 
      8.13512206E-01    2      1000021         2                       # UPR    -->  GLSS   UP                 
#         PDG         Width
DECAY   2000001  4.11801147E+01   # DNR   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      5.57411201E-02    2      1000022         1                       # DNR    -->  Z1SS   DN                 
      5.87272962E-06    2      1000023         1                       # DNR    -->  Z2SS   DN                 
      8.14214400E-06    2      1000025         1                       # DNR    -->  Z3SS   DN                 
      5.95397869E-05    2      1000035         1                       # DNR    -->  Z4SS   DN                 
      9.44185317E-01    2      1000021         1                       # DNR    -->  GLSS   DN                 
#         PDG         Width
DECAY   2000003  4.11801147E+01   # STR   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      5.57411201E-02    2      1000022         3                       # STR    -->  Z1SS   ST                 
      5.87272962E-06    2      1000023         3                       # STR    -->  Z2SS   ST                 
      8.14214400E-06    2      1000025         3                       # STR    -->  Z3SS   ST                 
      5.95397869E-05    2      1000035         3                       # STR    -->  Z4SS   ST                 
      9.44185317E-01    2      1000021         3                       # STR    -->  GLSS   ST                 
#         PDG         Width
DECAY   2000004  4.96094589E+01   # CHR   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      1.86241508E-01    2      1000022         4                       # CHR    -->  Z1SS   CH                 
      1.96520014E-05    2      1000023         4                       # CHR    -->  Z2SS   CH                 
      2.72741709E-05    2      1000025         4                       # CHR    -->  Z3SS   CH                 
      1.99455375E-04    2      1000035         4                       # CHR    -->  Z4SS   CH                 
      8.13512087E-01    2      1000021         4                       # CHR    -->  GLSS   CH                 
#         PDG         Width
DECAY   2000005  1.35947479E+02   # BT2   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      4.36799042E-03    2      1000022         5                       # BT2    -->  Z1SS   BT                 
      1.06159963E-01    2      1000023         5                       # BT2    -->  Z2SS   BT                 
      8.68899468E-03    2      1000025         5                       # BT2    -->  Z3SS   BT                 
      1.45361200E-02    2      1000035         5                       # BT2    -->  Z4SS   BT                 
      3.21526438E-01    2      1000021         5                       # BT2    -->  GLSS   BT                 
      2.21035212E-01    2     -1000024         6                       # BT2    -->  W1SS-  TP                 
      3.22182804E-01    2     -1000037         6                       # BT2    -->  W2SS-  TP                 
      1.49440428E-03    2          -24   1000006                       # BT2    -->  W-     TP1                
      8.06686694E-06    2           23   1000005                       # BT2    -->  Z0     BT1                
#         PDG         Width
DECAY   2000006  1.38441833E+02   # TP2   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      3.20512503E-01    2      1000021         6                       # TP2    -->  GLSS   TP                 
      2.14958563E-01    2      1000024         5                       # TP2    -->  W1SS+  BT                 
      2.62430087E-02    2      1000037         5                       # TP2    -->  W2SS+  BT                 
      8.73419107E-04    2           23   1000006                       # TP2    -->  Z0     TP1                
      2.98486976E-03    2           25   1000006                       # TP2    -->  HL0    TP1                
      2.38634137E-04    2           24   1000005                       # TP2    -->  W+     BT1                
      4.19178884E-03    2      1000022         6                       # TP2    -->  Z1SS   TP                 
      1.11521773E-01    2      1000023         6                       # TP2    -->  Z2SS   TP                 
      1.58879921E-01    2      1000025         6                       # TP2    -->  Z3SS   TP                 
      1.59595504E-01    2      1000035         6                       # TP2    -->  Z4SS   TP                 
#         PDG         Width
DECAY   1000011  4.18798304E+00   # EL-   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      3.25259805E-01    2      1000022        11                       # EL-    -->  Z1SS   E-                 
      2.26787537E-01    2      1000023        11                       # EL-    -->  Z2SS   E-                 
      2.53094663E-06    2      1000025        11                       # EL-    -->  Z3SS   E-                 
      6.73964212E-04    2      1000035        11                       # EL-    -->  Z4SS   E-                 
      4.45539504E-01    2     -1000024        12                       # EL-    -->  W1SS-  NUE                
      1.73671672E-03    2     -1000037        12                       # EL-    -->  W2SS-  NUE                
      3.28529961E-16    2           11   1000039                       # EL-    -->  E-     GVSS               
#         PDG         Width
DECAY   1000013  4.18798304E+00   # MUL-  decays
#          BR          NDA       ID1       ID2       ID3       ID4
      3.25259805E-01    2      1000022        13                       # MUL-   -->  Z1SS   MU-                
      2.26787537E-01    2      1000023        13                       # MUL-   -->  Z2SS   MU-                
      2.53094527E-06    2      1000025        13                       # MUL-   -->  Z3SS   MU-                
      6.73963630E-04    2      1000035        13                       # MUL-   -->  Z4SS   MU-                
      4.45539504E-01    2     -1000024        14                       # MUL-   -->  W1SS-  NUM                
      1.73671672E-03    2     -1000037        14                       # MUL-   -->  W2SS-  NUM                
      3.28529961E-16    2           13   1000039                       # MUL-   -->  MU-    GVSS               
#         PDG         Width
DECAY   1000015  4.13186073E-01   # TAU1- decays
#          BR          NDA       ID1       ID2       ID3       ID4
      1.00000000E+00    2      1000022        15                       # TAU1-  -->  Z1SS   TAU-               
      1.05278784E-16    2           15   1000039                       # TAU1-  -->  TAU-   GVSS               
#         PDG         Width
DECAY   1000012  4.06484079E+00   # NUEL  decays
#          BR          NDA       ID1       ID2       ID3       ID4
      3.37077528E-01    2      1000022        12                       # NUEL   -->  Z1SS   NUE                
      2.17134401E-01    2      1000023        12                       # NUEL   -->  Z2SS   NUE                
      1.18976241E-05    2      1000025        12                       # NUEL   -->  Z3SS   NUE                
      7.80048664E-04    2      1000035        12                       # NUEL   -->  Z4SS   NUE                
      4.43993092E-01    2      1000024        11                       # NUEL   -->  W1SS+  E-                 
      1.00299099E-03    2      1000037        11                       # NUEL   -->  W2SS+  E-                 
      3.27634276E-16    2           12   1000039                       # NUEL   -->  NUE    GVSS               
#         PDG         Width
DECAY   1000014  4.06484032E+00   # NUML  decays
#          BR          NDA       ID1       ID2       ID3       ID4
      3.37077588E-01    2      1000022        14                       # NUML   -->  Z1SS   NUM                
      2.17134416E-01    2      1000023        14                       # NUML   -->  Z2SS   NUM                
      1.18976259E-05    2      1000025        14                       # NUML   -->  Z3SS   NUM                
      7.80048780E-04    2      1000035        14                       # NUML   -->  Z4SS   NUM                
      4.43993121E-01    2      1000024        13                       # NUML   -->  W1SS+  MU-                
      1.00299006E-03    2      1000037        13                       # NUML   -->  W2SS+  MU-                
      3.27634329E-16    2           14   1000039                       # NUML   -->  NUM    GVSS               
#         PDG         Width
DECAY   1000016  4.27125216E+00   # NUTL  decays
#          BR          NDA       ID1       ID2       ID3       ID4
      3.17540169E-01    2      1000022        16                       # NUTL   -->  Z1SS   NUT                
      1.98046148E-01    2      1000023        16                       # NUTL   -->  Z2SS   NUT                
      9.25833319E-06    2      1000025        16                       # NUTL   -->  Z3SS   NUT                
      5.84388734E-04    2      1000035        16                       # NUTL   -->  Z4SS   NUT                
      4.06006783E-01    2      1000024        15                       # NUTL   -->  W1SS+  TAU-               
      1.87920011E-03    2      1000037        15                       # NUTL   -->  W2SS+  TAU-               
      7.59340525E-02    2           24   1000015                       # NUTL   -->  W+     TAU1-              
      3.03300343E-16    2           16   1000039                       # NUTL   -->  NUT    GVSS               
#         PDG         Width
DECAY   2000011  4.02572930E-01   # ER-   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      1.00000000E+00    2      1000022        11                       # ER-    -->  Z1SS   E-                 
      1.06586728E-16    2           11   1000039                       # ER-    -->  E-     GVSS               
#         PDG         Width
DECAY   2000013  4.02572751E-01   # MUR-  decays
#          BR          NDA       ID1       ID2       ID3       ID4
      1.00000000E+00    2      1000022        13                       # MUR-   -->  Z1SS   MU-                
      1.06586775E-16    2           13   1000039                       # MUR-   -->  MU-    GVSS               
#         PDG         Width
DECAY   2000015  4.47728872E+00   # TAU2- decays
#          BR          NDA       ID1       ID2       ID3       ID4
      3.01628828E-01    2      1000022        15                       # TAU2-  -->  Z1SS   TAU-               
      2.04340905E-01    2      1000023        15                       # TAU2-  -->  Z2SS   TAU-               
      1.01986108E-03    2      1000025        15                       # TAU2-  -->  Z3SS   TAU-               
      1.29104592E-03    2      1000035        15                       # TAU2-  -->  Z4SS   TAU-               
      4.00304049E-01    2     -1000024        16                       # TAU2-  -->  W1SS-  NUT                
      1.38850359E-03    2     -1000037        16                       # TAU2-  -->  W2SS-  NUT                
      3.78251188E-02    2           23   1000015                       # TAU2-  -->  Z0     TAU1-              
      5.22017032E-02    2           25   1000015                       # TAU2-  -->  HL0    TAU1-              
#         PDG         Width
DECAY   1000022  1.64878719E-17   # Z1SS  decays
#          BR          NDA       ID1       ID2       ID3       ID4
      7.65770733E-01    2      1000039        22                       # Z1SS   -->  GVSS   GM                 
      1.78440828E-02    3      1000039        11       -11             # Z1SS   -->  GVSS   E-     E+          
      2.16290191E-01    2      1000039        23                       # Z1SS   -->  GVSS   Z0                 
      9.50375470E-05    2      1000039        25                       # Z1SS   -->  GVSS   HL0                
#         PDG         Width
DECAY   1000023  6.00569062E-02   # Z2SS  decays
#          BR          NDA       ID1       ID2       ID3       ID4
      1.70643932E-06    2      1000022        22                       # Z2SS   -->  Z1SS   GM                 
      5.25196157E-02    2      1000022        23                       # Z2SS   -->  Z1SS   Z0                 
      1.04116225E-06    3      1000022         2        -2             # Z2SS   -->  Z1SS   UP     UB          
      1.08703125E-06    3      1000022         1        -1             # Z2SS   -->  Z1SS   DN     DB          
      1.08703125E-06    3      1000022         3        -3             # Z2SS   -->  Z1SS   ST     SB          
      1.04116225E-06    3      1000022         4        -4             # Z2SS   -->  Z1SS   CH     CB          
      2.73403498E-06    3      1000022         5        -5             # Z2SS   -->  Z1SS   BT     BB          
      5.17731882E-04    3      1000022        11       -11             # Z2SS   -->  Z1SS   E-     E+          
      5.17731882E-04    3      1000022        13       -13             # Z2SS   -->  Z1SS   MU-    MU+         
      5.35082538E-04    3      1000022        15       -15             # Z2SS   -->  Z1SS   TAU-   TAU+        
      5.37041633E-04    3      1000022        12       -12             # Z2SS   -->  Z1SS   NUE    ANUE        
      5.37041633E-04    3      1000022        14       -14             # Z2SS   -->  Z1SS   NUM    ANUM        
      5.57430263E-04    3      1000022        16       -16             # Z2SS   -->  Z1SS   NUT    ANUT        
      7.62359262E-01    2      1000022        25                       # Z2SS   -->  Z1SS   HL0                
      2.19587493E-03    2      2000011       -11                       # Z2SS   -->  ER-    E+                 
      2.19587493E-03    2     -2000011        11                       # Z2SS   -->  ER+    E-                 
      2.19587493E-03    2      2000013       -13                       # Z2SS   -->  MUR-   MU+                
      2.19587493E-03    2     -2000013        13                       # Z2SS   -->  MUR+   MU-                
      8.65633786E-02    2      1000015       -15                       # Z2SS   -->  TAU1-  TAU+               
      8.65633786E-02    2     -1000015        15                       # Z2SS   -->  TAU1+  TAU-               
      1.55602049E-15    2      1000039        22                       # Z2SS   -->  GVSS   GM                 
      3.79115801E-17    3      1000039        11       -11             # Z2SS   -->  GVSS   E-     E+          
      4.85854456E-15    2      1000039        23                       # Z2SS   -->  GVSS   Z0                 
      5.61281816E-17    2      1000039        25                       # Z2SS   -->  GVSS   HL0                
#         PDG         Width
DECAY   1000025  4.48118067E+00   # Z3SS  decays
#          BR          NDA       ID1       ID2       ID3       ID4
      1.20245375E-07    2      1000022        22                       # Z3SS   -->  Z1SS   GM                 
      1.64645684E-07    2      1000023        22                       # Z3SS   -->  Z2SS   GM                 
      2.51424730E-01    2      1000024       -24                       # Z3SS   -->  W1SS+  W-                 
      2.51424730E-01    2     -1000024        24                       # Z3SS   -->  W1SS-  W+                 
      1.75496504E-01    2      1000022        23                       # Z3SS   -->  Z1SS   Z0                 
      2.56087393E-01    2      1000023        23                       # Z3SS   -->  Z2SS   Z0                 
      6.17107893E-11    3      1000022         2        -2             # Z3SS   -->  Z1SS   UP     UB          
      1.58798981E-11    3      1000022         1        -1             # Z3SS   -->  Z1SS   DN     DB          
      1.58798981E-11    3      1000022         3        -3             # Z3SS   -->  Z1SS   ST     SB          
      6.17107893E-11    3      1000022         4        -4             # Z3SS   -->  Z1SS   CH     CB          
      9.08044626E-07    3      1000022         5        -5             # Z3SS   -->  Z1SS   BT     BB          
      3.62118113E-09    3      1000022        11       -11             # Z3SS   -->  Z1SS   E-     E+          
      3.62118113E-09    3      1000022        13       -13             # Z3SS   -->  Z1SS   MU-    MU+         
      2.32369666E-06    3      1000022        15       -15             # Z3SS   -->  Z1SS   TAU-   TAU+        
      2.25626788E-08    3      1000022        12       -12             # Z3SS   -->  Z1SS   NUE    ANUE        
      2.25626788E-08    3      1000022        14       -14             # Z3SS   -->  Z1SS   NUM    ANUM        
      2.42467877E-08    3      1000022        16       -16             # Z3SS   -->  Z1SS   NUT    ANUT        
      9.27839836E-13    3      1000023         2        -2             # Z3SS   -->  Z2SS   UP     UB          
      1.58868014E-12    3      1000023         1        -1             # Z3SS   -->  Z2SS   DN     DB          
      1.58868014E-12    3      1000023         3        -3             # Z3SS   -->  Z2SS   ST     SB          
      9.27839836E-13    3      1000023         4        -4             # Z3SS   -->  Z2SS   CH     CB          
      1.21817427E-08    3      1000023         5        -5             # Z3SS   -->  Z2SS   BT     BB          
      1.59171759E-10    3      1000023        11       -11             # Z3SS   -->  Z2SS   E-     E+          
      1.59171759E-10    3      1000023        13       -13             # Z3SS   -->  Z2SS   MU-    MU+         
      1.79342464E-07    3      1000023        15       -15             # Z3SS   -->  Z2SS   TAU-   TAU+        
      9.98943928E-10    3      1000023        12       -12             # Z3SS   -->  Z2SS   NUE    ANUE        
      9.98943928E-10    3      1000023        14       -14             # Z3SS   -->  Z2SS   NUM    ANUM        
      1.12417764E-09    3      1000023        16       -16             # Z3SS   -->  Z2SS   NUT    ANUT        
      2.24796031E-02    2      1000022        25                       # Z3SS   -->  Z1SS   HL0                
      1.56658690E-03    2      1000023        25                       # Z3SS   -->  Z2SS   HL0                
      7.74096261E-05    2      2000011       -11                       # Z3SS   -->  ER-    E+                 
      7.74096261E-05    2     -2000011        11                       # Z3SS   -->  ER+    E-                 
      7.74096261E-05    2      2000013       -13                       # Z3SS   -->  MUR-   MU+                
      7.74096261E-05    2     -2000013        13                       # Z3SS   -->  MUR+   MU-                
      2.06036493E-02    2      1000015       -15                       # Z3SS   -->  TAU1-  TAU+               
      2.06036493E-02    2     -1000015        15                       # Z3SS   -->  TAU1+  TAU-               
      2.07182975E-21    2      1000039        22                       # Z3SS   -->  GVSS   GM                 
      5.11004941E-23    3      1000039        11       -11             # Z3SS   -->  GVSS   E-     E+          
      4.80263975E-17    2      1000039        23                       # Z3SS   -->  GVSS   Z0                 
      6.18902358E-17    2      1000039        25                       # Z3SS   -->  GVSS   HL0                
#         PDG         Width
DECAY   1000035  5.33266163E+00   # Z4SS  decays
#          BR          NDA       ID1       ID2       ID3       ID4
      2.03612149E-08    2      1000022        22                       # Z4SS   -->  Z1SS   GM                 
      3.39964323E-08    2      1000023        22                       # Z4SS   -->  Z2SS   GM                 
      1.52449095E-10    2      1000025        22                       # Z4SS   -->  Z3SS   GM                 
      2.69327402E-01    2      1000024       -24                       # Z4SS   -->  W1SS+  W-                 
      2.69327402E-01    2     -1000024        24                       # Z4SS   -->  W1SS-  W+                 
      1.85474586E-02    2      1000022        23                       # Z4SS   -->  Z1SS   Z0                 
      1.98485190E-03    2      1000023        23                       # Z4SS   -->  Z2SS   Z0                 
      3.73517706E-09    3      1000022         2        -2             # Z4SS   -->  Z1SS   UP     UB          
      3.42704221E-09    3      1000022         1        -1             # Z4SS   -->  Z1SS   DN     DB          
      3.42704221E-09    3      1000022         3        -3             # Z4SS   -->  Z1SS   ST     SB          
      3.73517706E-09    3      1000022         4        -4             # Z4SS   -->  Z1SS   CH     CB          
      7.26791541E-07    3      1000022         5        -5             # Z4SS   -->  Z1SS   BT     BB          
      2.21600135E-06    3      1000022        11       -11             # Z4SS   -->  Z1SS   E-     E+          
      2.21600135E-06    3      1000022        13       -13             # Z4SS   -->  Z1SS   MU-    MU+         
      4.57565602E-06    3      1000022        15       -15             # Z4SS   -->  Z1SS   TAU-   TAU+        
      3.48893604E-06    3      1000022        12       -12             # Z4SS   -->  Z1SS   NUE    ANUE        
      3.48893604E-06    3      1000022        14       -14             # Z4SS   -->  Z1SS   NUM    ANUM        
      3.71772740E-06    3      1000022        16       -16             # Z4SS   -->  Z1SS   NUT    ANUT        
      5.45032575E-10    3      1000023         2        -2             # Z4SS   -->  Z2SS   UP     UB          
      6.09800599E-10    3      1000023         1        -1             # Z4SS   -->  Z2SS   DN     DB          
      6.09800599E-10    3      1000023         3        -3             # Z4SS   -->  Z2SS   ST     SB          
      5.45032575E-10    3      1000023         4        -4             # Z4SS   -->  Z2SS   CH     CB          
      1.13976810E-08    3      1000023         5        -5             # Z4SS   -->  Z2SS   BT     BB          
      1.63844277E-07    3      1000023        11       -11             # Z4SS   -->  Z2SS   E-     E+          
      1.63844277E-07    3      1000023        13       -13             # Z4SS   -->  Z2SS   MU-    MU+         
      3.89042754E-07    3      1000023        15       -15             # Z4SS   -->  Z2SS   TAU-   TAU+        
      2.60395183E-07    3      1000023        12       -12             # Z4SS   -->  Z2SS   NUE    ANUE        
      2.60395183E-07    3      1000023        14       -14             # Z4SS   -->  Z2SS   NUM    ANUM        
      2.90989277E-07    3      1000023        16       -16             # Z4SS   -->  Z2SS   NUT    ANUT        
      6.34403641E-09    3      1000025         2        -2             # Z4SS   -->  Z3SS   UP     UB          
      8.18073342E-09    3      1000025         1        -1             # Z4SS   -->  Z3SS   DN     DB          
      8.18073342E-09    3      1000025         3        -3             # Z4SS   -->  Z3SS   ST     SB          
      6.34403641E-09    3      1000025         4        -4             # Z4SS   -->  Z3SS   CH     CB          
      7.66640362E-10    3      1000025         5        -5             # Z4SS   -->  Z3SS   BT     BB          
      1.85599147E-09    3      1000025        11       -11             # Z4SS   -->  Z3SS   E-     E+          
      1.85599147E-09    3      1000025        13       -13             # Z4SS   -->  Z3SS   MU-    MU+         
      1.45290047E-09    3      1000025        15       -15             # Z4SS   -->  Z3SS   TAU-   TAU+        
      3.69120823E-09    3      1000025        12       -12             # Z4SS   -->  Z3SS   NUE    ANUE        
      3.69120823E-09    3      1000025        14       -14             # Z4SS   -->  Z3SS   NUM    ANUM        
      3.69105502E-09    3      1000025        16       -16             # Z4SS   -->  Z3SS   NUT    ANUT        
      1.55953854E-01    2      1000022        25                       # Z4SS   -->  Z1SS   HL0                
      2.49786690E-01    2      1000023        25                       # Z4SS   -->  Z2SS   HL0                
      4.88138467E-04    2      2000011       -11                       # Z4SS   -->  ER-    E+                 
      4.88138467E-04    2     -2000011        11                       # Z4SS   -->  ER+    E-                 
      4.88138467E-04    2      2000013       -13                       # Z4SS   -->  MUR-   MU+                
      4.88138467E-04    2     -2000013        13                       # Z4SS   -->  MUR+   MU-                
      1.65489390E-02    2      1000015       -15                       # Z4SS   -->  TAU1-  TAU+               
      1.65489390E-02    2     -1000015        15                       # Z4SS   -->  TAU1+  TAU-               
      1.07591424E-18    2      1000039        22                       # Z4SS   -->  GVSS   GM                 
      2.65516252E-20    3      1000039        11       -11             # Z4SS   -->  GVSS   E-     E+          
      6.09899258E-17    2      1000039        23                       # Z4SS   -->  GVSS   Z0                 
      3.97627540E-17    2      1000039        25                       # Z4SS   -->  GVSS   HL0                
#         PDG         Width
DECAY   1000024  5.08679077E-02   # W1SS+ decays
#          BR          NDA       ID1       ID2       ID3       ID4
      2.52344034E-06    3      1000022         2        -1             # W1SS+  -->  Z1SS   UP     DB          
      2.52344034E-06    3      1000022         4        -3             # W1SS+  -->  Z1SS   CH     SB          
      1.25266379E-03    3      1000022       -11        12             # W1SS+  -->  Z1SS   E+     NUE         
      1.25266379E-03    3      1000022       -13        14             # W1SS+  -->  Z1SS   MU+    NUM         
      1.29762990E-03    3      1000022       -15        16             # W1SS+  -->  Z1SS   TAU+   NUT         
      8.05840433E-01    2      1000022        24                       # W1SS+  -->  Z1SS   W+                 
      5.13013412E-17    3      1000023       -11        12             # W1SS+  -->  Z2SS   E+     NUE         
      5.13013412E-17    3      1000023       -13        14             # W1SS+  -->  Z2SS   MU+    NUM         
      1.90351546E-01    2     -1000015        16                       # W1SS+  -->  TAU1+  NUT                
#         PDG         Width
DECAY   1000037  4.78405762E+00   # W2SS+ decays
#          BR          NDA       ID1       ID2       ID3       ID4
      6.51662324E-09    3      1000022         2        -1             # W2SS+  -->  Z1SS   UP     DB          
      6.51662324E-09    3      1000022         4        -3             # W2SS+  -->  Z1SS   CH     SB          
      5.89257979E-06    3      1000022       -11        12             # W2SS+  -->  Z1SS   E+     NUE         
      5.89257979E-06    3      1000022       -13        14             # W2SS+  -->  Z1SS   MU+    NUM         
      9.07177855E-06    3      1000022       -15        16             # W2SS+  -->  Z1SS   TAU+   NUT         
      1.72842756E-01    2      1000022        24                       # W2SS+  -->  Z1SS   W+                 
      4.21464363E-10    3      1000023         2        -1             # W2SS+  -->  Z2SS   UP     DB          
      4.21464363E-10    3      1000023         4        -3             # W2SS+  -->  Z2SS   CH     SB          
      1.79941637E-07    3      1000023       -11        12             # W2SS+  -->  Z2SS   E+     NUE         
      1.79941637E-07    3      1000023       -13        14             # W2SS+  -->  Z2SS   MU+    NUM         
      4.50597668E-07    3      1000023       -15        16             # W2SS+  -->  Z2SS   TAU+   NUT         
      2.47086614E-01    2      1000023        24                       # W2SS+  -->  Z2SS   W+                 
      3.78294871E-08    3      1000025         2        -1             # W2SS+  -->  Z3SS   UP     DB          
      3.78294871E-08    3      1000025         4        -3             # W2SS+  -->  Z3SS   CH     SB          
      1.26088846E-08    3      1000025       -11        12             # W2SS+  -->  Z3SS   E+     NUE         
      1.26088846E-08    3      1000025       -13        14             # W2SS+  -->  Z3SS   MU+    NUM         
      1.26091422E-08    3      1000025       -15        16             # W2SS+  -->  Z3SS   TAU+   NUT         
      1.41706118E-13    3      1000035         2        -1             # W2SS+  -->  Z4SS   UP     DB          
      4.73685388E-14    3      1000035       -11        12             # W2SS+  -->  Z4SS   E+     NUE         
      4.73685388E-14    3      1000035       -13        14             # W2SS+  -->  Z4SS   MU+    NUM         
      3.61400507E-02    2     -1000015        16                       # W2SS+  -->  TAU1+  NUT                
      2.56188363E-01    2      1000024        23                       # W2SS+  -->  W1SS+  Z0                 
      6.97128577E-10    3      1000024         1        -1             # W2SS+  -->  W1SS+  DN     DB          
      6.97128577E-10    3      1000024         3        -3             # W2SS+  -->  W1SS+  ST     SB          
      9.51258960E-10    3      1000024         2        -2             # W2SS+  -->  W1SS+  UP     UB          
      9.51258960E-10    3      1000024         4        -4             # W2SS+  -->  W1SS+  CH     CB          
      3.40273914E-07    3      1000024        12       -12             # W2SS+  -->  W1SS+  NUE    ANUE        
      3.40273914E-07    3      1000024        14       -14             # W2SS+  -->  W1SS+  NUM    ANUM        
      2.83570103E-07    3      1000024        11       -11             # W2SS+  -->  W1SS+  E-     E+          
      2.83570103E-07    3      1000024        13       -13             # W2SS+  -->  W1SS+  MU-    MU+         
      3.18729775E-07    3      1000024        15       -15             # W2SS+  -->  W1SS+  TAU-   TAU+        
      2.87718773E-01    2      1000024        25                       # W2SS+  -->  W1SS+  HL0                
#         PDG         Width
DECAY        25  3.34974169E-03   # HL0   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      6.17636697E-09    2           11       -11                       # HL0    -->  E-     E+                 
      2.60776171E-04    2           13       -13                       # HL0    -->  MU-    MU+                
      7.45932758E-02    2           15       -15                       # HL0    -->  TAU-   TAU+               
      7.74652108E-06    2            1        -1                       # HL0    -->  DN     DB                 
      3.12998309E-03    2            3        -3                       # HL0    -->  ST     SB                 
      6.70358479E-01    2            5        -5                       # HL0    -->  BT     BB                 
      2.45423507E-06    2            2        -2                       # HL0    -->  UP     UB                 
      4.58685495E-02    2            4        -4                       # HL0    -->  CH     CB                 
      2.67822668E-03    2           22        22                       # HL0    -->  GM     GM                 
      5.54954708E-02    2           21        21                       # HL0    -->  GL     GL                 
      7.47911911E-03    3           24        11       -12             # HL0    -->  W+     E-     ANUE        
      7.47911911E-03    3           24        13       -14             # HL0    -->  W+     MU-    ANUM        
      7.47911911E-03    3           24        15       -16             # HL0    -->  W+     TAU-   ANUT        
      2.24373583E-02    3           24        -2         1             # HL0    -->  W+     UB     DN          
      2.24373583E-02    3           24        -4         3             # HL0    -->  W+     CB     ST          
      7.47911911E-03    3          -24       -11        12             # HL0    -->  W-     E+     NUE         
      7.47911911E-03    3          -24       -13        14             # HL0    -->  W-     MU+    NUM         
      7.47911911E-03    3          -24       -15        16             # HL0    -->  W-     TAU+   NUT         
      2.24373583E-02    3          -24         2        -1             # HL0    -->  W-     UP     DB          
      2.24373583E-02    3          -24         4        -3             # HL0    -->  W-     CH     SB          
      8.87775561E-04    3           23        12       -12             # HL0    -->  Z0     NUE    ANUE        
      8.87775561E-04    3           23        14       -14             # HL0    -->  Z0     NUM    ANUM        
      8.87775561E-04    3           23        16       -16             # HL0    -->  Z0     NUT    ANUT        
      4.46808495E-04    3           23        11       -11             # HL0    -->  Z0     E-     E+          
      4.46808495E-04    3           23        13       -13             # HL0    -->  Z0     MU-    MU+         
      4.46808495E-04    3           23        15       -15             # HL0    -->  Z0     TAU-   TAU+        
      1.53072795E-03    3           23         2        -2             # HL0    -->  Z0     UP     UB          
      1.53072795E-03    3           23         4        -4             # HL0    -->  Z0     CH     CB          
      1.97195495E-03    3           23         1        -1             # HL0    -->  Z0     DN     DB          
      1.97195495E-03    3           23         3        -3             # HL0    -->  Z0     ST     SB          
      1.97195495E-03    3           23         5        -5             # HL0    -->  Z0     BT     BB          
#         PDG         Width
DECAY        35  6.97842789E+00   # HH0   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      1.16385683E-08    2           11       -11                       # HH0    -->  E-     E+                 
      4.91401297E-04    2           13       -13                       # HH0    -->  MU-    MU+                
      1.40744284E-01    2           15       -15                       # HH0    -->  TAU-   TAU+               
      1.41753280E-05    2            1        -1                       # HH0    -->  DN     DB                 
      5.72754955E-03    2            3        -3                       # HH0    -->  ST     SB                 
      7.90122569E-01    2            5        -5                       # HH0    -->  BT     BB                 
      9.04838704E-11    2            2        -2                       # HH0    -->  UP     UB                 
      1.17059358E-06    2            4        -4                       # HH0    -->  CH     CB                 
      6.03690334E-02    2            6        -6                       # HH0    -->  TP     TB                 
      9.27879569E-08    2           22        22                       # HH0    -->  GM     GM                 
      1.23124828E-05    2           21        21                       # HH0    -->  GL     GL                 
      4.79936753E-05    2           24       -24                       # HH0    -->  W+     W-                 
      2.45742249E-05    2           23        23                       # HH0    -->  Z0     Z0                 
      4.74233559E-04    2      1000022   1000022                       # HH0    -->  Z1SS   Z1SS               
      1.79337605E-03    2      1000022   1000023                       # HH0    -->  Z1SS   Z2SS               
      1.66362195E-04    2           25        25                       # HH0    -->  HL0    HL0                
      3.98997463E-06    2      2000011  -2000011                       # HH0    -->  ER-    ER+                
      3.98490056E-06    2      2000013  -2000013                       # HH0    -->  MUR-   MUR+               
      2.91930814E-06    2      1000015  -1000015                       # HH0    -->  TAU1-  TAU1+              
#         PDG         Width
DECAY        36  6.99094009E+00   # HA0   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      1.15420944E-08    2           11       -11                       # HA0    -->  E-     E+                 
      4.87328041E-04    2           13       -13                       # HA0    -->  MU-    MU+                
      1.39578030E-01    2           15       -15                       # HA0    -->  TAU-   TAU+               
      1.40584916E-05    2            1        -1                       # HA0    -->  DN     DB                 
      5.68034220E-03    2            3        -3                       # HA0    -->  ST     SB                 
      7.83617496E-01    2            5        -5                       # HA0    -->  BT     BB                 
      8.88547499E-11    2            2        -2                       # HA0    -->  UP     UB                 
      1.15034663E-06    2            4        -4                       # HA0    -->  CH     CB                 
      6.05697967E-02    2            6        -6                       # HA0    -->  TP     TB                 
      3.09284360E-07    2           22        22                       # HA0    -->  GM     GM                 
      3.90074383E-05    2           21        21                       # HA0    -->  GL     GL                 
      8.49823293E-04    2      1000022   1000022                       # HA0    -->  Z1SS   Z1SS               
      9.11534950E-03    2      1000022   1000023                       # HA0    -->  Z1SS   Z2SS               
      4.72662978E-05    2           25        23                       # HA0    -->  HL0    Z0                 
#         PDG         Width
DECAY        37  6.52123451E+00   # H+    decays
#          BR          NDA       ID1       ID2       ID3       ID4
      1.24639010E-08    2           12       -11                       # H+     -->  NUE    E+                 
      5.26248361E-04    2           14       -13                       # H+     -->  NUM    MU+                
      1.50725380E-01    2           16       -15                       # H+     -->  NUT    TAU+               
      1.40348111E-05    2            2        -1                       # H+     -->  UP     DB                 
      5.67188114E-03    2            4        -3                       # H+     -->  CH     SB                 
      8.31610382E-01    2            6        -5                       # H+     -->  TP     BB                 
      1.14009334E-02    2      1000024   1000022                       # H+     -->  W1SS+  Z1SS               
      5.11903490E-05    2           25        24                       # H+     -->  HL0    W+                 
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

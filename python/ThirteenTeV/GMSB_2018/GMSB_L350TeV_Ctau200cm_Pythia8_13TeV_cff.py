
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
     1     3.50000000E+05   # Lambda scale of soft SSB
     2     7.00000000E+05   # M_mess overall messenger scale
     3     1.50000000E+01   # tan(beta)
     4     1.00000000E+00   # sign(mu)
     5     1.00000000E+00   # N_5 messenger index
     6     3.27294006E+02   # c_grav gravitino mass factor
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
        25     1.19521439E+02   #  h^0            
        35     1.68723303E+03   #  H^0            
        36     1.67615527E+03   #  A^0            
        37     1.68912537E+03   #  H^+            
   1000001     3.57301562E+03   #  dnl            
   1000002     3.57211157E+03   #  upl            
   1000003     3.57301562E+03   #  stl            
   1000004     3.57211206E+03   #  chl            
   1000005     3.36482544E+03   #  b1             
   1000006     3.08328418E+03   #  t1             
   1000011     1.22802441E+03   #  el-            
   1000012     1.21876587E+03   #  nuel           
   1000013     1.22802441E+03   #  mul-           
   1000014     1.21876587E+03   #  numl           
   1000015     6.11458435E+02   #  tau1           
   1000016     1.21199463E+03   #  nutl           
   1000021     2.59888721E+03   #  glss           
   1000022     5.03372681E+02   #  z1ss           
   1000023     9.59059998E+02   #  z2ss           
   1000024     9.59418518E+02   #  w1ss           
   1000025    -1.19855103E+03   #  z3ss           
   1000035     1.21149585E+03   #  z4ss           
   1000037     1.21225330E+03   #  w2ss           
   1000039     1.92900025E-05   #  gvss
   2000001     3.38093457E+03   #  dnr            
   2000002     3.39907593E+03   #  upr            
   2000003     3.38093457E+03   #  str            
   2000004     3.39907617E+03   #  chr            
   2000005     3.43748315E+03   #  b2             
   2000006     3.45906885E+03   #  t2             
   2000011     6.09659973E+02   #  er-            
   2000013     6.09659973E+02   #  mur-           
   2000015     1.22182471E+03   #  tau2           
Block ALPHA   # Effective Higgs mixing parameter
         -6.70838282E-02   # alpha
Block STOPMIX   # stop mixing matrix
  1  1     2.98777018E-02   # O_{11}
  1  2     9.99553561E-01   # O_{12}
  2  1    -9.99553561E-01   # O_{21}
  2  2     2.98777018E-02   # O_{22}
Block SBOTMIX   # sbottom mixing matrix
  1  1     1.12763099E-01   # O_{11}
  1  2     9.93621886E-01   # O_{12}
  2  1    -9.93621886E-01   # O_{21}
  2  2     1.12763099E-01   # O_{22}
Block STAUMIX   # stau mixing matrix
  1  1     2.49658674E-02   # O_{11}
  1  2     9.99688327E-01   # O_{12}
  2  1    -9.99688327E-01   # O_{21}
  2  2     2.49658674E-02   # O_{22}
Block NMIX   # neutralino mixing matrix
  1  1     9.98810112E-01   #
  1  2    -3.84472869E-03   #
  1  3     4.41573858E-02   #
  1  4    -2.03386247E-02   #
  2  1     1.45020857E-02   #
  2  2     9.73443747E-01   #
  2  3    -1.76392958E-01   #
  2  4     1.45199671E-01   #
  3  1     1.67319626E-02   #
  3  2    -2.27130279E-02   #
  3  3    -7.06261814E-01   #
  3  4    -7.07388401E-01   #
  4  1     4.34516557E-02   #
  4  2    -2.27765217E-01   #
  4  3    -6.84200346E-01   #
  4  4     6.91451550E-01   #
Block UMIX   # chargino U mixing matrix
  1  1    -9.70058799E-01   # U_{11}
  1  2     2.42870227E-01   # U_{12}
  2  1    -2.42870227E-01   # U_{21}
  2  2    -9.70058799E-01   # U_{22}
Block VMIX   # chargino V mixing matrix
  1  1    -9.79929209E-01   # V_{11}
  1  2     1.99345842E-01   # V_{12}
  2  1    -1.99345842E-01   # V_{21}
  2  2    -9.79929209E-01   # V_{22}
Block GAUGE Q=  3.16654077E+03   #
     1     3.57515931E-01   # g`
     2     6.52410865E-01   # g_2
     3     1.21975029E+00   # g_3
Block YU Q=  3.16654077E+03   #
  3  3     8.32704961E-01   # y_t
Block YD Q=  3.16654077E+03   #
  3  3     1.83760628E-01   # y_b
Block YE Q=  3.16654077E+03   #
  3  3     1.52978808E-01   # y_tau
Block HMIX Q=  3.16654077E+03   # Higgs mixing parameters
     1     1.18394910E+03   # mu(Q)
     2     1.43081198E+01   # tan(beta)(Q)
     3     2.51883026E+02   # Higgs vev at Q
     4     2.80949650E+06   # m_A^2(Q)
Block MSOFT Q=  3.16654077E+03   # DRbar SUSY breaking parameters
     1     5.13211487E+02   # M_1(Q)          
     2     9.34986145E+02   # M_2(Q)          
     3     2.36365674E+03   # M_3(Q)          
    21     1.33385800E+06   # MHd^2(Q)        
    22    -1.22752075E+06   # MHu^2(Q)        
    31     1.21775757E+03   # MeL(Q)          
    32     1.21775757E+03   # MmuL(Q)         
    33     1.21113538E+03   # MtauL(Q)        
    34     6.09089172E+02   # MeR(Q)          
    35     6.09089172E+02   # MmuR(Q)         
    36     6.00120422E+02   # MtauR(Q)        
    41     3.45662549E+03   # MqL1(Q)         
    42     3.45662549E+03   # MqL2(Q)         
    43     3.33054224E+03   # MqL3(Q)         
    44     3.28104395E+03   # MuR(Q)          
    45     3.28104395E+03   # McR(Q)          
    46     3.01061548E+03   # MtR(Q)          
    47     3.26234912E+03   # MdR(Q)          
    48     3.26234912E+03   # MsR(Q)          
    49     3.25059448E+03   # MbR(Q)          
Block AU Q=  3.16654077E+03   #
  1  1    -6.91412842E+02   # A_u
  2  2    -6.91412842E+02   # A_c
  3  3    -6.91412842E+02   # A_t
Block AD Q=  3.16654077E+03   #
  1  1    -7.69640198E+02   # A_d
  2  2    -7.69640198E+02   # A_s
  3  3    -7.69640198E+02   # A_b
Block AE Q=  3.16654077E+03   #
  1  1    -9.23325729E+01   # A_e
  2  2    -9.23325729E+01   # A_mu
  3  3    -9.23325729E+01   # A_tau
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
DECAY   1000021  6.51233792E-02   # GLSS  decays
#          BR          NDA       ID1       ID2       ID3       ID4
      5.01080863E-02    3      1000024         1        -2             # GLSS   -->  W1SS+  DN     UB          
      5.01080863E-02    3     -1000024         2        -1             # GLSS   -->  W1SS-  UP     DB          
      5.01080640E-02    3      1000024         3        -4             # GLSS   -->  W1SS+  ST     CB          
      5.01080640E-02    3     -1000024         4        -3             # GLSS   -->  W1SS-  CH     SB          
      5.49271479E-02    3      1000024         5        -6             # GLSS   -->  W1SS+  BT     TB          
      5.49271479E-02    3     -1000024         6        -5             # GLSS   -->  W1SS-  TP     BB          
      1.58578937E-03    3      1000037         1        -2             # GLSS   -->  W2SS+  DN     UB          
      1.58578937E-03    3     -1000037         2        -1             # GLSS   -->  W2SS-  UP     DB          
      1.58578902E-03    3      1000037         3        -4             # GLSS   -->  W2SS+  ST     CB          
      1.58578902E-03    3     -1000037         4        -3             # GLSS   -->  W2SS-  CH     SB          
      9.86516401E-02    3      1000037         5        -6             # GLSS   -->  W2SS+  BT     TB          
      9.86516401E-02    3     -1000037         6        -5             # GLSS   -->  W2SS-  TP     BB          
      7.69654616E-06    2      1000022        21                       # GLSS   -->  Z1SS   GL                 
      3.24418694E-02    3      1000022         2        -2             # GLSS   -->  Z1SS   UP     UB          
      9.53111332E-03    3      1000022         1        -1             # GLSS   -->  Z1SS   DN     DB          
      9.53111332E-03    3      1000022         3        -3             # GLSS   -->  Z1SS   ST     SB          
      3.24418545E-02    3      1000022         4        -4             # GLSS   -->  Z1SS   CH     CB          
      1.01519348E-02    3      1000022         5        -5             # GLSS   -->  Z1SS   BT     BB          
      4.97143529E-02    3      1000022         6        -6             # GLSS   -->  Z1SS   TP     TB          
      1.39506490E-04    2      1000023        21                       # GLSS   -->  Z2SS   GL                 
      2.51470990E-02    3      1000023         2        -2             # GLSS   -->  Z2SS   UP     UB          
      2.48355325E-02    3      1000023         1        -1             # GLSS   -->  Z2SS   DN     DB          
      2.48355325E-02    3      1000023         3        -3             # GLSS   -->  Z2SS   ST     SB          
      2.51470786E-02    3      1000023         4        -4             # GLSS   -->  Z2SS   CH     CB          
      3.14866155E-02    3      1000023         5        -5             # GLSS   -->  Z2SS   BT     BB          
      2.38046050E-02    3      1000023         6        -6             # GLSS   -->  Z2SS   TP     TB          
      1.99775398E-03    2      1000025        21                       # GLSS   -->  Z3SS   GL                 
      4.21278128E-06    3      1000025         2        -2             # GLSS   -->  Z3SS   UP     UB          
      5.09700976E-06    3      1000025         1        -1             # GLSS   -->  Z3SS   DN     DB          
      5.09700976E-06    3      1000025         3        -3             # GLSS   -->  Z3SS   ST     SB          
      4.21277855E-06    3      1000025         4        -4             # GLSS   -->  Z3SS   CH     CB          
      3.97982355E-03    3      1000025         5        -5             # GLSS   -->  Z3SS   BT     BB          
      7.95492828E-02    3      1000025         6        -6             # GLSS   -->  Z3SS   TP     TB          
      1.89110357E-03    2      1000035        21                       # GLSS   -->  Z4SS   GL                 
      8.04536161E-04    3      1000035         2        -2             # GLSS   -->  Z4SS   UP     UB          
      9.04212066E-04    3      1000035         1        -1             # GLSS   -->  Z4SS   DN     DB          
      9.04212066E-04    3      1000035         3        -3             # GLSS   -->  Z4SS   ST     SB          
      8.04535521E-04    3      1000035         4        -4             # GLSS   -->  Z4SS   CH     CB          
      4.77254763E-03    3      1000035         5        -5             # GLSS   -->  Z4SS   BT     BB          
      9.12243873E-02    3      1000035         6        -6             # GLSS   -->  Z4SS   TP     TB          
      5.63276986E-12    2      1000039        21                       # GLSS   -->  GVSS   GL                 
#         PDG         Width
DECAY   1000002  8.21454849E+01   # UPL   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      5.64335706E-03    2      1000022         2                       # UPL    -->  Z1SS   UP                 
      1.50161833E-01    2      1000023         2                       # UPL    -->  Z2SS   UP                 
      5.56456325E-05    2      1000025         2                       # UPL    -->  Z3SS   UP                 
      6.92609511E-03    2      1000035         2                       # UPL    -->  Z4SS   UP                 
      5.23174167E-01    2      1000021         2                       # UPL    -->  GLSS   UP                 
      3.02649051E-01    2      1000024         1                       # UPL    -->  W1SS+  DN                 
      1.13898534E-02    2      1000037         1                       # UPL    -->  W2SS+  DN                 
#         PDG         Width
DECAY   1000001  8.21676254E+01   # DNL   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      6.13824138E-03    2      1000022         1                       # DNL    -->  Z1SS   DN                 
      1.48540571E-01    2      1000023         1                       # DNL    -->  Z2SS   DN                 
      9.58017263E-05    2      1000025         1                       # DNL    -->  Z3SS   DN                 
      7.96671771E-03    2      1000035         1                       # DNL    -->  Z4SS   DN                 
      5.23748994E-01    2      1000021         1                       # DNL    -->  GLSS   DN                 
      2.96601295E-01    2     -1000024         2                       # DNL    -->  W1SS-  UP                 
      1.69083979E-02    2     -1000037         2                       # DNL    -->  W2SS-  UP                 
#         PDG         Width
DECAY   1000003  8.21676178E+01   # STL   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      6.13824185E-03    2      1000022         3                       # STL    -->  Z1SS   ST                 
      1.48540586E-01    2      1000023         3                       # STL    -->  Z2SS   ST                 
      9.58017336E-05    2      1000025         3                       # STL    -->  Z3SS   ST                 
      7.96671864E-03    2      1000035         3                       # STL    -->  Z4SS   ST                 
      5.23749053E-01    2      1000021         3                       # STL    -->  GLSS   ST                 
      2.96601236E-01    2     -1000024         4                       # STL    -->  W1SS-  CH                 
      1.69083942E-02    2     -1000037         4                       # STL    -->  W2SS-  CH                 
#         PDG         Width
DECAY   1000004  8.21454620E+01   # CHL   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      5.64335799E-03    2      1000022         4                       # CHL    -->  Z1SS   CH                 
      1.50161833E-01    2      1000023         4                       # CHL    -->  Z2SS   CH                 
      5.56456471E-05    2      1000025         4                       # CHL    -->  Z3SS   CH                 
      6.92609465E-03    2      1000035         4                       # CHL    -->  Z4SS   CH                 
      5.23174047E-01    2      1000021         4                       # CHL    -->  GLSS   CH                 
      3.02649170E-01    2      1000024         3                       # CHL    -->  W1SS+  ST                 
      1.13898572E-02    2      1000037         3                       # CHL    -->  W2SS+  ST                 
#         PDG         Width
DECAY   1000005  3.61842957E+01   # BT1   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      5.05896434E-02    2      1000022         5                       # BT1    -->  Z1SS   BT                 
      1.08378809E-02    2      1000023         5                       # BT1    -->  Z2SS   BT                 
      2.42584981E-02    2      1000025         5                       # BT1    -->  Z3SS   BT                 
      1.87854525E-02    2      1000035         5                       # BT1    -->  Z4SS   BT                 
      8.27533305E-01    2      1000021         5                       # BT1    -->  GLSS   BT                 
      2.13719271E-02    2     -1000024         6                       # BT1    -->  W1SS-  TP                 
      4.65950109E-02    2     -1000037         6                       # BT1    -->  W2SS-  TP                 
      2.82126548E-05    2          -24   1000006                       # BT1    -->  W-     TP1                
#         PDG         Width
DECAY   1000006  7.80598907E+01   # TP1   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      1.71607867E-01    2      1000021         6                       # TP1    -->  GLSS   TP                 
      8.40164721E-02    2      1000022         6                       # TP1    -->  Z1SS   TP                 
      1.09829241E-02    2      1000023         6                       # TP1    -->  Z2SS   TP                 
      1.86479807E-01    2      1000025         6                       # TP1    -->  Z3SS   TP                 
      1.73671111E-01    2      1000035         6                       # TP1    -->  Z4SS   TP                 
      2.09863037E-02    2      1000024         5                       # TP1    -->  W1SS+  BT                 
      3.52255553E-01    2      1000037         5                       # TP1    -->  W2SS+  BT                 
#         PDG         Width
DECAY   2000002  3.93463058E+01   # UPR   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      1.86370879E-01    2      1000022         2                       # UPR    -->  Z1SS   UP                 
      3.47918431E-05    2      1000023         2                       # UPR    -->  Z2SS   UP                 
      4.19221469E-05    2      1000025         2                       # UPR    -->  Z3SS   UP                 
      2.80982873E-04    2      1000035         2                       # UPR    -->  Z4SS   UP                 
      8.13271403E-01    2      1000021         2                       # UPR    -->  GLSS   UP                 
#         PDG         Width
DECAY   2000001  3.27129478E+01   # DNR   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      5.57145514E-02    2      1000022         1                       # DNR    -->  Z1SS   DN                 
      1.03864904E-05    2      1000023         1                       # DNR    -->  Z2SS   DN                 
      1.25001607E-05    2      1000025         1                       # DNR    -->  Z3SS   DN                 
      8.37758635E-05    2      1000035         1                       # DNR    -->  Z4SS   DN                 
      9.44178820E-01    2      1000021         1                       # DNR    -->  GLSS   DN                 
#         PDG         Width
DECAY   2000003  3.27129478E+01   # STR   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      5.57145514E-02    2      1000022         3                       # STR    -->  Z1SS   ST                 
      1.03864904E-05    2      1000023         3                       # STR    -->  Z2SS   ST                 
      1.25001607E-05    2      1000025         3                       # STR    -->  Z3SS   ST                 
      8.37758635E-05    2      1000035         3                       # STR    -->  Z4SS   ST                 
      9.44178820E-01    2      1000021         3                       # STR    -->  GLSS   ST                 
#         PDG         Width
DECAY   2000004  3.93462677E+01   # CHR   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      1.86371014E-01    2      1000022         4                       # CHR    -->  Z1SS   CH                 
      3.47918649E-05    2      1000023         4                       # CHR    -->  Z2SS   CH                 
      4.19221687E-05    2      1000025         4                       # CHR    -->  Z3SS   CH                 
      2.80983077E-04    2      1000035         4                       # CHR    -->  Z4SS   CH                 
      8.13271284E-01    2      1000021         4                       # CHR    -->  GLSS   CH                 
#         PDG         Width
DECAY   2000005  1.07239754E+02   # BT2   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      4.42644767E-03    2      1000022         5                       # BT2    -->  Z1SS   BT                 
      1.05549827E-01    2      1000023         5                       # BT2    -->  Z2SS   BT                 
      8.95130076E-03    2      1000025         5                       # BT2    -->  Z3SS   BT                 
      1.54173300E-02    2      1000035         5                       # BT2    -->  Z4SS   BT                 
      3.20224583E-01    2      1000021         5                       # BT2    -->  GLSS   BT                 
      2.19094366E-01    2     -1000024         6                       # BT2    -->  W1SS-  TP                 
      3.24837893E-01    2     -1000037         6                       # BT2    -->  W2SS-  TP                 
      1.49819441E-03    2          -24   1000006                       # BT2    -->  W-     TP1                
#         PDG         Width
DECAY   2000006  1.09145164E+02   # TP2   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      3.15428674E-01    2      1000021         6                       # TP2    -->  GLSS   TP                 
      2.15896115E-01    2      1000024         5                       # TP2    -->  W1SS+  BT                 
      2.75635235E-02    2      1000037         5                       # TP2    -->  W2SS+  BT                 
      8.82906839E-04    2           23   1000006                       # TP2    -->  Z0     TP1                
      2.98312772E-03    2           25   1000006                       # TP2    -->  HL0    TP1                
      6.89631488E-05    2           24   1000005                       # TP2    -->  W+     BT1                
      4.16048151E-03    2      1000022         6                       # TP2    -->  Z1SS   TP                 
      1.11650273E-01    2      1000023         6                       # TP2    -->  Z2SS   TP                 
      1.59837723E-01    2      1000025         6                       # TP2    -->  Z3SS   TP                 
      1.61528096E-01    2      1000035         6                       # TP2    -->  Z4SS   TP                 
#         PDG         Width
DECAY   1000011  3.29815292E+00   # EL-   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      3.22374284E-01    2      1000022        11                       # EL-    -->  Z1SS   E-                 
      2.29692057E-01    2      1000023        11                       # EL-    -->  Z2SS   E-                 
      6.44045258E-07    2      1000025        11                       # EL-    -->  Z3SS   E-                 
      4.65760022E-05    2      1000035        11                       # EL-    -->  Z4SS   E-                 
      4.47765976E-01    2     -1000024        12                       # EL-    -->  W1SS-  NUE                
      1.20421850E-04    2     -1000037        12                       # EL-    -->  W2SS-  NUE                
      2.61990827E-15    2           11   1000039                       # EL-    -->  E-     GVSS               
#         PDG         Width
DECAY   1000013  3.29815292E+00   # MUL-  decays
#          BR          NDA       ID1       ID2       ID3       ID4
      3.22374284E-01    2      1000022        13                       # MUL-   -->  Z1SS   MU-                
      2.29692057E-01    2      1000023        13                       # MUL-   -->  Z2SS   MU-                
      6.44041108E-07    2      1000025        13                       # MUL-   -->  Z3SS   MU-                
      4.65750454E-05    2      1000035        13                       # MUL-   -->  Z4SS   MU-                
      4.47765976E-01    2     -1000024        14                       # MUL-   -->  W1SS-  NUM                
      1.20421850E-04    2     -1000037        14                       # MUL-   -->  W2SS-  NUM                
      2.61990827E-15    2           13   1000039                       # MUL-   -->  MU-    GVSS               
#         PDG         Width
DECAY   1000015  3.22224319E-01   # TAU1- decays
#          BR          NDA       ID1       ID2       ID3       ID4
      1.00000000E+00    2      1000022        15                       # TAU1-  -->  Z1SS   TAU-               
      8.20698216E-16    2           15   1000039                       # TAU1-  -->  TAU-   GVSS               
#         PDG         Width
DECAY   1000012  3.19698715E+00   # NUEL  decays
#          BR          NDA       ID1       ID2       ID3       ID4
      3.37357759E-01    2      1000022        12                       # NUEL   -->  Z1SS   NUE                
      2.16870576E-01    2      1000023        12                       # NUEL   -->  Z2SS   NUE                
      1.76828951E-06    2      1000025        12                       # NUEL   -->  Z3SS   NUE                
      1.43770476E-05    2      1000035        12                       # NUEL   -->  Z4SS   NUE                
      4.45741087E-01    2      1000024        11                       # NUEL   -->  W1SS+  E-                 
      1.44890528E-05    2      1000037        11                       # NUEL   -->  W2SS+  E-                 
      2.60244965E-15    2           12   1000039                       # NUEL   -->  NUE    GVSS               
#         PDG         Width
DECAY   1000014  3.19698715E+00   # NUML  decays
#          BR          NDA       ID1       ID2       ID3       ID4
      3.37357759E-01    2      1000022        14                       # NUML   -->  Z1SS   NUM                
      2.16870576E-01    2      1000023        14                       # NUML   -->  Z2SS   NUM                
      1.76828951E-06    2      1000025        14                       # NUML   -->  Z3SS   NUM                
      1.43770476E-05    2      1000035        14                       # NUML   -->  Z4SS   NUM                
      4.45741057E-01    2      1000024        13                       # NUML   -->  W1SS+  MU-                
      1.44871610E-05    2      1000037        13                       # NUML   -->  W2SS+  MU-                
      2.60244965E-15    2           14   1000039                       # NUML   -->  NUM    GVSS               
#         PDG         Width
DECAY   1000016  3.39215326E+00   # NUTL  decays
#          BR          NDA       ID1       ID2       ID3       ID4
      3.14725965E-01    2      1000022        16                       # NUTL   -->  Z1SS   NUT                
      1.95917428E-01    2      1000023        16                       # NUTL   -->  Z2SS   NUT                
      7.45302941E-07    2      1000025        16                       # NUTL   -->  Z3SS   NUT                
      6.44937046E-08    2      1000035        16                       # NUTL   -->  Z4SS   NUT                
      4.03766185E-01    2      1000024        15                       # NUTL   -->  W1SS+  TAU-               
      8.55876729E-02    2           24   1000015                       # NUTL   -->  W+     TAU1-              
      1.97153759E-06    3     -1000015        16        15             # NUTL   -->  TAU1+  NUT    TAU-        
      2.38533753E-15    2           16   1000039                       # NUTL   -->  NUT    GVSS               
#         PDG         Width
DECAY   2000011  3.13386351E-01   # ER-   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      1.00000000E+00    2      1000022        11                       # ER-    -->  Z1SS   E-                 
      8.31534362E-16    2           11   1000039                       # ER-    -->  E-     GVSS               
#         PDG         Width
DECAY   2000013  3.13386202E-01   # MUR-  decays
#          BR          NDA       ID1       ID2       ID3       ID4
      1.00000000E+00    2      1000022        13                       # MUR-   -->  Z1SS   MU-                
      8.31534732E-16    2           13   1000039                       # MUR-   -->  MU-    GVSS               
#         PDG         Width
DECAY   2000015  3.55055857E+00   # TAU2- decays
#          BR          NDA       ID1       ID2       ID3       ID4
      2.97077447E-01    2      1000022        15                       # TAU2-  -->  Z1SS   TAU-               
      2.05452889E-01    2      1000023        15                       # TAU2-  -->  Z2SS   TAU-               
      1.18406686E-04    2      1000025        15                       # TAU2-  -->  Z3SS   TAU-               
      4.66857273E-05    2      1000035        15                       # TAU2-  -->  Z4SS   TAU-               
      3.99294168E-01    2     -1000024        16                       # TAU2-  -->  W1SS-  NUT                
      4.36130431E-05    2     -1000037        16                       # TAU2-  -->  W2SS-  NUT                
      4.27921675E-02    2           23   1000015                       # TAU2-  -->  Z0     TAU1-              
      5.51747009E-02    2           25   1000015                       # TAU2-  -->  HL0    TAU1-              
#         PDG         Width
DECAY   1000022  9.86065266E-17   # Z1SS  decays
#          BR          NDA       ID1       ID2       ID3       ID4
      7.73663223E-01    2      1000039        22                       # Z1SS   -->  GVSS   GM                 
      1.77008770E-02    3      1000039        11       -11             # Z1SS   -->  GVSS   E-     E+          
      2.08515108E-01    2      1000039        23                       # Z1SS   -->  GVSS   Z0                 
      1.20771954E-04    2      1000039        25                       # Z1SS   -->  GVSS   HL0                
#         PDG         Width
DECAY   1000023  5.26740924E-02   # Z2SS  decays
#          BR          NDA       ID1       ID2       ID3       ID4
      2.05788865E-06    2      1000022        22                       # Z2SS   -->  Z1SS   GM                 
      5.16658016E-02    2      1000022        23                       # Z2SS   -->  Z1SS   Z0                 
      8.57316650E-07    3      1000022         2        -2             # Z2SS   -->  Z1SS   UP     UB          
      9.19286720E-07    3      1000022         1        -1             # Z2SS   -->  Z1SS   DN     DB          
      9.19286720E-07    3      1000022         3        -3             # Z2SS   -->  Z1SS   ST     SB          
      8.57316024E-07    3      1000022         4        -4             # Z2SS   -->  Z1SS   CH     CB          
      2.58580781E-06    3      1000022         5        -5             # Z2SS   -->  Z1SS   BT     BB          
      4.52565873E-04    3      1000022        11       -11             # Z2SS   -->  Z1SS   E-     E+          
      4.52565873E-04    3      1000022        13       -13             # Z2SS   -->  Z1SS   MU-    MU+         
      4.67017904E-04    3      1000022        15       -15             # Z2SS   -->  Z1SS   TAU-   TAU+        
      4.73640132E-04    3      1000022        12       -12             # Z2SS   -->  Z1SS   NUE    ANUE        
      4.73640132E-04    3      1000022        14       -14             # Z2SS   -->  Z1SS   NUM    ANUM        
      4.91674931E-04    3      1000022        16       -16             # Z2SS   -->  Z1SS   NUT    ANUT        
      7.33741105E-01    2      1000022        25                       # Z2SS   -->  Z1SS   HL0                
      3.45805567E-03    2      2000011       -11                       # Z2SS   -->  ER-    E+                 
      3.45805567E-03    2     -2000011        11                       # Z2SS   -->  ER+    E-                 
      3.45805543E-03    2      2000013       -13                       # Z2SS   -->  MUR-   MU+                
      3.45805543E-03    2     -2000013        13                       # Z2SS   -->  MUR+   MU-                
      9.89707485E-02    2      1000015       -15                       # Z2SS   -->  TAU1-  TAU+               
      9.89707485E-02    2     -1000015        15                       # Z2SS   -->  TAU1+  TAU-               
      1.10533361E-14    2      1000039        22                       # Z2SS   -->  GVSS   GM                 
      2.64705340E-16    3      1000039        11       -11             # Z2SS   -->  GVSS   E-     E+          
      3.34658787E-14    2      1000039        23                       # Z2SS   -->  GVSS   Z0                 
      3.96236004E-16    2      1000039        25                       # Z2SS   -->  GVSS   HL0                
#         PDG         Width
DECAY   1000025  4.14483023E+00   # Z3SS  decays
#          BR          NDA       ID1       ID2       ID3       ID4
      9.54410240E-08    2      1000022        22                       # Z3SS   -->  Z1SS   GM                 
      2.15405834E-07    2      1000023        22                       # Z3SS   -->  Z2SS   GM                 
      2.60399938E-01    2      1000024       -24                       # Z3SS   -->  W1SS+  W-                 
      2.60399938E-01    2     -1000024        24                       # Z3SS   -->  W1SS-  W+                 
      1.55504569E-01    2      1000022        23                       # Z3SS   -->  Z1SS   Z0                 
      2.61076391E-01    2      1000023        23                       # Z3SS   -->  Z2SS   Z0                 
      1.06555521E-10    3      1000022         2        -2             # Z3SS   -->  Z1SS   UP     UB          
      2.80845104E-11    3      1000022         1        -1             # Z3SS   -->  Z1SS   DN     DB          
      2.80845104E-11    3      1000022         3        -3             # Z3SS   -->  Z1SS   ST     SB          
      1.06555487E-10    3      1000022         4        -4             # Z3SS   -->  Z1SS   CH     CB          
      1.03417767E-06    3      1000022         5        -5             # Z3SS   -->  Z1SS   BT     BB          
      9.59607771E-09    3      1000022        11       -11             # Z3SS   -->  Z1SS   E-     E+          
      9.59607771E-09    3      1000022        13       -13             # Z3SS   -->  Z1SS   MU-    MU+         
      3.81655582E-06    3      1000022        15       -15             # Z3SS   -->  Z1SS   TAU-   TAU+        
      6.33155679E-08    3      1000022        12       -12             # Z3SS   -->  Z1SS   NUE    ANUE        
      6.33155679E-08    3      1000022        14       -14             # Z3SS   -->  Z1SS   NUM    ANUM        
      7.13261414E-08    3      1000022        16       -16             # Z3SS   -->  Z1SS   NUT    ANUT        
      3.44900063E-12    3      1000023         2        -2             # Z3SS   -->  Z2SS   UP     UB          
      5.86593144E-12    3      1000023         1        -1             # Z3SS   -->  Z2SS   DN     DB          
      5.86593144E-12    3      1000023         3        -3             # Z3SS   -->  Z2SS   ST     SB          
      3.44899890E-12    3      1000023         4        -4             # Z3SS   -->  Z2SS   CH     CB          
      2.88586524E-08    3      1000023         5        -5             # Z3SS   -->  Z2SS   BT     BB          
      1.12351572E-09    3      1000023        11       -11             # Z3SS   -->  Z2SS   E-     E+          
      1.12351572E-09    3      1000023        13       -13             # Z3SS   -->  Z2SS   MU-    MU+         
      7.13234158E-07    3      1000023        15       -15             # Z3SS   -->  Z2SS   TAU-   TAU+        
      7.77296449E-09    3      1000023        12       -12             # Z3SS   -->  Z2SS   NUE    ANUE        
      7.77296449E-09    3      1000023        14       -14             # Z3SS   -->  Z2SS   NUM    ANUM        
      9.67431113E-09    3      1000023        16       -16             # Z3SS   -->  Z2SS   NUT    ANUT        
      2.13938262E-02    2      1000022        25                       # Z3SS   -->  Z1SS   HL0                
      2.33140495E-03    2      1000023        25                       # Z3SS   -->  Z2SS   HL0                
      1.13123897E-04    2      2000011       -11                       # Z3SS   -->  ER-    E+                 
      1.13123897E-04    2     -2000011        11                       # Z3SS   -->  ER+    E-                 
      1.13123897E-04    2      2000013       -13                       # Z3SS   -->  MUR-   MU+                
      1.13123897E-04    2     -2000013        13                       # Z3SS   -->  MUR+   MU-                
      1.92176010E-02    2      1000015       -15                       # Z3SS   -->  TAU1-  TAU+               
      1.92176010E-02    2     -1000015        15                       # Z3SS   -->  TAU1+  TAU-               
      2.55923849E-20    2      1000039        22                       # Z3SS   -->  GVSS   GM                 
      6.22344510E-22    3      1000039        11       -11             # Z3SS   -->  GVSS   E-     E+          
      3.92926302E-16    2      1000039        23                       # Z3SS   -->  GVSS   Z0                 
      5.03104219E-16    2      1000039        25                       # Z3SS   -->  GVSS   HL0                
#         PDG         Width
DECAY   1000035  4.85948706E+00   # Z4SS  decays
#          BR          NDA       ID1       ID2       ID3       ID4
      9.41621447E-09    2      1000022        22                       # Z4SS   -->  Z1SS   GM                 
      3.84765393E-08    2      1000023        22                       # Z4SS   -->  Z2SS   GM                 
      2.31865374E-10    2      1000025        22                       # Z4SS   -->  Z3SS   GM                 
      2.79978245E-01    2      1000024       -24                       # Z4SS   -->  W1SS+  W-                 
      2.79978245E-01    2     -1000024        24                       # Z4SS   -->  W1SS-  W+                 
      1.81793980E-02    2      1000022        23                       # Z4SS   -->  Z1SS   Z0                 
      3.03001935E-03    2      1000023        23                       # Z4SS   -->  Z2SS   Z0                 
      4.79628381E-09    3      1000022         2        -2             # Z4SS   -->  Z1SS   UP     UB          
      4.27458247E-09    3      1000022         1        -1             # Z4SS   -->  Z1SS   DN     DB          
      4.27458247E-09    3      1000022         3        -3             # Z4SS   -->  Z1SS   ST     SB          
      4.79628115E-09    3      1000022         4        -4             # Z4SS   -->  Z1SS   CH     CB          
      8.40935513E-07    3      1000022         5        -5             # Z4SS   -->  Z1SS   BT     BB          
      3.77320862E-06    3      1000022        11       -11             # Z4SS   -->  Z1SS   E-     E+          
      3.77320862E-06    3      1000022        13       -13             # Z4SS   -->  Z1SS   MU-    MU+         
      8.35954233E-06    3      1000022        15       -15             # Z4SS   -->  Z1SS   TAU-   TAU+        
      6.82466225E-06    3      1000022        12       -12             # Z4SS   -->  Z1SS   NUE    ANUE        
      6.82466225E-06    3      1000022        14       -14             # Z4SS   -->  Z1SS   NUM    ANUM        
      7.95195774E-06    3      1000022        16       -16             # Z4SS   -->  Z1SS   NUT    ANUT        
      1.39632150E-09    3      1000023         2        -2             # Z4SS   -->  Z2SS   UP     UB          
      1.58671598E-09    3      1000023         1        -1             # Z4SS   -->  Z2SS   DN     DB          
      1.58671598E-09    3      1000023         3        -3             # Z4SS   -->  Z2SS   ST     SB          
      1.39632061E-09    3      1000023         4        -4             # Z4SS   -->  Z2SS   CH     CB          
      2.75000591E-08    3      1000023         5        -5             # Z4SS   -->  Z2SS   BT     BB          
      7.37007724E-07    3      1000023        11       -11             # Z4SS   -->  Z2SS   E-     E+          
      7.37007724E-07    3      1000023        13       -13             # Z4SS   -->  Z2SS   MU-    MU+         
      1.87479338E-06    3      1000023        15       -15             # Z4SS   -->  Z2SS   TAU-   TAU+        
      1.40950920E-06    3      1000023        12       -12             # Z4SS   -->  Z2SS   NUE    ANUE        
      1.40950920E-06    3      1000023        14       -14             # Z4SS   -->  Z2SS   NUM    ANUM        
      1.89267223E-06    3      1000023        16       -16             # Z4SS   -->  Z2SS   NUT    ANUT        
      8.95998209E-09    3      1000025         2        -2             # Z4SS   -->  Z3SS   UP     UB          
      1.15541194E-08    3      1000025         1        -1             # Z4SS   -->  Z3SS   DN     DB          
      1.15541194E-08    3      1000025         3        -3             # Z4SS   -->  Z3SS   ST     SB          
      8.95998209E-09    3      1000025         4        -4             # Z4SS   -->  Z3SS   CH     CB          
      1.50660973E-09    3      1000025         5        -5             # Z4SS   -->  Z3SS   BT     BB          
      2.62283795E-09    3      1000025        11       -11             # Z4SS   -->  Z3SS   E-     E+          
      2.62283795E-09    3      1000025        13       -13             # Z4SS   -->  Z3SS   MU-    MU+         
      2.10273443E-09    3      1000025        15       -15             # Z4SS   -->  Z3SS   TAU-   TAU+        
      5.19810017E-09    3      1000025        12       -12             # Z4SS   -->  Z3SS   NUE    ANUE        
      5.19810017E-09    3      1000025        14       -14             # Z4SS   -->  Z3SS   NUM    ANUM        
      5.17668131E-09    3      1000025        16       -16             # Z4SS   -->  Z3SS   NUT    ANUT        
      1.36779115E-01    2      1000022        25                       # Z4SS   -->  Z1SS   HL0                
      2.48164982E-01    2      1000023        25                       # Z4SS   -->  Z2SS   HL0                
      6.67537563E-04    2      2000011       -11                       # Z4SS   -->  ER-    E+                 
      6.67537563E-04    2     -2000011        11                       # Z4SS   -->  ER+    E-                 
      6.67537563E-04    2      2000013       -13                       # Z4SS   -->  MUR-   MU+                
      6.67537563E-04    2     -2000013        13                       # Z4SS   -->  MUR+   MU-                
      1.55866332E-02    2      1000015       -15                       # Z4SS   -->  TAU1-  TAU+               
      1.55866332E-02    2     -1000015        15                       # Z4SS   -->  TAU1+  TAU-               
      8.52503506E-18    2      1000039        22                       # Z4SS   -->  GVSS   GM                 
      2.07459922E-19    3      1000039        11       -11             # Z4SS   -->  GVSS   E-     E+          
      5.18268014E-16    2      1000039        23                       # Z4SS   -->  GVSS   Z0                 
      3.31386712E-16    2      1000039        25                       # Z4SS   -->  GVSS   HL0                
#         PDG         Width
DECAY   1000024  4.61193062E-02   # W1SS+ decays
#          BR          NDA       ID1       ID2       ID3       ID4
      2.03952141E-06    3      1000022         2        -1             # W1SS+  -->  Z1SS   UP     DB          
      2.03952095E-06    3      1000022         4        -3             # W1SS+  -->  Z1SS   CH     SB          
      1.06639997E-03    3      1000022       -11        12             # W1SS+  -->  Z1SS   E+     NUE         
      1.06639997E-03    3      1000022       -13        14             # W1SS+  -->  Z1SS   MU+    NUM         
      1.10401679E-03    3      1000022       -15        16             # W1SS+  -->  Z1SS   TAU+   NUT         
      7.85970211E-01    2      1000022        24                       # W1SS+  -->  Z1SS   W+                 
      1.39684542E-13    3      1000023       -11        12             # W1SS+  -->  Z2SS   E+     NUE         
      1.39684542E-13    3      1000023       -13        14             # W1SS+  -->  Z2SS   MU+    NUM         
      2.10788935E-01    2     -1000015        16                       # W1SS+  -->  TAU1+  NUT                
#         PDG         Width
DECAY   1000037  4.39139080E+00   # W2SS+ decays
#          BR          NDA       ID1       ID2       ID3       ID4
      7.93044030E-09    3      1000022         2        -1             # W2SS+  -->  Z1SS   UP     DB          
      7.93043942E-09    3      1000022         4        -3             # W2SS+  -->  Z1SS   CH     SB          
      1.07896876E-05    3      1000022       -11        12             # W2SS+  -->  Z1SS   E+     NUE         
      1.07896876E-05    3      1000022       -13        14             # W2SS+  -->  Z1SS   MU+    NUM         
      5.86126680E-06    3      1000022       -15        16             # W2SS+  -->  Z1SS   TAU+   NUT         
      1.53648511E-01    2      1000022        24                       # W2SS+  -->  Z1SS   W+                 
      1.09793086E-09    3      1000023         2        -1             # W2SS+  -->  Z2SS   UP     DB          
      1.09793086E-09    3      1000023         4        -3             # W2SS+  -->  Z2SS   CH     SB          
      1.14783654E-06    3      1000023       -11        12             # W2SS+  -->  Z2SS   E+     NUE         
      1.14783654E-06    3      1000023       -13        14             # W2SS+  -->  Z2SS   MU+    NUM         
      1.13676811E-06    3      1000023       -15        16             # W2SS+  -->  Z2SS   TAU+   NUT         
      2.61380374E-01    2      1000023        24                       # W2SS+  -->  Z2SS   W+                 
      4.50926123E-08    3      1000025         2        -1             # W2SS+  -->  Z3SS   UP     DB          
      4.50926123E-08    3      1000025         4        -3             # W2SS+  -->  Z3SS   CH     SB          
      1.50169726E-08    3      1000025       -11        12             # W2SS+  -->  Z3SS   E+     NUE         
      1.50169726E-08    3      1000025       -13        14             # W2SS+  -->  Z3SS   MU+    NUM         
      1.50450035E-08    3      1000025       -15        16             # W2SS+  -->  Z3SS   TAU+   NUT         
      1.23721131E-14    3      1000035       -11        12             # W2SS+  -->  Z4SS   E+     NUE         
      1.23721131E-14    3      1000035       -13        14             # W2SS+  -->  Z4SS   MU+    NUM         
      3.33471000E-02    2     -1000015        16                       # W2SS+  -->  TAU1+  NUT                
      2.66831845E-01    2      1000024        23                       # W2SS+  -->  W1SS+  Z0                 
      1.72885872E-09    3      1000024         1        -1             # W2SS+  -->  W1SS+  DN     DB          
      1.72885761E-09    3      1000024         3        -3             # W2SS+  -->  W1SS+  ST     SB          
      2.51197707E-09    3      1000024         2        -2             # W2SS+  -->  W1SS+  UP     UB          
      2.51197707E-09    3      1000024         4        -4             # W2SS+  -->  W1SS+  CH     CB          
      1.73401668E-06    3      1000024        12       -12             # W2SS+  -->  W1SS+  NUE    ANUE        
      1.73401668E-06    3      1000024        14       -14             # W2SS+  -->  W1SS+  NUM    ANUM        
      1.60237175E-06    3      1000024        11       -11             # W2SS+  -->  W1SS+  E-     E+          
      1.60237175E-06    3      1000024        13       -13             # W2SS+  -->  W1SS+  MU-    MU+         
      2.84754366E-01    2      1000024        25                       # W2SS+  -->  W1SS+  HL0                
#         PDG         Width
DECAY        25  3.27489432E-03   # HL0   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      6.29845154E-09    2           11       -11                       # HL0    -->  E-     E+                 
      2.65930750E-04    2           13       -13                       # HL0    -->  MU-    MU+                
      7.60659948E-02    2           15       -15                       # HL0    -->  TAU-   TAU+               
      7.90065133E-06    2            1        -1                       # HL0    -->  DN     DB                 
      3.19225946E-03    2            3        -3                       # HL0    -->  ST     SB                 
      6.83629036E-01    2            5        -5                       # HL0    -->  BT     BB                 
      2.48912488E-06    2            2        -2                       # HL0    -->  UP     UB                 
      4.65816408E-02    2            4        -4                       # HL0    -->  CH     CB                 
      2.64787790E-03    2           22        22                       # HL0    -->  GM     GM                 
      5.51567525E-02    2           21        21                       # HL0    -->  GL     GL                 
      6.73486106E-03    3           24        11       -12             # HL0    -->  W+     E-     ANUE        
      6.73486106E-03    3           24        13       -14             # HL0    -->  W+     MU-    ANUM        
      6.73486106E-03    3           24        15       -16             # HL0    -->  W+     TAU-   ANUT        
      2.02045813E-02    3           24        -2         1             # HL0    -->  W+     UB     DN          
      2.02045813E-02    3           24        -4         3             # HL0    -->  W+     CB     ST          
      6.73486106E-03    3          -24       -11        12             # HL0    -->  W-     E+     NUE         
      6.73486106E-03    3          -24       -13        14             # HL0    -->  W-     MU+    NUM         
      6.73486106E-03    3          -24       -15        16             # HL0    -->  W-     TAU+   NUT         
      2.02045813E-02    3          -24         2        -1             # HL0    -->  W-     UP     DB          
      2.02045813E-02    3          -24         4        -3             # HL0    -->  W-     CH     SB          
      7.67501711E-04    3           23        12       -12             # HL0    -->  Z0     NUE    ANUE        
      7.67501711E-04    3           23        14       -14             # HL0    -->  Z0     NUM    ANUM        
      7.67501711E-04    3           23        16       -16             # HL0    -->  Z0     NUT    ANUT        
      3.86275904E-04    3           23        11       -11             # HL0    -->  Z0     E-     E+          
      3.86275904E-04    3           23        13       -13             # HL0    -->  Z0     MU-    MU+         
      3.86275904E-04    3           23        15       -15             # HL0    -->  Z0     TAU-   TAU+        
      1.32334861E-03    3           23         2        -2             # HL0    -->  Z0     UP     UB          
      1.32334861E-03    3           23         4        -4             # HL0    -->  Z0     CH     CB          
      1.70479901E-03    3           23         1        -1             # HL0    -->  Z0     DN     DB          
      1.70479901E-03    3           23         3        -3             # HL0    -->  Z0     ST     SB          
      1.70479901E-03    3           23         5        -5             # HL0    -->  Z0     BT     BB          
#         PDG         Width
DECAY        35  5.67022705E+00   # HH0   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      1.13767937E-08    2           11       -11                       # HH0    -->  E-     E+                 
      4.80348739E-04    2           13       -13                       # HH0    -->  MU-    MU+                
      1.37578323E-01    2           15       -15                       # HH0    -->  TAU-   TAU+               
      1.38799551E-05    2            1        -1                       # HH0    -->  DN     DB                 
      5.60820382E-03    2            3        -3                       # HH0    -->  ST     SB                 
      7.92674482E-01    2            5        -5                       # HH0    -->  BT     BB                 
      8.90946969E-11    2            2        -2                       # HH0    -->  UP     UB                 
      1.18240632E-06    2            4        -4                       # HH0    -->  CH     CB                 
      5.98782897E-02    2            6        -6                       # HH0    -->  TP     TB                 
      1.17206405E-07    2           22        22                       # HH0    -->  GM     GM                 
      1.58605981E-05    2           21        21                       # HH0    -->  GL     GL                 
      7.22902187E-05    2           24       -24                       # HH0    -->  W+     W-                 
      3.69617410E-05    2           23        23                       # HH0    -->  Z0     Z0                 
      7.21133198E-04    2      1000022   1000022                       # HH0    -->  Z1SS   Z1SS               
      2.65140692E-03    2      1000022   1000023                       # HH0    -->  Z1SS   Z2SS               
      2.50120735E-04    2           25        25                       # HH0    -->  HL0    HL0                
      6.37442372E-06    2      2000011  -2000011                       # HH0    -->  ER-    ER+                
      6.36632922E-06    2      2000013  -2000013                       # HH0    -->  MUR-   MUR+               
      4.59985404E-06    2      1000015  -1000015                       # HH0    -->  TAU1-  TAU1+              
#         PDG         Width
DECAY        36  5.69278908E+00   # HA0   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      1.12580816E-08    2           11       -11                       # HA0    -->  E-     E+                 
      4.75336536E-04    2           13       -13                       # HA0    -->  MU-    MU+                
      1.36143357E-01    2           15       -15                       # HA0    -->  TAU-   TAU+               
      1.37358029E-05    2            1        -1                       # HA0    -->  DN     DB                 
      5.54995984E-03    2            3        -3                       # HA0    -->  ST     SB                 
      7.84453809E-01    2            5        -5                       # HA0    -->  BT     BB                 
      8.68152494E-11    2            2        -2                       # HA0    -->  UP     UB                 
      1.15300656E-06    2            4        -4                       # HA0    -->  CH     CB                 
      6.04113825E-02    2            6        -6                       # HA0    -->  TP     TB                 
      3.93056553E-07    2           22        22                       # HA0    -->  GM     GM                 
      5.07805926E-05    2           21        21                       # HA0    -->  GL     GL                 
      1.24902348E-03    2      1000022   1000022                       # HA0    -->  Z1SS   Z1SS               
      1.15801916E-02    2      1000022   1000023                       # HA0    -->  Z1SS   Z2SS               
      7.07663057E-05    2           25        23                       # HA0    -->  HL0    Z0                 
#         PDG         Width
DECAY        37  5.27255106E+00   # H+    decays
#          BR          NDA       ID1       ID2       ID3       ID4
      1.22494423E-08    2           12       -11                       # H+     -->  NUE    E+                 
      5.17193577E-04    2           14       -13                       # H+     -->  NUM    MU+                
      1.48131832E-01    2           16       -15                       # H+     -->  NUT    TAU+               
      1.37933239E-05    2            2        -1                       # H+     -->  UP     DB                 
      5.57431532E-03    2            4        -3                       # H+     -->  CH     SB                 
      8.30716252E-01    2            6        -5                       # H+     -->  TP     BB                 
      1.49692735E-02    2      1000024   1000022                       # H+     -->  W1SS+  Z1SS               
      7.73567663E-05    2           25        24                       # H+     -->  HL0    W+                 
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

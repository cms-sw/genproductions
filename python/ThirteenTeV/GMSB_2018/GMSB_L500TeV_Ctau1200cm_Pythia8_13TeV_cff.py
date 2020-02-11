
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
     1     5.00000000E+05   # Lambda scale of soft SSB
     2     1.00000000E+06   # M_mess overall messenger scale
     3     1.50000000E+01   # tan(beta)
     4     1.00000000E+00   # sign(mu)
     5     1.00000000E+00   # N_5 messenger index
     6     9.78250000E+02   # c_grav gravitino mass factor
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
        25     1.20963837E+02   #  h^0            
        35     2.33982568E+03   #  H^0            
        36     2.32449658E+03   #  A^0            
        37     2.34119263E+03   #  H^+            
   1000001     4.97861523E+03   #  dnl            
   1000002     4.97796680E+03   #  upl            
   1000003     4.97861523E+03   #  stl            
   1000004     4.97796680E+03   #  chl            
   1000005     4.67874658E+03   #  b1             
   1000006     4.28783154E+03   #  t1             
   1000011     1.74556335E+03   #  el-            
   1000012     1.73479907E+03   #  nuel           
   1000013     1.74556335E+03   #  mul-           
   1000014     1.73479907E+03   #  numl           
   1000015     8.77231506E+02   #  tau1           
   1000016     1.72525610E+03   #  nutl           
   1000021     3.59506958E+03   #  glss           
   1000022     7.22850281E+02   #  z1ss           
   1000023     1.36930420E+03   #  z2ss           
   1000024     1.36947656E+03   #  w1ss           
   1000025    -1.61272607E+03   #  z3ss           
   1000035     1.62492908E+03   #  z4ss           
   1000037     1.62620276E+03   #  w2ss           
   1000039     1.17665186E-04   #  gvss
   2000001     4.69962061E+03   #  dnr            
   2000002     4.72713818E+03   #  upr            
   2000003     4.69962061E+03   #  str            
   2000004     4.72713867E+03   #  chr            
   2000005     4.79156592E+03   #  b2             
   2000006     4.82052734E+03   #  t2             
   2000011     8.74960999E+02   #  er-            
   2000013     8.74960999E+02   #  mur-           
   2000015     1.73672705E+03   #  tau2           
Block ALPHA   # Effective Higgs mixing parameter
         -6.68404102E-02   # alpha
Block STOPMIX   # stop mixing matrix
  1  1     2.02913228E-02   # O_{11}
  1  2     9.99794126E-01   # O_{12}
  2  1    -9.99794126E-01   # O_{21}
  2  2     2.02913228E-02   # O_{22}
Block SBOTMIX   # sbottom mixing matrix
  1  1     7.70947039E-02   # O_{11}
  1  2     9.97023761E-01   # O_{12}
  2  1    -9.97023761E-01   # O_{21}
  2  2     7.70947039E-02   # O_{22}
Block STAUMIX   # stau mixing matrix
  1  1     1.58321951E-02   # O_{11}
  1  2     9.99874651E-01   # O_{12}
  2  1    -9.99874651E-01   # O_{21}
  2  2     1.58321951E-02   # O_{22}
Block NMIX   # neutralino mixing matrix
  1  1     9.99309301E-01   #
  1  2    -1.99518353E-03   #
  1  3     3.34340036E-02   #
  1  4    -1.60968006E-02   #
  2  1    -9.81154572E-03   #
  2  2    -9.75596309E-01   #
  2  3     1.66011706E-01   #
  2  4    -1.43372744E-01   #
  3  1     1.22171640E-02   #
  3  2    -1.64305065E-02   #
  3  3    -7.06651509E-01   #
  3  4    -7.07265913E-01   #
  4  1    -3.36956233E-02   #
  4  2     2.18947381E-01   #
  4  3     6.86998308E-01   #
  4  4    -6.92069888E-01   #
Block UMIX   # chargino U mixing matrix
  1  1    -9.73944068E-01   # U_{11}
  1  2     2.26788387E-01   # U_{12}
  2  1    -2.26788387E-01   # U_{21}
  2  2    -9.73944068E-01   # U_{22}
Block VMIX   # chargino V mixing matrix
  1  1    -9.80740488E-01   # V_{11}
  1  2     1.95315510E-01   # V_{12}
  2  1    -1.95315510E-01   # V_{21}
  2  2    -9.80740488E-01   # V_{22}
Block GAUGE Q=  4.41147656E+03   #
     1     3.57515574E-01   # g`
     2     6.52412176E-01   # g_2
     3     1.21976912E+00   # g_3
Block YU Q=  4.41147656E+03   #
  3  3     8.23612988E-01   # y_t
Block YD Q=  4.41147656E+03   #
  3  3     1.79693878E-01   # y_b
Block YE Q=  4.41147656E+03   #
  3  3     1.53216481E-01   # y_tau
Block HMIX Q=  4.41147656E+03   # Higgs mixing parameters
     1     1.59305103E+03   # mu(Q)
     2     1.42569609E+01   # tan(beta)(Q)
     3     2.52204025E+02   # Higgs vev at Q
     4     5.40328450E+06   # m_A^2(Q)
Block MSOFT Q=  4.41147656E+03   # DRbar SUSY breaking parameters
     1     7.36200684E+02   # M_1(Q)          
     2     1.32917480E+03   # M_2(Q)          
     3     3.27734692E+03   # M_3(Q)          
    21     2.71649850E+06   # MHd^2(Q)        
    22    -2.20397425E+06   # MHu^2(Q)        
    31     1.73146716E+03   # MeL(Q)          
    32     1.73146716E+03   # MmuL(Q)         
    33     1.72214929E+03   # MtauL(Q)        
    34     8.74915039E+02   # MeR(Q)          
    35     8.74915039E+02   # MmuR(Q)         
    36     8.62024658E+02   # MtauR(Q)        
    41     4.81839209E+03   # MqL1(Q)         
    42     4.81839209E+03   # MqL2(Q)         
    43     4.64446826E+03   # MqL3(Q)         
    44     4.56409326E+03   # MuR(Q)          
    45     4.56409326E+03   # McR(Q)          
    46     4.19017236E+03   # MtR(Q)          
    47     4.53597314E+03   # MdR(Q)          
    48     4.53597314E+03   # MsR(Q)          
    49     4.52017188E+03   # MbR(Q)          
Block AU Q=  4.41147656E+03   #
  1  1    -9.46278625E+02   # A_u
  2  2    -9.46278625E+02   # A_c
  3  3    -9.46278625E+02   # A_t
Block AD Q=  4.41147656E+03   #
  1  1    -1.05144751E+03   # A_d
  2  2    -1.05144751E+03   # A_s
  3  3    -1.05144751E+03   # A_b
Block AE Q=  4.41147656E+03   #
  1  1    -1.31876114E+02   # A_e
  2  2    -1.31876114E+02   # A_mu
  3  3    -1.31876114E+02   # A_tau
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
DECAY   1000021  8.68305862E-02   # GLSS  decays
#          BR          NDA       ID1       ID2       ID3       ID4
      4.62337658E-02    3      1000024         1        -2             # GLSS   -->  W1SS+  DN     UB          
      4.62337658E-02    3     -1000024         2        -1             # GLSS   -->  W1SS-  UP     DB          
      4.62337658E-02    3      1000024         3        -4             # GLSS   -->  W1SS+  ST     CB          
      4.62337658E-02    3     -1000024         4        -3             # GLSS   -->  W1SS-  CH     SB          
      5.38903959E-02    3      1000024         5        -6             # GLSS   -->  W1SS+  BT     TB          
      5.38903959E-02    3     -1000024         6        -5             # GLSS   -->  W1SS-  TP     BB          
      1.50996190E-03    3      1000037         1        -2             # GLSS   -->  W2SS+  DN     UB          
      1.50996190E-03    3     -1000037         2        -1             # GLSS   -->  W2SS-  UP     DB          
      1.50996190E-03    3      1000037         3        -4             # GLSS   -->  W2SS+  ST     CB          
      1.50996190E-03    3     -1000037         4        -3             # GLSS   -->  W2SS-  CH     SB          
      1.04367949E-01    3      1000037         5        -6             # GLSS   -->  W2SS+  BT     TB          
      1.04367949E-01    3     -1000037         6        -5             # GLSS   -->  W2SS-  TP     BB          
      1.29011560E-05    2      1000022        21                       # GLSS   -->  Z1SS   GL                 
      3.13564092E-02    3      1000022         2        -2             # GLSS   -->  Z1SS   UP     UB          
      9.18567367E-03    3      1000022         1        -1             # GLSS   -->  Z1SS   DN     DB          
      9.18567367E-03    3      1000022         3        -3             # GLSS   -->  Z1SS   ST     SB          
      3.13563906E-02    3      1000022         4        -4             # GLSS   -->  Z1SS   CH     CB          
      9.73962154E-03    3      1000022         5        -5             # GLSS   -->  Z1SS   BT     BB          
      5.12548834E-02    3      1000022         6        -6             # GLSS   -->  Z1SS   TP     TB          
      8.66053379E-05    2      1000023        21                       # GLSS   -->  Z2SS   GL                 
      2.31342353E-02    3      1000023         2        -2             # GLSS   -->  Z2SS   UP     UB          
      2.29462497E-02    3      1000023         1        -1             # GLSS   -->  Z2SS   DN     DB          
      2.29462497E-02    3      1000023         3        -3             # GLSS   -->  Z2SS   ST     SB          
      2.31342353E-02    3      1000023         4        -4             # GLSS   -->  Z2SS   CH     CB          
      2.89335642E-02    3      1000023         5        -5             # GLSS   -->  Z2SS   BT     BB          
      2.51190588E-02    3      1000023         6        -6             # GLSS   -->  Z2SS   TP     TB          
      1.33392459E-03    2      1000025        21                       # GLSS   -->  Z3SS   GL                 
      2.32389334E-06    3      1000025         2        -2             # GLSS   -->  Z3SS   UP     UB          
      2.80408267E-06    3      1000025         1        -1             # GLSS   -->  Z3SS   DN     DB          
      2.80408267E-06    3      1000025         3        -3             # GLSS   -->  Z3SS   ST     SB          
      2.32389311E-06    3      1000025         4        -4             # GLSS   -->  Z3SS   CH     CB          
      3.98141053E-03    3      1000025         5        -5             # GLSS   -->  Z3SS   BT     BB          
      9.14951563E-02    3      1000025         6        -6             # GLSS   -->  Z3SS   TP     TB          
      1.26973237E-03    2      1000035        21                       # GLSS   -->  Z4SS   GL                 
      7.81447918E-04    3      1000035         2        -2             # GLSS   -->  Z4SS   UP     UB          
      8.62020184E-04    3      1000035         1        -1             # GLSS   -->  Z4SS   DN     DB          
      8.62020184E-04    3      1000035         3        -3             # GLSS   -->  Z4SS   ST     SB          
      7.81447918E-04    3      1000035         4        -4             # GLSS   -->  Z4SS   CH     CB          
      4.77397442E-03    3      1000035         5        -5             # GLSS   -->  Z4SS   BT     BB          
      9.79650915E-02    3      1000035         6        -6             # GLSS   -->  Z4SS   TP     TB          
      5.75114283E-13    2      1000039        21                       # GLSS   -->  GVSS   GL                 
#         PDG         Width
DECAY   1000002  1.14308014E+02   # UPL   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      5.76027064E-03    2      1000022         2                       # UPL    -->  Z1SS   UP                 
      1.49618506E-01    2      1000023         2                       # UPL    -->  Z2SS   UP                 
      2.95784539E-05    2      1000025         2                       # UPL    -->  Z3SS   UP                 
      6.62464788E-03    2      1000035         2                       # UPL    -->  Z4SS   UP                 
      5.25529683E-01    2      1000021         2                       # UPL    -->  GLSS   UP                 
      3.01277310E-01    2      1000024         1                       # UPL    -->  W1SS+  DN                 
      1.11600524E-02    2      1000037         1                       # UPL    -->  W2SS+  DN                 
#         PDG         Width
DECAY   1000001  1.14323929E+02   # DNL   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      6.01694640E-03    2      1000022         1                       # DNL    -->  Z1SS   DN                 
      1.48524866E-01    2      1000023         1                       # DNL    -->  Z2SS   DN                 
      5.11833059E-05    2      1000025         1                       # DNL    -->  Z3SS   DN                 
      7.41615752E-03    2      1000035         1                       # DNL    -->  Z4SS   DN                 
      5.25817454E-01    2      1000021         1                       # DNL    -->  GLSS   DN                 
      2.97126144E-01    2     -1000024         2                       # DNL    -->  W1SS-  UP                 
      1.50472615E-02    2     -1000037         2                       # DNL    -->  W2SS-  UP                 
#         PDG         Width
DECAY   1000003  1.14323921E+02   # STL   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      6.01694686E-03    2      1000022         3                       # STL    -->  Z1SS   ST                 
      1.48524880E-01    2      1000023         3                       # STL    -->  Z2SS   ST                 
      5.11833096E-05    2      1000025         3                       # STL    -->  Z3SS   ST                 
      7.41615798E-03    2      1000035         3                       # STL    -->  Z4SS   ST                 
      5.25817454E-01    2      1000021         3                       # STL    -->  GLSS   ST                 
      2.97126085E-01    2     -1000024         4                       # STL    -->  W1SS-  CH                 
      1.50472606E-02    2     -1000037         4                       # STL    -->  W2SS-  CH                 
#         PDG         Width
DECAY   1000004  1.14307983E+02   # CHL   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      5.76027157E-03    2      1000022         4                       # CHL    -->  Z1SS   CH                 
      1.49618536E-01    2      1000023         4                       # CHL    -->  Z2SS   CH                 
      2.95784575E-05    2      1000025         4                       # CHL    -->  Z3SS   CH                 
      6.62464881E-03    2      1000035         4                       # CHL    -->  Z4SS   CH                 
      5.25529563E-01    2      1000021         4                       # CHL    -->  GLSS   CH                 
      3.01277399E-01    2      1000024         3                       # CHL    -->  W1SS+  ST                 
      1.11600552E-02    2      1000037         3                       # CHL    -->  W2SS+  ST                 
#         PDG         Width
DECAY   1000005  4.95794106E+01   # BT1   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      5.10155894E-02    2      1000022         5                       # BT1    -->  Z1SS   BT                 
      6.63381396E-03    2      1000023         5                       # BT1    -->  Z2SS   BT                 
      2.43770368E-02    2      1000025         5                       # BT1    -->  Z3SS   BT                 
      2.02832837E-02    2      1000035         5                       # BT1    -->  Z4SS   BT                 
      8.39735508E-01    2      1000021         5                       # BT1    -->  GLSS   BT                 
      1.29529992E-02    2     -1000024         6                       # BT1    -->  W1SS-  TP                 
      4.49891165E-02    2     -1000037         6                       # BT1    -->  W2SS-  TP                 
      1.26570822E-05    2          -24   1000006                       # BT1    -->  W-     TP1                
#         PDG         Width
DECAY   1000006  1.09464081E+02   # TP1   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      1.79395169E-01    2      1000021         6                       # TP1    -->  GLSS   TP                 
      8.32933933E-02    2      1000022         6                       # TP1    -->  Z1SS   TP                 
      9.64419264E-03    2      1000023         6                       # TP1    -->  Z2SS   TP                 
      1.84550896E-01    2      1000025         6                       # TP1    -->  Z3SS   TP                 
      1.74011007E-01    2      1000035         6                       # TP1    -->  Z4SS   TP                 
      1.80708487E-02    2      1000024         5                       # TP1    -->  W1SS+  BT                 
      3.51034433E-01    2      1000037         5                       # TP1    -->  W2SS+  BT                 
#         PDG         Width
DECAY   2000002  5.46927567E+01   # UPR   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      1.86094329E-01    2      1000022         2                       # UPR    -->  Z1SS   UP                 
      1.57847553E-05    2      1000023         2                       # UPR    -->  Z2SS   UP                 
      2.27690271E-05    2      1000025         2                       # UPR    -->  Z3SS   UP                 
      1.72508648E-04    2      1000035         2                       # UPR    -->  Z4SS   UP                 
      8.13694596E-01    2      1000021         2                       # UPR    -->  GLSS   UP                 
#         PDG         Width
DECAY   2000001  4.53831749E+01   # DNR   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      5.57093807E-02    2      1000022         1                       # DNR    -->  Z1SS   DN                 
      4.71783096E-06    2      1000023         1                       # DNR    -->  Z2SS   DN                 
      6.79890718E-06    2      1000025         1                       # DNR    -->  Z3SS   DN                 
      5.15089087E-05    2      1000035         1                       # DNR    -->  Z4SS   DN                 
      9.44227636E-01    2      1000021         1                       # DNR    -->  GLSS   DN                 
#         PDG         Width
DECAY   2000003  4.53831749E+01   # STR   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      5.57093807E-02    2      1000022         3                       # STR    -->  Z1SS   ST                 
      4.71783096E-06    2      1000023         3                       # STR    -->  Z2SS   ST                 
      6.79890718E-06    2      1000025         3                       # STR    -->  Z3SS   ST                 
      5.15089087E-05    2      1000035         3                       # STR    -->  Z4SS   ST                 
      9.44227636E-01    2      1000021         3                       # STR    -->  GLSS   ST                 
#         PDG         Width
DECAY   2000004  5.46927414E+01   # CHR   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      1.86094388E-01    2      1000022         4                       # CHR    -->  Z1SS   CH                 
      1.57847589E-05    2      1000023         4                       # CHR    -->  Z2SS   CH                 
      2.27690271E-05    2      1000025         4                       # CHR    -->  Z3SS   CH                 
      1.72508720E-04    2      1000035         4                       # CHR    -->  Z4SS   CH                 
      8.13694537E-01    2      1000021         4                       # CHR    -->  GLSS   CH                 
#         PDG         Width
DECAY   2000005  1.50105927E+02   # BT2   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      4.34915954E-03    2      1000022         5                       # BT2    -->  Z1SS   BT                 
      1.06184937E-01    2      1000023         5                       # BT2    -->  Z2SS   BT                 
      8.59377626E-03    2      1000025         5                       # BT2    -->  Z3SS   BT                 
      1.43277599E-02    2      1000035         5                       # BT2    -->  Z4SS   BT                 
      3.22379410E-01    2      1000021         5                       # BT2    -->  GLSS   BT                 
      2.21527189E-01    2     -1000024         6                       # BT2    -->  W1SS-  TP                 
      3.21123898E-01    2     -1000037         6                       # BT2    -->  W2SS-  TP                 
      1.48442271E-03    2          -24   1000006                       # BT2    -->  W-     TP1                
      2.94006004E-05    2           23   1000005                       # BT2    -->  Z0     BT1                
#         PDG         Width
DECAY   2000006  1.52893723E+02   # TP2   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      3.22430015E-01    2      1000021         6                       # TP2    -->  GLSS   TP                 
      2.14427546E-01    2      1000024         5                       # TP2    -->  W1SS+  BT                 
      2.59258728E-02    2      1000037         5                       # TP2    -->  W2SS+  BT                 
      8.65389244E-04    2           23   1000006                       # TP2    -->  Z0     TP1                
      2.97915353E-03    2           25   1000006                       # TP2    -->  HL0    TP1                
      3.08037765E-04    2           24   1000005                       # TP2    -->  W+     BT1                
      4.20037098E-03    2      1000022         6                       # TP2    -->  Z1SS   TP                 
      1.11457132E-01    2      1000023         6                       # TP2    -->  Z2SS   TP                 
      1.58510298E-01    2      1000025         6                       # TP2    -->  Z3SS   TP                 
      1.58896253E-01    2      1000035         6                       # TP2    -->  Z4SS   TP                 
#         PDG         Width
DECAY   1000011  4.63572931E+00   # EL-   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      3.25833857E-01    2      1000022        11                       # EL-    -->  Z1SS   E-                 
      2.25667387E-01    2      1000023        11                       # EL-    -->  Z2SS   E-                 
      3.20709046E-06    2      1000025        11                       # EL-    -->  Z3SS   E-                 
      1.13378419E-03    2      1000035        11                       # EL-    -->  Z4SS   E-                 
      4.44517344E-01    2     -1000024        12                       # EL-    -->  W1SS-  NUE                
      2.84443167E-03    2     -1000037        12                       # EL-    -->  W2SS-  NUE                
      2.90703587E-16    2           11   1000039                       # EL-    -->  E-     GVSS               
#         PDG         Width
DECAY   1000013  4.63572931E+00   # MUL-  decays
#          BR          NDA       ID1       ID2       ID3       ID4
      3.25833857E-01    2      1000022        13                       # MUL-   -->  Z1SS   MU-                
      2.25667387E-01    2      1000023        13                       # MUL-   -->  Z2SS   MU-                
      3.20708955E-06    2      1000025        13                       # MUL-   -->  Z3SS   MU-                
      1.13378372E-03    2      1000035        13                       # MUL-   -->  Z4SS   MU-                
      4.44517344E-01    2     -1000024        14                       # MUL-   -->  W1SS-  NUM                
      2.84443167E-03    2     -1000037        14                       # MUL-   -->  W2SS-  NUM                
      2.90703587E-16    2           13   1000039                       # MUL-   -->  MU-    GVSS               
#         PDG         Width
DECAY   1000015  4.59101647E-01   # TAU1- decays
#          BR          NDA       ID1       ID2       ID3       ID4
      1.00000000E+00    2      1000022        15                       # TAU1-  -->  Z1SS   TAU-               
      9.40904381E-17    2           15   1000039                       # TAU1-  -->  TAU-   GVSS               
#         PDG         Width
DECAY   1000012  4.50129700E+00   # NUEL  decays
#          BR          NDA       ID1       ID2       ID3       ID4
      3.36635560E-01    2      1000022        12                       # NUEL   -->  Z1SS   NUE                
      2.17013910E-01    2      1000023        12                       # NUEL   -->  Z2SS   NUE                
      1.60228446E-05    2      1000025        12                       # NUEL   -->  Z3SS   NUE                
      1.37625961E-03    2      1000035        12                       # NUEL   -->  Z4SS   NUE                
      4.43137556E-01    2      1000024        11                       # NUEL   -->  W1SS+  E-                 
      1.82052923E-03    2      1000037        11                       # NUEL   -->  W2SS+  E-                 
      2.90267630E-16    2           12   1000039                       # NUEL   -->  NUE    GVSS               
#         PDG         Width
DECAY   1000014  4.50129652E+00   # NUML  decays
#          BR          NDA       ID1       ID2       ID3       ID4
      3.36635590E-01    2      1000022        14                       # NUML   -->  Z1SS   NUM                
      2.17013940E-01    2      1000023        14                       # NUML   -->  Z2SS   NUM                
      1.60228465E-05    2      1000025        14                       # NUML   -->  Z3SS   NUM                
      1.37625972E-03    2      1000035        14                       # NUML   -->  Z4SS   NUM                
      4.43137586E-01    2      1000024        13                       # NUML   -->  W1SS+  MU-                
      1.82052853E-03    2      1000037        13                       # NUML   -->  W2SS+  MU-                
      2.90267656E-16    2           14   1000039                       # NUML   -->  NUM    GVSS               
#         PDG         Width
DECAY   1000016  4.71154881E+00   # NUTL  decays
#          BR          NDA       ID1       ID2       ID3       ID4
      3.18354905E-01    2      1000022        16                       # NUTL   -->  Z1SS   NUT                
      1.98698238E-01    2      1000023        16                       # NUTL   -->  Z2SS   NUT                
      1.31497764E-05    2      1000025        16                       # NUTL   -->  Z3SS   NUT                
      1.10832800E-03    2      1000035        16                       # NUTL   -->  Z4SS   NUT                
      4.06784296E-01    2      1000024        15                       # NUTL   -->  W1SS+  TAU-               
      3.64203425E-03    2      1000037        15                       # NUTL   -->  W2SS+  TAU-               
      7.13991448E-02    2           24   1000015                       # NUTL   -->  W+     TAU1-              
      2.69770570E-16    2           16   1000039                       # NUTL   -->  NUT    GVSS               
#         PDG         Width
DECAY   2000011  4.47922766E-01   # ER-   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      1.00000000E+00    2      1000022        11                       # ER-    -->  Z1SS   E-                 
      9.51986219E-17    2           11   1000039                       # ER-    -->  E-     GVSS               
#         PDG         Width
DECAY   2000013  4.47922617E-01   # MUR-  decays
#          BR          NDA       ID1       ID2       ID3       ID4
      1.00000000E+00    2      1000022        13                       # MUR-   -->  Z1SS   MU-                
      9.51986550E-17    2           13   1000039                       # MUR-   -->  MU-    GVSS               
#         PDG         Width
DECAY   2000015  4.94278526E+00   # TAU2- decays
#          BR          NDA       ID1       ID2       ID3       ID4
      3.02915215E-01    2      1000022        15                       # TAU2-  -->  Z1SS   TAU-               
      2.03916073E-01    2      1000023        15                       # TAU2-  -->  Z2SS   TAU-               
      1.63616333E-03    2      1000025        15                       # TAU2-  -->  Z3SS   TAU-               
      2.25865375E-03    2      1000035        15                       # TAU2-  -->  Z4SS   TAU-               
      4.00540292E-01    2     -1000024        16                       # TAU2-  -->  W1SS-  NUT                
      2.38614902E-03    2     -1000037        16                       # TAU2-  -->  W2SS-  NUT                
      3.55129205E-02    2           23   1000015                       # TAU2-  -->  Z0     TAU1-              
      5.08345105E-02    2           25   1000015                       # TAU2-  -->  HL0    TAU1-              
#         PDG         Width
DECAY   1000022  1.64491995E-17   # Z1SS  decays
#          BR          NDA       ID1       ID2       ID3       ID4
      7.63475120E-01    2      1000039        22                       # Z1SS   -->  GVSS   GM                 
      1.79258157E-02    3      1000039        11       -11             # Z1SS   -->  GVSS   E-     E+          
      2.18513936E-01    2      1000039        23                       # Z1SS   -->  GVSS   Z0                 
      8.51374789E-05    2      1000039        25                       # Z1SS   -->  GVSS   HL0                
#         PDG         Width
DECAY   1000023  6.47801831E-02   # Z2SS  decays
#          BR          NDA       ID1       ID2       ID3       ID4
      1.51884387E-06    2      1000022        22                       # Z2SS   -->  Z1SS   GM                 
      5.29290959E-02    2      1000022        23                       # Z2SS   -->  Z1SS   Z0                 
      1.09971518E-06    3      1000022         2        -2             # Z2SS   -->  Z1SS   UP     UB          
      1.13848353E-06    3      1000022         1        -1             # Z2SS   -->  Z1SS   DN     DB          
      1.13848353E-06    3      1000022         3        -3             # Z2SS   -->  Z1SS   ST     SB          
      1.09971518E-06    3      1000022         4        -4             # Z2SS   -->  Z1SS   CH     CB          
      2.76562628E-06    3      1000022         5        -5             # Z2SS   -->  Z1SS   BT     BB          
      5.33444283E-04    3      1000022        11       -11             # Z2SS   -->  Z1SS   E-     E+          
      5.33444283E-04    3      1000022        13       -13             # Z2SS   -->  Z1SS   MU-    MU+         
      5.51571313E-04    3      1000022        15       -15             # Z2SS   -->  Z1SS   TAU-   TAU+        
      5.51783713E-04    3      1000022        12       -12             # Z2SS   -->  Z1SS   NUE    ANUE        
      5.51783713E-04    3      1000022        14       -14             # Z2SS   -->  Z1SS   NUM    ANUM        
      5.72720019E-04    3      1000022        16       -16             # Z2SS   -->  Z1SS   NUT    ANUT        
      7.72540212E-01    2      1000022        25                       # Z2SS   -->  Z1SS   HL0                
      1.81178725E-03    2      2000011       -11                       # Z2SS   -->  ER-    E+                 
      1.81178725E-03    2     -2000011        11                       # Z2SS   -->  ER+    E-                 
      1.81178725E-03    2      2000013       -13                       # Z2SS   -->  MUR-   MU+                
      1.81178725E-03    2     -2000013        13                       # Z2SS   -->  MUR+   MU-                
      8.19899961E-02    2      1000015       -15                       # Z2SS   -->  TAU1-  TAU+               
      8.19899961E-02    2     -1000015        15                       # Z2SS   -->  TAU1+  TAU-               
      1.41490078E-15    2      1000039        22                       # Z2SS   -->  GVSS   GM                 
      3.47193677E-17    3      1000039        11       -11             # Z2SS   -->  GVSS   E-     E+          
      4.46058477E-15    2      1000039        23                       # Z2SS   -->  GVSS   Z0                 
      5.21462037E-17    2      1000039        25                       # Z2SS   -->  GVSS   HL0                
#         PDG         Width
DECAY   1000025  4.55006266E+00   # Z3SS  decays
#          BR          NDA       ID1       ID2       ID3       ID4
      1.31105821E-07    2      1000022        22                       # Z3SS   -->  Z1SS   GM                 
      1.44819921E-07    2      1000023        22                       # Z3SS   -->  Z2SS   GM                 
      2.46137574E-01    2      1000024       -24                       # Z3SS   -->  W1SS+  W-                 
      2.46137574E-01    2     -1000024        24                       # Z3SS   -->  W1SS-  W+                 
      1.87547490E-01    2      1000022        23                       # Z3SS   -->  Z1SS   Z0                 
      2.52311766E-01    2      1000023        23                       # Z3SS   -->  Z2SS   Z0                 
      4.97181150E-11    3      1000022         2        -2             # Z3SS   -->  Z1SS   UP     UB          
      1.26815962E-11    3      1000022         1        -1             # Z3SS   -->  Z1SS   DN     DB          
      1.26815962E-11    3      1000022         3        -3             # Z3SS   -->  Z1SS   ST     SB          
      4.97180977E-11    3      1000022         4        -4             # Z3SS   -->  Z1SS   CH     CB          
      8.70761028E-07    3      1000022         5        -5             # Z3SS   -->  Z1SS   BT     BB          
      2.53470489E-09    3      1000022        11       -11             # Z3SS   -->  Z1SS   E-     E+          
      2.53470489E-09    3      1000022        13       -13             # Z3SS   -->  Z1SS   MU-    MU+         
      1.99515966E-06    3      1000022        15       -15             # Z3SS   -->  Z1SS   TAU-   TAU+        
      1.56548055E-08    3      1000022        12       -12             # Z3SS   -->  Z1SS   NUE    ANUE        
      1.56548055E-08    3      1000022        14       -14             # Z3SS   -->  Z1SS   NUM    ANUM        
      1.66999463E-08    3      1000022        16       -16             # Z3SS   -->  Z1SS   NUT    ANUT        
      5.08758021E-13    3      1000023         2        -2             # Z3SS   -->  Z2SS   UP     UB          
      8.73334391E-13    3      1000023         1        -1             # Z3SS   -->  Z2SS   DN     DB          
      8.73334391E-13    3      1000023         3        -3             # Z3SS   -->  Z2SS   ST     SB          
      5.08758021E-13    3      1000023         4        -4             # Z3SS   -->  Z2SS   CH     CB          
      8.05588130E-09    3      1000023         5        -5             # Z3SS   -->  Z2SS   BT     BB          
      7.19554624E-11    3      1000023        11       -11             # Z3SS   -->  Z2SS   E-     E+          
      7.19554624E-11    3      1000023        13       -13             # Z3SS   -->  Z2SS   MU-    MU+         
      1.02007419E-07    3      1000023        15       -15             # Z3SS   -->  Z2SS   TAU-   TAU+        
      4.45968401E-10    3      1000023        12       -12             # Z3SS   -->  Z2SS   NUE    ANUE        
      4.45968401E-10    3      1000023        14       -14             # Z3SS   -->  Z2SS   NUM    ANUM        
      4.94453700E-10    3      1000023        16       -16             # Z3SS   -->  Z2SS   NUT    ANUT        
      2.32765023E-02    2      1000022        25                       # Z3SS   -->  Z1SS   HL0                
      1.27826596E-03    2      1000023        25                       # Z3SS   -->  Z2SS   HL0                
      6.69943329E-05    2      2000011       -11                       # Z3SS   -->  ER-    E+                 
      6.69943329E-05    2     -2000011        11                       # Z3SS   -->  ER+    E-                 
      6.69943329E-05    2      2000013       -13                       # Z3SS   -->  MUR-   MU+                
      6.69943329E-05    2     -2000013        13                       # Z3SS   -->  MUR+   MU-                
      2.15199906E-02    2      1000015       -15                       # Z3SS   -->  TAU1-  TAU+               
      2.15199906E-02    2     -1000015        15                       # Z3SS   -->  TAU1+  TAU-               
      1.55487589E-21    2      1000039        22                       # Z3SS   -->  GVSS   GM                 
      3.85759104E-23    3      1000039        11       -11             # Z3SS   -->  GVSS   E-     E+          
      4.27834026E-17    2      1000039        23                       # Z3SS   -->  GVSS   Z0                 
      5.52461358E-17    2      1000039        25                       # Z3SS   -->  GVSS   HL0                
#         PDG         Width
DECAY   1000035  5.46985912E+00   # Z4SS  decays
#          BR          NDA       ID1       ID2       ID3       ID4
      2.45756127E-08    2      1000022        22                       # Z4SS   -->  Z1SS   GM                 
      3.05220986E-08    2      1000023        22                       # Z4SS   -->  Z2SS   GM                 
      1.32293912E-10    2      1000025        22                       # Z4SS   -->  Z3SS   GM                 
      2.64156073E-01    2      1000024       -24                       # Z4SS   -->  W1SS+  W-                 
      2.64156073E-01    2     -1000024        24                       # Z4SS   -->  W1SS-  W+                 
      1.89151037E-02    2      1000022        23                       # Z4SS   -->  Z1SS   Z0                 
      1.62043236E-03    2      1000023        23                       # Z4SS   -->  Z2SS   Z0                 
      3.47367934E-09    3      1000022         2        -2             # Z4SS   -->  Z1SS   UP     UB          
      3.23292926E-09    3      1000022         1        -1             # Z4SS   -->  Z1SS   DN     DB          
      3.23292926E-09    3      1000022         3        -3             # Z4SS   -->  Z1SS   ST     SB          
      3.47367890E-09    3      1000022         4        -4             # Z4SS   -->  Z1SS   CH     CB          
      6.89211788E-07    3      1000022         5        -5             # Z4SS   -->  Z1SS   BT     BB          
      1.91832100E-06    3      1000022        11       -11             # Z4SS   -->  Z1SS   E-     E+          
      1.91832100E-06    3      1000022        13       -13             # Z4SS   -->  Z1SS   MU-    MU+         
      3.86848205E-06    3      1000022        15       -15             # Z4SS   -->  Z1SS   TAU-   TAU+        
      2.90643720E-06    3      1000022        12       -12             # Z4SS   -->  Z1SS   NUE    ANUE        
      2.90643720E-06    3      1000022        14       -14             # Z4SS   -->  Z1SS   NUM    ANUM        
      3.07654636E-06    3      1000022        16       -16             # Z4SS   -->  Z1SS   NUT    ANUT        
      3.57983559E-10    3      1000023         2        -2             # Z4SS   -->  Z2SS   UP     UB          
      3.97563316E-10    3      1000023         1        -1             # Z4SS   -->  Z2SS   DN     DB          
      3.97563316E-10    3      1000023         3        -3             # Z4SS   -->  Z2SS   ST     SB          
      3.57983559E-10    3      1000023         4        -4             # Z4SS   -->  Z2SS   CH     CB          
      7.47458184E-09    3      1000023         5        -5             # Z4SS   -->  Z2SS   BT     BB          
      9.17663456E-08    3      1000023        11       -11             # Z4SS   -->  Z2SS   E-     E+          
      9.17663456E-08    3      1000023        13       -13             # Z4SS   -->  Z2SS   MU-    MU+         
      2.14685855E-07    3      1000023        15       -15             # Z4SS   -->  Z2SS   TAU-   TAU+        
      1.39999415E-07    3      1000023        12       -12             # Z4SS   -->  Z2SS   NUE    ANUE        
      1.39999415E-07    3      1000023        14       -14             # Z4SS   -->  Z2SS   NUM    ANUM        
      1.54462825E-07    3      1000023        16       -16             # Z4SS   -->  Z2SS   NUT    ANUT        
      5.98884276E-09    3      1000025         2        -2             # Z4SS   -->  Z3SS   UP     UB          
      7.72269182E-09    3      1000025         1        -1             # Z4SS   -->  Z3SS   DN     DB          
      7.72269182E-09    3      1000025         3        -3             # Z4SS   -->  Z3SS   ST     SB          
      5.98884276E-09    3      1000025         4        -4             # Z4SS   -->  Z3SS   CH     CB          
      6.67879807E-10    3      1000025         5        -5             # Z4SS   -->  Z3SS   BT     BB          
      1.75199422E-09    3      1000025        11       -11             # Z4SS   -->  Z3SS   E-     E+          
      1.75199422E-09    3      1000025        13       -13             # Z4SS   -->  Z3SS   MU-    MU+         
      1.35863532E-09    3      1000025        15       -15             # Z4SS   -->  Z3SS   TAU-   TAU+        
      3.48505869E-09    3      1000025        12       -12             # Z4SS   -->  Z3SS   NUE    ANUE        
      3.48505869E-09    3      1000025        14       -14             # Z4SS   -->  Z3SS   NUM    ANUM        
      3.48499962E-09    3      1000025        16       -16             # Z4SS   -->  Z3SS   NUT    ANUT        
      1.66654408E-01    2      1000022        25                       # Z4SS   -->  Z1SS   HL0                
      2.48504817E-01    2      1000023        25                       # Z4SS   -->  Z2SS   HL0                
      4.32477711E-04    2      2000011       -11                       # Z4SS   -->  ER-    E+                 
      4.32477711E-04    2     -2000011        11                       # Z4SS   -->  ER+    E-                 
      4.32477711E-04    2      2000013       -13                       # Z4SS   -->  MUR-   MU+                
      4.32477711E-04    2     -2000013        13                       # Z4SS   -->  MUR+   MU-                
      1.71224866E-02    2      1000015       -15                       # Z4SS   -->  TAU1-  TAU+               
      1.71224866E-02    2     -1000015        15                       # Z4SS   -->  TAU1+  TAU-               
      9.92908420E-19    2      1000039        22                       # Z4SS   -->  GVSS   GM                 
      2.46461065E-20    3      1000039        11       -11             # Z4SS   -->  GVSS   E-     E+          
      5.34557067E-17    2      1000039        23                       # Z4SS   -->  GVSS   Z0                 
      3.49974492E-17    2      1000039        25                       # Z4SS   -->  GVSS   HL0                
#         PDG         Width
DECAY   1000024  5.37228398E-02   # W1SS+ decays
#          BR          NDA       ID1       ID2       ID3       ID4
      2.70995679E-06    3      1000022         2        -1             # W1SS+  -->  Z1SS   UP     DB          
      2.70995679E-06    3      1000022         4        -3             # W1SS+  -->  Z1SS   CH     SB          
      1.31557952E-03    3      1000022       -11        12             # W1SS+  -->  Z1SS   E+     NUE         
      1.31557952E-03    3      1000022       -13        14             # W1SS+  -->  Z1SS   MU+    NUM         
      1.36305287E-03    3      1000022       -15        16             # W1SS+  -->  Z1SS   TAU+   NUT         
      8.12415600E-01    2      1000022        24                       # W1SS+  -->  Z1SS   W+                 
      8.53304796E-18    3      1000023       -11        12             # W1SS+  -->  Z2SS   E+     NUE         
      8.53304796E-18    3      1000023       -13        14             # W1SS+  -->  Z2SS   MU+    NUM         
      1.83584750E-01    2     -1000015        16                       # W1SS+  -->  TAU1+  NUT                
#         PDG         Width
DECAY   1000037  4.87808514E+00   # W2SS+ decays
#          BR          NDA       ID1       ID2       ID3       ID4
      6.21149754E-09    3      1000022         2        -1             # W2SS+  -->  Z1SS   UP     DB          
      6.21149754E-09    3      1000022         4        -3             # W2SS+  -->  Z1SS   CH     SB          
      5.00140595E-06    3      1000022       -11        12             # W2SS+  -->  Z1SS   E+     NUE         
      5.00140595E-06    3      1000022       -13        14             # W2SS+  -->  Z1SS   MU+    NUM         
      7.58720262E-06    3      1000022       -15        16             # W2SS+  -->  Z1SS   TAU+   NUT         
      1.83986947E-01    2      1000022        24                       # W2SS+  -->  Z1SS   W+                 
      2.75353351E-10    3      1000023         2        -1             # W2SS+  -->  Z2SS   UP     DB          
      2.75353351E-10    3      1000023         4        -3             # W2SS+  -->  Z2SS   CH     SB          
      9.38854967E-08    3      1000023       -11        12             # W2SS+  -->  Z2SS   E+     NUE         
      9.38854967E-08    3      1000023       -13        14             # W2SS+  -->  Z2SS   MU+    NUM         
      2.38884525E-07    3      1000023       -15        16             # W2SS+  -->  Z2SS   TAU+   NUT         
      2.39771843E-01    2      1000023        24                       # W2SS+  -->  Z2SS   W+                 
      3.78574860E-08    3      1000025         2        -1             # W2SS+  -->  Z3SS   UP     DB          
      3.78574860E-08    3      1000025         4        -3             # W2SS+  -->  Z3SS   CH     SB          
      1.26186341E-08    3      1000025       -11        12             # W2SS+  -->  Z3SS   E+     NUE         
      1.26186341E-08    3      1000025       -13        14             # W2SS+  -->  Z3SS   MU+    NUM         
      1.26187842E-08    3      1000025       -15        16             # W2SS+  -->  Z3SS   TAU+   NUT         
      2.97163336E-13    3      1000035         2        -1             # W2SS+  -->  Z4SS   UP     DB          
      9.92110379E-14    3      1000035       -11        12             # W2SS+  -->  Z4SS   E+     NUE         
      9.92110379E-14    3      1000035       -13        14             # W2SS+  -->  Z4SS   MU+    NUM         
      3.77806313E-02    2     -1000015        16                       # W2SS+  -->  TAU1+  NUT                
      2.50494421E-01    2      1000024        23                       # W2SS+  -->  W1SS+  Z0                 
      4.64719235E-10    3      1000024         1        -1             # W2SS+  -->  W1SS+  DN     DB          
      4.64719235E-10    3      1000024         3        -3             # W2SS+  -->  W1SS+  ST     SB          
      6.17546625E-10    3      1000024         2        -2             # W2SS+  -->  W1SS+  UP     UB          
      6.17546625E-10    3      1000024         4        -4             # W2SS+  -->  W1SS+  CH     CB          
      1.84072505E-07    3      1000024        12       -12             # W2SS+  -->  W1SS+  NUE    ANUE        
      1.84072505E-07    3      1000024        14       -14             # W2SS+  -->  W1SS+  NUM    ANUM        
      1.54359057E-07    3      1000024        11       -11             # W2SS+  -->  W1SS+  E-     E+          
      1.54359057E-07    3      1000024        13       -13             # W2SS+  -->  W1SS+  MU-    MU+         
      1.70915968E-07    3      1000024        15       -15             # W2SS+  -->  W1SS+  TAU-   TAU+        
      2.87947237E-01    2      1000024        25                       # W2SS+  -->  W1SS+  HL0                
#         PDG         Width
DECAY        25  3.38289002E-03   # HL0   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      6.12632656E-09    2           11       -11                       # HL0    -->  E-     E+                 
      2.58663378E-04    2           13       -13                       # HL0    -->  MU-    MU+                
      7.39895925E-02    2           15       -15                       # HL0    -->  TAU-   TAU+               
      7.68337031E-06    2            1        -1                       # HL0    -->  DN     DB                 
      3.10446718E-03    2            3        -3                       # HL0    -->  ST     SB                 
      6.64919317E-01    2            5        -5                       # HL0    -->  BT     BB                 
      2.43838622E-06    2            2        -2                       # HL0    -->  UP     UB                 
      4.55486365E-02    2            4        -4                       # HL0    -->  CH     CB                 
      2.68818764E-03    2           22        22                       # HL0    -->  GM     GM                 
      5.55850491E-02    2           21        21                       # HL0    -->  GL     GL                 
      7.78724020E-03    3           24        11       -12             # HL0    -->  W+     E-     ANUE        
      7.78724020E-03    3           24        13       -14             # HL0    -->  W+     MU-    ANUM        
      7.78724020E-03    3           24        15       -16             # HL0    -->  W+     TAU-   ANUT        
      2.33617220E-02    3           24        -2         1             # HL0    -->  W+     UB     DN          
      2.33617220E-02    3           24        -4         3             # HL0    -->  W+     CB     ST          
      7.78724020E-03    3          -24       -11        12             # HL0    -->  W-     E+     NUE         
      7.78724020E-03    3          -24       -13        14             # HL0    -->  W-     MU+    NUM         
      7.78724020E-03    3          -24       -15        16             # HL0    -->  W-     TAU+   NUT         
      2.33617220E-02    3          -24         2        -1             # HL0    -->  W-     UP     DB          
      2.33617220E-02    3          -24         4        -3             # HL0    -->  W-     CH     SB          
      9.38703422E-04    3           23        12       -12             # HL0    -->  Z0     NUE    ANUE        
      9.38703422E-04    3           23        14       -14             # HL0    -->  Z0     NUM    ANUM        
      9.38703422E-04    3           23        16       -16             # HL0    -->  Z0     NUT    ANUT        
      4.72440035E-04    3           23        11       -11             # HL0    -->  Z0     E-     E+          
      4.72440035E-04    3           23        13       -13             # HL0    -->  Z0     MU-    MU+         
      4.72440035E-04    3           23        15       -15             # HL0    -->  Z0     TAU-   TAU+        
      1.61853945E-03    3           23         2        -2             # HL0    -->  Z0     UP     UB          
      1.61853945E-03    3           23         4        -4             # HL0    -->  Z0     CH     CB          
      2.08507758E-03    3           23         1        -1             # HL0    -->  Z0     DN     DB          
      2.08507758E-03    3           23         3        -3             # HL0    -->  Z0     ST     SB          
      2.08507758E-03    3           23         5        -5             # HL0    -->  Z0     BT     BB          
#         PDG         Width
DECAY        35  7.69684315E+00   # HH0   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      1.16233236E-08    2           11       -11                       # HH0    -->  E-     E+                 
      4.90757695E-04    2           13       -13                       # HH0    -->  MU-    MU+                
      1.40560046E-01    2           15       -15                       # HH0    -->  TAU-   TAU+               
      1.41470109E-05    2            1        -1                       # HH0    -->  DN     DB                 
      5.71610872E-03    2            3        -3                       # HH0    -->  ST     SB                 
      7.80803204E-01    2            5        -5                       # HH0    -->  BT     BB                 
      9.01491173E-11    2            2        -2                       # HH0    -->  UP     UB                 
      1.15403418E-06    2            4        -4                       # HH0    -->  CH     CB                 
      5.98319769E-02    2            6        -6                       # HH0    -->  TP     TB                 
      8.35925889E-08    2           22        22                       # HH0    -->  GM     GM                 
      1.09686289E-05    2           21        21                       # HH0    -->  GL     GL                 
      3.98430493E-05    2           24       -24                       # HH0    -->  W+     W-                 
      2.04096523E-05    2           23        23                       # HH0    -->  Z0     Z0                 
      3.94009112E-04    2      1000022   1000022                       # HH0    -->  Z1SS   Z1SS               
      1.52045733E-03    2      1000022   1000023                       # HH0    -->  Z1SS   Z2SS               
      1.04495510E-02    2      1000022   1000025                       # HH0    -->  Z1SS   Z3SS               
      1.38539312E-04    2           25        25                       # HH0    -->  HL0    HL0                
      3.24044777E-06    2      2000011  -2000011                       # HH0    -->  ER-    ER+                
      3.23632548E-06    2      2000013  -2000013                       # HH0    -->  MUR-   MUR+               
      2.38183475E-06    2      1000015  -1000015                       # HH0    -->  TAU1-  TAU1+              
#         PDG         Width
DECAY        36  7.62508869E+00   # HA0   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      1.16562608E-08    2           11       -11                       # HA0    -->  E-     E+                 
      4.92148334E-04    2           13       -13                       # HA0    -->  MU-    MU+                
      1.40958667E-01    2           15       -15                       # HA0    -->  TAU-   TAU+               
      1.41877599E-05    2            1        -1                       # HA0    -->  DN     DB                 
      5.73257264E-03    2            3        -3                       # HA0    -->  ST     SB                 
      7.83057988E-01    2            5        -5                       # HA0    -->  BT     BB                 
      8.96717769E-11    2            2        -2                       # HA0    -->  UP     UB                 
      1.14874319E-06    2            4        -4                       # HA0    -->  CH     CB                 
      6.05726093E-02    2            6        -6                       # HA0    -->  TP     TB                 
      2.78544547E-07    2           22        22                       # HA0    -->  GM     GM                 
      3.47714595E-05    2           21        21                       # HA0    -->  GL     GL                 
      7.23920704E-04    2      1000022   1000022                       # HA0    -->  Z1SS   Z1SS               
      8.37204233E-03    2      1000022   1000023                       # HA0    -->  Z1SS   Z2SS               
      3.97260228E-05    2           25        23                       # HA0    -->  HL0    Z0                 
#         PDG         Width
DECAY        37  7.12928963E+00   # H+    decays
#          BR          NDA       ID1       ID2       ID3       ID4
      1.25564288E-08    2           12       -11                       # H+     -->  NUE    E+                 
      5.30155085E-04    2           14       -13                       # H+     -->  NUM    MU+                
      1.51844352E-01    2           16       -15                       # H+     -->  NUT    TAU+               
      1.41390019E-05    2            2        -1                       # H+     -->  UP     DB                 
      5.71397599E-03    2            4        -3                       # H+     -->  CH     SB                 
      8.31536174E-01    2            6        -5                       # H+     -->  TP     BB                 
      1.03183016E-02    2      1000024   1000022                       # H+     -->  W1SS+  Z1SS               
      4.28967905E-05    2           25        24                       # H+     -->  HL0    W+                 
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

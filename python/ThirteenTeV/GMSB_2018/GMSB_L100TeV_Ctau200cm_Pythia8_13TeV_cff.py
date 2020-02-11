
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
     1     1.00000000E+05   # Lambda scale of soft SSB
     2     2.00000000E+05   # M_mess overall messenger scale
     3     1.50000000E+01   # tan(beta)
     4     1.00000000E+00   # sign(mu)
     5     1.00000000E+00   # N_5 messenger index
     6     1.42850998E+02   # c_grav gravitino mass factor
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
        25     1.12660583E+02   #  h^0            
        35     5.32464172E+02   #  H^0            
        36     5.28826233E+02   #  A^0            
        37     5.38383728E+02   #  H^+            
   1000001     1.12279468E+03   #  dnl            
   1000002     1.11991418E+03   #  upl            
   1000003     1.12279468E+03   #  stl            
   1000004     1.11991504E+03   #  chl            
   1000005     1.06131323E+03   #  b1             
   1000006     9.83236389E+02   #  t1             
   1000011     3.59282593E+02   #  el-            
   1000012     3.47792084E+02   #  nuel           
   1000013     3.59282623E+02   #  mul-           
   1000014     3.47792084E+02   #  numl           
   1000015     1.73505310E+02   #  tau1           
   1000016     3.45739380E+02   #  nutl           
   1000021     8.37941467E+02   #  glss           
   1000022     1.39420563E+02   #  z1ss           
   1000023     2.62437164E+02   #  z2ss           
   1000024     2.62887177E+02   #  w1ss           
   1000025    -4.17500519E+02   #  z3ss           
   1000035     4.38492828E+02   #  z4ss           
   1000037     4.38239990E+02   #  w2ss           
   1000039     6.87292186E-07   #  gvss
   2000001     1.07008032E+03   #  dnr            
   2000002     1.07382593E+03   #  upr            
   2000003     1.07008032E+03   #  str            
   2000004     1.07382678E+03   #  chr            
   2000005     1.08112476E+03   #  b2             
   2000006     1.09331641E+03   #  t2             
   2000011     1.75364502E+02   #  er-            
   2000013     1.75364532E+02   #  mur-           
   2000015     3.58682983E+02   #  tau2           
Block ALPHA   # Effective Higgs mixing parameter
         -7.14971721E-02   # alpha
Block STOPMIX   # stop mixing matrix
  1  1     1.31063163E-01   # O_{11}
  1  2     9.91374016E-01   # O_{12}
  2  1    -9.91374016E-01   # O_{21}
  2  2     1.31063163E-01   # O_{22}
Block SBOTMIX   # sbottom mixing matrix
  1  1     4.31396365E-01   # O_{11}
  1  2     9.02162492E-01   # O_{12}
  2  1    -9.02162492E-01   # O_{21}
  2  2     4.31396365E-01   # O_{22}
Block STAUMIX   # stau mixing matrix
  1  1     1.06868908E-01   # O_{11}
  1  2     9.94273126E-01   # O_{12}
  2  1    -9.94273126E-01   # O_{21}
  2  2     1.06868908E-01   # O_{22}
Block NMIX   # neutralino mixing matrix
  1  1     9.90213990E-01   #
  1  2    -3.34359631E-02   #
  1  3     1.26071051E-01   #
  1  4    -4.96434607E-02   #
  2  1     7.86210597E-02   #
  2  2     9.32150126E-01   #
  2  3    -2.91743159E-01   #
  2  4     1.99502394E-01   #
  3  1     5.10697402E-02   #
  3  2    -7.13807121E-02   #
  3  3    -6.99376285E-01   #
  3  4    -7.09344268E-01   #
  4  1     1.03377640E-01   #
  4  2    -3.53388399E-01   #
  4  3    -6.40206873E-01   #
  4  4     6.74214423E-01   #
Block UMIX   # chargino U mixing matrix
  1  1    -9.10573781E-01   # U_{11}
  1  2     4.13346618E-01   # U_{12}
  2  1    -4.13346618E-01   # U_{21}
  2  2    -9.10573781E-01   # U_{22}
Block VMIX   # chargino V mixing matrix
  1  1    -9.59697127E-01   # V_{11}
  1  2     2.81036437E-01   # V_{12}
  2  1    -2.81036437E-01   # V_{21}
  2  2    -9.59697127E-01   # V_{22}
Block GAUGE Q=  9.94140076E+02   #
     1     3.57524991E-01   # g`
     2     6.52378619E-01   # g_2
     3     1.21928000E+00   # g_3
Block YU Q=  9.94140076E+02   #
  3  3     8.68142247E-01   # y_t
Block YD Q=  9.94140076E+02   #
  3  3     1.97788149E-01   # y_b
Block YE Q=  9.94140076E+02   #
  3  3     1.52371675E-01   # y_tau
Block HMIX Q=  9.94140076E+02   # Higgs mixing parameters
     1     4.09184875E+02   # mu(Q)
     2     1.45027819E+01   # tan(beta)(Q)
     3     2.50755539E+02   # Higgs vev at Q
     4     2.79657188E+05   # m_A^2(Q)
Block MSOFT Q=  9.94140076E+02   # DRbar SUSY breaking parameters
     1     1.44496643E+02   # M_1(Q)          
     2     2.71771362E+02   # M_2(Q)          
     3     7.54792358E+02   # M_3(Q)          
    21     1.09165062E+05   # MHd^2(Q)        
    22    -1.54057359E+05   # MHu^2(Q)        
    31     3.53854187E+02   # MeL(Q)          
    32     3.53854187E+02   # MmuL(Q)         
    33     3.51864899E+02   # MtauL(Q)        
    34     1.70639542E+02   # MeR(Q)          
    35     1.70639542E+02   # MmuR(Q)         
    36     1.68160034E+02   # MtauR(Q)        
    41     1.08344421E+03   # MqL1(Q)         
    42     1.08344421E+03   # MqL2(Q)         
    43     1.04217688E+03   # MqL3(Q)         
    44     1.03599817E+03   # MuR(Q)          
    45     1.03599817E+03   # McR(Q)          
    46     9.48317505E+02   # MtR(Q)          
    47     1.03127246E+03   # MdR(Q)          
    48     1.03127246E+03   # MsR(Q)          
    49     1.02739893E+03   # MbR(Q)          
Block AU Q=  9.94140076E+02   #
  1  1    -2.32469284E+02   # A_u
  2  2    -2.32469284E+02   # A_c
  3  3    -2.32469284E+02   # A_t
Block AD Q=  9.94140076E+02   #
  1  1    -2.60610168E+02   # A_d
  2  2    -2.60610168E+02   # A_s
  3  3    -2.60610168E+02   # A_b
Block AE Q=  9.94140076E+02   #
  1  1    -2.63792038E+01   # A_e
  2  2    -2.63792038E+01   # A_mu
  3  3    -2.63792038E+01   # A_tau
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
DECAY   1000021  1.81679316E-02   # GLSS  decays
#          BR          NDA       ID1       ID2       ID3       ID4
      8.55864882E-02    3      1000024         1        -2             # GLSS   -->  W1SS+  DN     UB          
      8.55864882E-02    3     -1000024         2        -1             # GLSS   -->  W1SS-  UP     DB          
      8.55862945E-02    3      1000024         3        -4             # GLSS   -->  W1SS+  ST     CB          
      8.55862945E-02    3     -1000024         4        -3             # GLSS   -->  W1SS-  CH     SB          
      4.87022921E-02    3      1000024         5        -6             # GLSS   -->  W1SS+  BT     TB          
      4.87022921E-02    3     -1000024         6        -5             # GLSS   -->  W1SS-  TP     BB          
      4.16135555E-03    3      1000037         1        -2             # GLSS   -->  W2SS+  DN     UB          
      4.16135555E-03    3     -1000037         2        -1             # GLSS   -->  W2SS-  UP     DB          
      4.16134857E-03    3      1000037         3        -4             # GLSS   -->  W2SS+  ST     CB          
      4.16134857E-03    3     -1000037         4        -3             # GLSS   -->  W2SS-  CH     SB          
      4.93497662E-02    3      1000037         5        -6             # GLSS   -->  W2SS+  BT     TB          
      4.93497662E-02    3     -1000037         6        -5             # GLSS   -->  W2SS-  TP     BB          
      4.90525490E-05    2      1000022        21                       # GLSS   -->  Z1SS   GL                 
      4.75236885E-02    3      1000022         2        -2             # GLSS   -->  Z1SS   UP     UB          
      1.48751494E-02    3      1000022         1        -1             # GLSS   -->  Z1SS   DN     DB          
      1.48751494E-02    3      1000022         3        -3             # GLSS   -->  Z1SS   ST     SB          
      4.75234650E-02    3      1000022         4        -4             # GLSS   -->  Z1SS   CH     CB          
      1.66878048E-02    3      1000022         5        -5             # GLSS   -->  Z1SS   BT     BB          
      2.34468691E-02    3      1000022         6        -6             # GLSS   -->  Z1SS   TP     TB          
      1.27998251E-03    2      1000023        21                       # GLSS   -->  Z2SS   GL                 
      4.44265231E-02    3      1000023         2        -2             # GLSS   -->  Z2SS   UP     UB          
      4.10145558E-02    3      1000023         1        -1             # GLSS   -->  Z2SS   DN     DB          
      4.10145558E-02    3      1000023         3        -3             # GLSS   -->  Z2SS   ST     SB          
      4.44263257E-02    3      1000023         4        -4             # GLSS   -->  Z2SS   CH     CB          
      5.44009879E-02    3      1000023         5        -5             # GLSS   -->  Z2SS   BT     BB          
      7.33838743E-03    3      1000023         6        -6             # GLSS   -->  Z2SS   TP     TB          
      7.48055940E-03    2      1000025        21                       # GLSS   -->  Z3SS   GL                 
      4.82715732E-05    3      1000025         2        -2             # GLSS   -->  Z3SS   UP     UB          
      5.82629727E-05    3      1000025         1        -1             # GLSS   -->  Z3SS   DN     DB          
      5.82629727E-05    3      1000025         3        -3             # GLSS   -->  Z3SS   ST     SB          
      4.82713258E-05    3      1000025         4        -4             # GLSS   -->  Z3SS   CH     CB          
      5.34723373E-03    3      1000025         5        -5             # GLSS   -->  Z3SS   BT     BB          
      2.98272283E-03    3      1000025         6        -6             # GLSS   -->  Z3SS   TP     TB          
      6.09271135E-03    2      1000035        21                       # GLSS   -->  Z4SS   GL                 
      2.04657554E-03    3      1000035         2        -2             # GLSS   -->  Z4SS   UP     UB          
      2.37213145E-03    3      1000035         1        -1             # GLSS   -->  Z4SS   DN     DB          
      2.37213145E-03    3      1000035         3        -3             # GLSS   -->  Z4SS   ST     SB          
      2.04656553E-03    3      1000035         4        -4             # GLSS   -->  Z4SS   CH     CB          
      6.89315982E-03    3      1000035         5        -5             # GLSS   -->  Z4SS   BT     BB          
      8.17588624E-03    3      1000035         6        -6             # GLSS   -->  Z4SS   TP     TB          
      5.54199880E-11    2      1000039        21                       # GLSS   -->  GVSS   GL                 
#         PDG         Width
DECAY   1000002  2.57213306E+01   # UPL   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      3.88983870E-03    2      1000022         2                       # UPL    -->  Z1SS   UP                 
      1.46668598E-01    2      1000023         2                       # UPL    -->  Z2SS   UP                 
      5.22705843E-04    2      1000025         2                       # UPL    -->  Z3SS   UP                 
      1.46966139E-02    2      1000035         2                       # UPL    -->  Z4SS   UP                 
      5.12039423E-01    2      1000021         2                       # UPL    -->  GLSS   UP                 
      3.01419586E-01    2      1000024         1                       # UPL    -->  W1SS+  DN                 
      2.07631979E-02    2      1000037         1                       # UPL    -->  W2SS+  DN                 
#         PDG         Width
DECAY   1000001  2.57980404E+01   # DNL   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      8.19784775E-03    2      1000022         1                       # DNL    -->  Z1SS   DN                 
      1.37902573E-01    2      1000023         1                       # DNL    -->  Z2SS   DN                 
      8.86785449E-04    2      1000025         1                       # DNL    -->  Z3SS   DN                 
      1.82404984E-02    2      1000035         1                       # DNL    -->  Z4SS   DN                 
      5.18388391E-01    2      1000021         1                       # DNL    -->  GLSS   DN                 
      2.71403313E-01    2     -1000024         2                       # DNL    -->  W1SS-  UP                 
      4.49805297E-02    2     -1000037         2                       # DNL    -->  W2SS-  UP                 
#         PDG         Width
DECAY   1000003  2.57980118E+01   # STL   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      8.19785707E-03    2      1000022         3                       # STL    -->  Z1SS   ST                 
      1.37902722E-01    2      1000023         3                       # STL    -->  Z2SS   ST                 
      8.86786438E-04    2      1000025         3                       # STL    -->  Z3SS   ST                 
      1.82405189E-02    2      1000035         3                       # STL    -->  Z4SS   ST                 
      5.18388987E-01    2      1000021         3                       # STL    -->  GLSS   ST                 
      2.71402717E-01    2     -1000024         4                       # STL    -->  W1SS-  CH                 
      4.49804030E-02    2     -1000037         4                       # STL    -->  W2SS-  CH                 
#         PDG         Width
DECAY   1000004  2.57211914E+01   # CHL   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      3.88985127E-03    2      1000022         4                       # CHL    -->  Z1SS   CH                 
      1.46669075E-01    2      1000023         4                       # CHL    -->  Z2SS   CH                 
      5.22707298E-04    2      1000025         4                       # CHL    -->  Z3SS   CH                 
      1.46966530E-02    2      1000035         4                       # CHL    -->  Z4SS   CH                 
      5.12036860E-01    2      1000021         4                       # CHL    -->  GLSS   CH                 
      3.01421493E-01    2      1000024         3                       # CHL    -->  W1SS+  ST                 
      2.07633413E-02    2      1000037         3                       # CHL    -->  W2SS+  ST                 
#         PDG         Width
DECAY   1000005  1.48761377E+01   # BT1   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      4.15984280E-02    2      1000022         5                       # BT1    -->  Z1SS   BT                 
      6.93178177E-02    2      1000023         5                       # BT1    -->  Z2SS   BT                 
      1.53560946E-02    2      1000025         5                       # BT1    -->  Z3SS   BT                 
      3.27727827E-03    2      1000035         5                       # BT1    -->  Z4SS   BT                 
      6.26963496E-01    2      1000021         5                       # BT1    -->  GLSS   BT                 
      1.33943021E-01    2     -1000024         6                       # BT1    -->  W1SS-  TP                 
      1.09543815E-01    2     -1000037         6                       # BT1    -->  W2SS-  TP                 
#         PDG         Width
DECAY   1000006  2.04089775E+01   # TP1   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      9.74955931E-02    2      1000022         6                       # TP1    -->  Z1SS   TP                 
      3.93494964E-02    2      1000023         6                       # TP1    -->  Z2SS   TP                 
      2.31883302E-01    2      1000025         6                       # TP1    -->  Z3SS   TP                 
      1.64355695E-01    2      1000035         6                       # TP1    -->  Z4SS   TP                 
      8.44876096E-02    2      1000024         5                       # TP1    -->  W1SS+  BT                 
      3.82428288E-01    2      1000037         5                       # TP1    -->  W2SS+  BT                 
#         PDG         Width
DECAY   2000002  1.23479118E+01   # UPR   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      1.86315045E-01    2      1000022         2                       # UPR    -->  Z1SS   UP                 
      1.07433763E-03    2      1000023         2                       # UPR    -->  Z2SS   UP                 
      3.69430083E-04    2      1000025         2                       # UPR    -->  Z3SS   UP                 
      1.45869330E-03    2      1000035         2                       # UPR    -->  Z4SS   UP                 
      8.10782492E-01    2      1000021         2                       # UPR    -->  GLSS   UP                 
#         PDG         Width
DECAY   2000001  1.03452616E+01   # DNR   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      5.53883016E-02    2      1000022         1                       # DNR    -->  Z1SS   DN                 
      3.19174578E-04    2      1000023         1                       # DNR    -->  Z2SS   DN                 
      1.09577493E-04    2      1000025         1                       # DNR    -->  Z3SS   DN                 
      4.32532455E-04    2      1000035         1                       # DNR    -->  Z4SS   DN                 
      9.43750381E-01    2      1000021         1                       # DNR    -->  GLSS   DN                 
#         PDG         Width
DECAY   2000003  1.03452616E+01   # STR   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      5.53883016E-02    2      1000022         3                       # STR    -->  Z1SS   ST                 
      3.19174578E-04    2      1000023         3                       # STR    -->  Z2SS   ST                 
      1.09577493E-04    2      1000025         3                       # STR    -->  Z3SS   ST                 
      4.32532455E-04    2      1000035         3                       # STR    -->  Z4SS   ST                 
      9.43750381E-01    2      1000021         3                       # STR    -->  GLSS   ST                 
#         PDG         Width
DECAY   2000004  1.23477583E+01   # CHR   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      1.86316893E-01    2      1000022         4                       # CHR    -->  Z1SS   CH                 
      1.07434823E-03    2      1000023         4                       # CHR    -->  Z2SS   CH                 
      3.69433546E-04    2      1000025         4                       # CHR    -->  Z3SS   CH                 
      1.45870703E-03    2      1000035         4                       # CHR    -->  Z4SS   CH                 
      8.10780585E-01    2      1000021         4                       # CHR    -->  GLSS   CH                 
#         PDG         Width
DECAY   2000005  2.93883991E+01   # BT2   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      6.04425883E-03    2      1000022         5                       # BT2    -->  Z1SS   BT                 
      8.40901881E-02    2      1000023         5                       # BT2    -->  Z2SS   BT                 
      1.36831133E-02    2      1000025         5                       # BT2    -->  Z3SS   BT                 
      3.04729678E-02    2      1000035         5                       # BT2    -->  Z4SS   BT                 
      3.52164775E-01    2      1000021         5                       # BT2    -->  GLSS   BT                 
      1.69198439E-01    2     -1000024         6                       # BT2    -->  W1SS-  TP                 
      3.43972504E-01    2     -1000037         6                       # BT2    -->  W2SS-  TP                 
      3.73750256E-04    2          -24   1000006                       # BT2    -->  W-     TP1                
#         PDG         Width
DECAY   2000006  2.98252010E+01   # TP2   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      2.18654707E-01    2      1000021         6                       # TP2    -->  GLSS   TP                 
      2.27669343E-01    2      1000024         5                       # TP2    -->  W1SS+  BT                 
      5.99550307E-02    2      1000037         5                       # TP2    -->  W2SS+  BT                 
      2.99326435E-04    2           23   1000006                       # TP2    -->  Z0     TP1                
      3.56667209E-03    2      1000022         6                       # TP2    -->  Z1SS   TP                 
      1.13468163E-01    2      1000023         6                       # TP2    -->  Z2SS   TP                 
      1.74201041E-01    2      1000025         6                       # TP2    -->  Z3SS   TP                 
      2.02185735E-01    2      1000035         6                       # TP2    -->  Z4SS   TP                 
#         PDG         Width
DECAY   1000011  1.13910723E+00   # EL-   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      2.49940380E-01    2      1000022        11                       # EL-    -->  Z1SS   E-                 
      2.74800956E-01    2      1000023        11                       # EL-    -->  Z2SS   E-                 
      4.75258499E-01    2     -1000024        12                       # EL-    -->  W1SS-  NUE                
      1.04332671E-07    3      1000015        11       -15             # EL-    -->  TAU1-  E-     TAU+        
      9.53828803E-08    3     -1000015        11        15             # EL-    -->  TAU1+  E-     TAU-        
      1.28091372E-14    2           11   1000039                       # EL-    -->  E-     GVSS               
#         PDG         Width
DECAY   1000013  1.13910723E+00   # MUL-  decays
#          BR          NDA       ID1       ID2       ID3       ID4
      2.49940351E-01    2      1000022        13                       # MUL-   -->  Z1SS   MU-                
      2.74800777E-01    2      1000023        13                       # MUL-   -->  Z2SS   MU-                
      4.75258648E-01    2     -1000024        14                       # MUL-   -->  W1SS-  NUM                
      1.04332763E-07    3      1000015        13       -15             # MUL-   -->  TAU1-  MU-    TAU+        
      9.53829442E-08    3     -1000015        13        15             # MUL-   -->  TAU1+  MU-    TAU-        
      1.28091372E-14    2           13   1000039                       # MUL-   -->  MU-    GVSS               
#         PDG         Width
DECAY   1000015  1.08232148E-01   # TAU1- decays
#          BR          NDA       ID1       ID2       ID3       ID4
      1.00000000E+00    2      1000022        15                       # TAU1-  -->  Z1SS   TAU-               
      3.53939324E-15    2           15   1000039                       # TAU1-  -->  TAU-   GVSS               
#         PDG         Width
DECAY   1000012  1.05414987E+00   # NUEL  decays
#          BR          NDA       ID1       ID2       ID3       ID4
      3.26465011E-01    2      1000022        12                       # NUEL   -->  Z1SS   NUE                
      2.03487173E-01    2      1000023        12                       # NUEL   -->  Z2SS   NUE                
      4.70046759E-01    2      1000024        11                       # NUEL   -->  W1SS+  E-                 
      1.76525631E-07    3      1000015        12       -15             # NUEL   -->  TAU1-  NUE    TAU+        
      1.06743258E-07    3     -1000015        12        15             # NUEL   -->  TAU1+  NUE    TAU-        
      7.22027096E-07    3     -1000015        11        16             # NUEL   -->  TAU1+  E-     NUT         
      1.17652097E-14    2           12   1000039                       # NUEL   -->  NUE    GVSS               
#         PDG         Width
DECAY   1000014  1.05414951E+00   # NUML  decays
#          BR          NDA       ID1       ID2       ID3       ID4
      3.26465130E-01    2      1000022        14                       # NUML   -->  Z1SS   NUM                
      2.03487232E-01    2      1000023        14                       # NUML   -->  Z2SS   NUM                
      4.70046520E-01    2      1000024        13                       # NUML   -->  W1SS+  MU-                
      1.76525702E-07    3      1000015        14       -15             # NUML   -->  TAU1-  NUM    TAU+        
      1.06743293E-07    3     -1000015        14        15             # NUML   -->  TAU1+  NUM    TAU-        
      7.22270329E-07    3     -1000015        13        16             # NUML   -->  TAU1+  MU-    NUT         
      1.17652139E-14    2           14   1000039                       # NUML   -->  NUM    GVSS               
#         PDG         Width
DECAY   1000016  1.11302769E+00   # NUTL  decays
#          BR          NDA       ID1       ID2       ID3       ID4
      3.05970341E-01    2      1000022        16                       # NUTL   -->  Z1SS   NUT                
      1.85598820E-01    2      1000023        16                       # NUTL   -->  Z2SS   NUT                
      4.31368649E-01    2      1000024        15                       # NUTL   -->  W1SS+  TAU-               
      7.70618916E-02    2           24   1000015                       # NUTL   -->  W+     TAU1-              
      2.22993307E-07    3     -1000015        16        15             # NUTL   -->  TAU1+  NUT    TAU-        
      9.55034309E-08    3      1000015        16       -15             # NUTL   -->  TAU1-  NUT    TAU+        
      1.08178711E-14    2           16   1000039                       # NUTL   -->  NUT    GVSS               
#         PDG         Width
DECAY   2000011  1.18389025E-01   # ER-   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      1.00000000E+00    2      1000022        11                       # ER-    -->  Z1SS   E-                 
      1.91164202E-20    3      1000015        11       -15             # ER-    -->  TAU1-  E-     TAU+        
      5.78249897E-21    3     -1000015        11        15             # ER-    -->  TAU1+  E-     TAU-        
      2.39223092E-22    3      1000015        12       -16             # ER-    -->  TAU1-  NUE    ANUT        
      3.41429134E-15    2           11   1000039                       # ER-    -->  E-     GVSS               
#         PDG         Width
DECAY   2000013  1.18388548E-01   # MUR-  decays
#          BR          NDA       ID1       ID2       ID3       ID4
      1.00000000E+00    2      1000022        13                       # MUR-   -->  Z1SS   MU-                
      1.01712659E-17    3      1000015        14       -16             # MUR-   -->  TAU1-  NUM    ANUT        
      3.41430214E-15    2           13   1000039                       # MUR-   -->  MU-    GVSS               
#         PDG         Width
DECAY   2000015  1.22149456E+00   # TAU2- decays
#          BR          NDA       ID1       ID2       ID3       ID4
      2.38455683E-01    2      1000022        15                       # TAU2-  -->  Z1SS   TAU-               
      2.47719064E-01    2      1000023        15                       # TAU2-  -->  Z2SS   TAU-               
      4.24014062E-01    2     -1000024        16                       # TAU2-  -->  W1SS-  NUT                
      3.98590975E-02    2           23   1000015                       # TAU2-  -->  Z0     TAU1-              
      4.99520861E-02    2           25   1000015                       # TAU2-  -->  HL0    TAU1-              
#         PDG         Width
DECAY   1000022  9.86184314E-17   # Z1SS  decays
#          BR          NDA       ID1       ID2       ID3       ID4
      9.44338500E-01    2      1000039        22                       # Z1SS   -->  GVSS   GM                 
      1.95958521E-02    3      1000039        11       -11             # Z1SS   -->  GVSS   E-     E+          
      3.60501595E-02    2      1000039        23                       # Z1SS   -->  GVSS   Z0                 
      1.54944664E-05    2      1000039        25                       # Z1SS   -->  GVSS   HL0                
#         PDG         Width
DECAY   1000023  3.37404385E-02   # Z2SS  decays
#          BR          NDA       ID1       ID2       ID3       ID4
      1.95272605E-06    2      1000022        22                       # Z2SS   -->  Z1SS   GM                 
      6.14033900E-02    2      1000022        23                       # Z2SS   -->  Z1SS   Z0                 
      1.39800733E-07    3      1000022         2        -2             # Z2SS   -->  Z1SS   UP     UB          
      2.48679754E-07    3      1000022         1        -1             # Z2SS   -->  Z1SS   DN     DB          
      2.48679754E-07    3      1000022         3        -3             # Z2SS   -->  Z1SS   ST     SB          
      1.39800264E-07    3      1000022         4        -4             # Z2SS   -->  Z1SS   CH     CB          
      1.84038163E-06    3      1000022         5        -5             # Z2SS   -->  Z1SS   BT     BB          
      1.04971456E-04    3      1000022        11       -11             # Z2SS   -->  Z1SS   E-     E+          
      1.04971397E-04    3      1000022        13       -13             # Z2SS   -->  Z1SS   MU-    MU+         
      1.03464954E-04    3      1000022        15       -15             # Z2SS   -->  Z1SS   TAU-   TAU+        
      1.36373623E-04    3      1000022        12       -12             # Z2SS   -->  Z1SS   NUE    ANUE        
      1.36373623E-04    3      1000022        14       -14             # Z2SS   -->  Z1SS   NUM    ANUM        
      1.41589582E-04    3      1000022        16       -16             # Z2SS   -->  Z1SS   NUT    ANUT        
      3.13608617E-01    2      1000022        25                       # Z2SS   -->  Z1SS   HL0                
      3.74574549E-02    2      2000011       -11                       # Z2SS   -->  ER-    E+                 
      3.74574549E-02    2     -2000011        11                       # Z2SS   -->  ER+    E-                 
      3.74574214E-02    2      2000013       -13                       # Z2SS   -->  MUR-   MU+                
      3.74574214E-02    2     -2000013        13                       # Z2SS   -->  MUR+   MU-                
      2.37213030E-01    2      1000015       -15                       # Z2SS   -->  TAU1-  TAU+               
      2.37213030E-01    2     -1000015        15                       # Z2SS   -->  TAU1+  TAU-               
      2.41180824E-14    2      1000039        22                       # Z2SS   -->  GVSS   GM                 
      5.25762297E-16    3      1000039        11       -11             # Z2SS   -->  GVSS   E-     E+          
      3.39087889E-14    2      1000039        23                       # Z2SS   -->  GVSS   Z0                 
      6.31800711E-16    2      1000039        25                       # Z2SS   -->  GVSS   HL0                
#         PDG         Width
DECAY   1000025  1.57316959E+00   # Z3SS  decays
#          BR          NDA       ID1       ID2       ID3       ID4
      1.20124328E-08    2      1000022        22                       # Z3SS   -->  Z1SS   GM                 
      5.28377257E-07    2      1000023        22                       # Z3SS   -->  Z2SS   GM                 
      2.77044445E-01    2      1000024       -24                       # Z3SS   -->  W1SS+  W-                 
      2.77044445E-01    2     -1000024        24                       # Z3SS   -->  W1SS-  W+                 
      1.42719850E-01    2      1000022        23                       # Z3SS   -->  Z1SS   Z0                 
      2.18985945E-01    2      1000023        23                       # Z3SS   -->  Z2SS   Z0                 
      2.24235208E-09    3      1000022         2        -2             # Z3SS   -->  Z1SS   UP     UB          
      8.18013768E-10    3      1000022         1        -1             # Z3SS   -->  Z1SS   DN     DB          
      8.18013768E-10    3      1000022         3        -3             # Z3SS   -->  Z1SS   ST     SB          
      2.24234431E-09    3      1000022         4        -4             # Z3SS   -->  Z1SS   CH     CB          
      2.94018173E-06    3      1000022         5        -5             # Z3SS   -->  Z1SS   BT     BB          
      4.65083332E-07    3      1000022        15       -15             # Z3SS   -->  Z1SS   TAU-   TAU+        
      7.64406760E-10    3      1000023         2        -2             # Z3SS   -->  Z2SS   UP     UB          
      1.20192112E-09    3      1000023         1        -1             # Z3SS   -->  Z2SS   DN     DB          
      1.20192112E-09    3      1000023         3        -3             # Z3SS   -->  Z2SS   ST     SB          
      7.64404262E-10    3      1000023         4        -4             # Z3SS   -->  Z2SS   CH     CB          
      6.00259625E-07    3      1000023         5        -5             # Z3SS   -->  Z2SS   BT     BB          
      8.73540458E-08    3      1000023        15       -15             # Z3SS   -->  Z2SS   TAU-   TAU+        
      2.45094318E-02    2      1000022        25                       # Z3SS   -->  Z1SS   HL0                
      5.48282638E-03    2      1000023        25                       # Z3SS   -->  Z2SS   HL0                
      7.05276034E-05    2      1000011       -11                       # Z3SS   -->  EL-    E+                 
      7.05276034E-05    2     -1000011        11                       # Z3SS   -->  EL+    E-                 
      7.05274651E-05    2      1000013       -13                       # Z3SS   -->  MUL-   MU+                
      7.05274651E-05    2     -1000013        13                       # Z3SS   -->  MUL+   MU-                
      1.19393296E-03    2      2000011       -11                       # Z3SS   -->  ER-    E+                 
      1.19393296E-03    2     -2000011        11                       # Z3SS   -->  ER+    E-                 
      1.19393272E-03    2      2000013       -13                       # Z3SS   -->  MUR-   MU+                
      1.19393272E-03    2     -2000013        13                       # Z3SS   -->  MUR+   MU-                
      2.05654204E-02    2      1000015       -15                       # Z3SS   -->  TAU1-  TAU+               
      2.05654204E-02    2     -1000015        15                       # Z3SS   -->  TAU1+  TAU-               
      2.42938125E-03    2      2000015       -15                       # Z3SS   -->  TAU2-  TAU+               
      2.42938125E-03    2     -2000015        15                       # Z3SS   -->  TAU2+  TAU-               
      5.17449924E-04    2      1000012       -12                       # Z3SS   -->  NUEL   ANUE               
      5.17449924E-04    2     -1000012        12                       # Z3SS   -->  ANUEL  NUE                
      5.17449924E-04    2      1000014       -14                       # Z3SS   -->  NUML   ANUM               
      5.17449924E-04    2     -1000014        14                       # Z3SS   -->  ANUML  NUM                
      5.45435527E-04    2      1000016       -16                       # Z3SS   -->  NUTL   ANUT               
      5.45435527E-04    2     -1000016        16                       # Z3SS   -->  ANUTL  NUT                
      2.11488679E-18    2      1000039        22                       # Z3SS   -->  GVSS   GM                 
      4.77313231E-20    3      1000039        11       -11             # Z3SS   -->  GVSS   E-     E+          
      3.65653366E-15    2      1000039        23                       # Z3SS   -->  GVSS   Z0                 
      4.16680282E-15    2      1000039        25                       # Z3SS   -->  GVSS   HL0                
#         PDG         Width
DECAY   1000035  2.16767907E+00   # Z4SS  decays
#          BR          NDA       ID1       ID2       ID3       ID4
      5.12962606E-09    2      1000022        22                       # Z4SS   -->  Z1SS   GM                 
      1.47069485E-07    2      1000023        22                       # Z4SS   -->  Z2SS   GM                 
      5.73870995E-09    2      1000025        22                       # Z4SS   -->  Z3SS   GM                 
      2.95617312E-01    2      1000024       -24                       # Z4SS   -->  W1SS+  W-                 
      2.95617312E-01    2     -1000024        24                       # Z4SS   -->  W1SS-  W+                 
      2.26062648E-02    2      1000022        23                       # Z4SS   -->  Z1SS   Z0                 
      1.18649490E-02    2      1000023        23                       # Z4SS   -->  Z2SS   Z0                 
      3.13977466E-08    3      1000022         2        -2             # Z4SS   -->  Z1SS   UP     UB          
      3.38946826E-08    3      1000022         1        -1             # Z4SS   -->  Z1SS   DN     DB          
      3.38946826E-08    3      1000022         3        -3             # Z4SS   -->  Z1SS   ST     SB          
      3.13976400E-08    3      1000022         4        -4             # Z4SS   -->  Z1SS   CH     CB          
      1.98484599E-06    3      1000022         5        -5             # Z4SS   -->  Z1SS   BT     BB          
      3.11173693E-07    3      1000022        15       -15             # Z4SS   -->  Z1SS   TAU-   TAU+        
      8.10549281E-08    3      1000023         2        -2             # Z4SS   -->  Z2SS   UP     UB          
      9.33409083E-08    3      1000023         1        -1             # Z4SS   -->  Z2SS   DN     DB          
      9.33409083E-08    3      1000023         3        -3             # Z4SS   -->  Z2SS   ST     SB          
      8.10546581E-08    3      1000023         4        -4             # Z4SS   -->  Z2SS   CH     CB          
      5.49582580E-07    3      1000023         5        -5             # Z4SS   -->  Z2SS   BT     BB          
      6.36902442E-08    3      1000023        15       -15             # Z4SS   -->  Z2SS   TAU-   TAU+        
      1.95981002E-07    3      1000025         2        -2             # Z4SS   -->  Z3SS   UP     UB          
      2.52914703E-07    3      1000025         1        -1             # Z4SS   -->  Z3SS   DN     DB          
      2.52914703E-07    3      1000025         3        -3             # Z4SS   -->  Z3SS   ST     SB          
      1.95981002E-07    3      1000025         4        -4             # Z4SS   -->  Z3SS   CH     CB          
      1.33505694E-07    3      1000025         5        -5             # Z4SS   -->  Z3SS   BT     BB          
      5.73485686E-08    3      1000025        11       -11             # Z4SS   -->  Z3SS   E-     E+          
      5.73485686E-08    3      1000025        13       -13             # Z4SS   -->  Z3SS   MU-    MU+         
      5.19428589E-08    3      1000025        15       -15             # Z4SS   -->  Z3SS   TAU-   TAU+        
      1.14105603E-07    3      1000025        12       -12             # Z4SS   -->  Z3SS   NUE    ANUE        
      1.14105603E-07    3      1000025        14       -14             # Z4SS   -->  Z3SS   NUM    ANUM        
      1.14105603E-07    3      1000025        16       -16             # Z4SS   -->  Z3SS   NUT    ANUT        
      8.87805372E-02    2      1000022        25                       # Z4SS   -->  Z1SS   HL0                
      1.55075401E-01    2      1000023        25                       # Z4SS   -->  Z2SS   HL0                
      4.04461799E-03    2      1000011       -11                       # Z4SS   -->  EL-    E+                 
      4.04461799E-03    2     -1000011        11                       # Z4SS   -->  EL+    E-                 
      4.04461334E-03    2      1000013       -13                       # Z4SS   -->  MUL-   MU+                
      4.04461334E-03    2     -1000013        13                       # Z4SS   -->  MUL+   MU-                
      3.87980347E-03    2      2000011       -11                       # Z4SS   -->  ER-    E+                 
      3.87980347E-03    2     -2000011        11                       # Z4SS   -->  ER+    E-                 
      3.87980253E-03    2      2000013       -13                       # Z4SS   -->  MUR-   MU+                
      3.87980253E-03    2     -2000013        13                       # Z4SS   -->  MUR+   MU-                
      1.24651380E-02    2      1000015       -15                       # Z4SS   -->  TAU1-  TAU+               
      1.24651380E-02    2     -1000015        15                       # Z4SS   -->  TAU1+  TAU-               
      6.93802815E-03    2      2000015       -15                       # Z4SS   -->  TAU2-  TAU+               
      6.93802815E-03    2     -2000015        15                       # Z4SS   -->  TAU2+  TAU-               
      9.85569134E-03    2      1000012       -12                       # Z4SS   -->  NUEL   ANUE               
      9.85569134E-03    2     -1000012        12                       # Z4SS   -->  ANUEL  NUE                
      9.85569134E-03    2      1000014       -14                       # Z4SS   -->  NUML   ANUM               
      9.85569134E-03    2     -1000014        14                       # Z4SS   -->  ANUML  NUM                
      1.02530960E-02    2      1000016       -16                       # Z4SS   -->  NUTL   ANUT               
      1.02530960E-02    2     -1000016        16                       # Z4SS   -->  ANUTL  NUT                
      1.15545180E-16    2      1000039        22                       # Z4SS   -->  GVSS   GM                 
      2.61716079E-18    3      1000039        11       -11             # Z4SS   -->  GVSS   E-     E+          
      5.88145077E-15    2      1000039        23                       # Z4SS   -->  GVSS   Z0                 
      2.72431292E-15    2      1000039        25                       # Z4SS   -->  GVSS   HL0                
#         PDG         Width
DECAY   1000024  2.74252333E-02   # W1SS+ decays
#          BR          NDA       ID1       ID2       ID3       ID4
      4.60564763E-07    3      1000022         2        -1             # W1SS+  -->  Z1SS   UP     DB          
      4.60564195E-07    3      1000022         4        -3             # W1SS+  -->  Z1SS   CH     SB          
      3.10645497E-04    3      1000022       -11        12             # W1SS+  -->  Z1SS   E+     NUE         
      3.10645439E-04    3      1000022       -13        14             # W1SS+  -->  Z1SS   MU+    NUM         
      3.16078134E-04    3      1000022       -15        16             # W1SS+  -->  Z1SS   TAU+   NUT         
      5.09333432E-01    2      1000022        24                       # W1SS+  -->  Z1SS   W+                 
      4.02256055E-13    3      1000023       -11        12             # W1SS+  -->  Z2SS   E+     NUE         
      4.02256028E-13    3      1000023       -13        14             # W1SS+  -->  Z2SS   MU+    NUM         
      4.89728302E-01    2     -1000015        16                       # W1SS+  -->  TAU1+  NUT                
#         PDG         Width
DECAY   1000037  1.89320457E+00   # W2SS+ decays
#          BR          NDA       ID1       ID2       ID3       ID4
      5.26969011E-08    3      1000022         2        -1             # W2SS+  -->  Z1SS   UP     DB          
      5.26968478E-08    3      1000022         4        -3             # W2SS+  -->  Z1SS   CH     SB          
      2.55078703E-07    3      1000022       -15        16             # W2SS+  -->  Z1SS   TAU+   NUT         
      1.01918742E-01    2      1000022        24                       # W2SS+  -->  Z1SS   W+                 
      7.45283941E-08    3      1000023         2        -1             # W2SS+  -->  Z2SS   UP     DB          
      7.45283444E-08    3      1000023         4        -3             # W2SS+  -->  Z2SS   CH     SB          
      1.99299237E-07    3      1000023       -15        16             # W2SS+  -->  Z2SS   TAU+   NUT         
      3.11031610E-01    2      1000023        24                       # W2SS+  -->  Z2SS   W+                 
      7.50685388E-07    3      1000025         2        -1             # W2SS+  -->  Z3SS   UP     DB          
      7.50685388E-07    3      1000025         4        -3             # W2SS+  -->  Z3SS   CH     SB          
      2.50198951E-07    3      1000025       -11        12             # W2SS+  -->  Z3SS   E+     NUE         
      2.50198951E-07    3      1000025       -13        14             # W2SS+  -->  Z3SS   MU+    NUM         
      2.50199861E-07    3      1000025       -15        16             # W2SS+  -->  Z3SS   TAU+   NUT         
      1.05458433E-02    2      1000012       -11                       # W2SS+  -->  NUEL   E+                 
      1.05458349E-02    2      1000014       -13                       # W2SS+  -->  NUML   MU+                
      1.71406157E-02    2      1000016       -15                       # W2SS+  -->  NUTL   TAU+               
      1.78969409E-02    2     -1000011        12                       # W2SS+  -->  EL+    NUE                
      1.78969335E-02    2     -1000013        14                       # W2SS+  -->  MUL+   NUM                
      2.03690995E-02    2     -1000015        16                       # W2SS+  -->  TAU1+  NUT                
      2.00152602E-02    2     -2000015        16                       # W2SS+  -->  TAU2+  NUT                
      2.70409793E-01    2      1000024        23                       # W2SS+  -->  W1SS+  Z0                 
      9.01657131E-08    3      1000024         1        -1             # W2SS+  -->  W1SS+  DN     DB          
      9.01653792E-08    3      1000024         3        -3             # W2SS+  -->  W1SS+  ST     SB          
      1.73596064E-07    3      1000024         2        -2             # W2SS+  -->  W1SS+  UP     UB          
      1.73596064E-07    3      1000024         4        -4             # W2SS+  -->  W1SS+  CH     CB          
      2.02225879E-01    2      1000024        25                       # W2SS+  -->  W1SS+  HL0                
#         PDG         Width
DECAY        25  3.14349518E-03   # HL0   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      7.02421721E-09    2           11       -11                       # HL0    -->  E-     E+                 
      2.96573562E-04    2           13       -13                       # HL0    -->  MU-    MU+                
      8.48168954E-02    2           15       -15                       # HL0    -->  TAU-   TAU+               
      8.81886808E-06    2            1        -1                       # HL0    -->  DN     DB                 
      3.56326415E-03    2            3        -3                       # HL0    -->  ST     SB                 
      7.62523592E-01    2            5        -5                       # HL0    -->  BT     BB                 
      2.44499006E-06    2            2        -2                       # HL0    -->  UP     UB                 
      4.61725593E-02    2            4        -4                       # HL0    -->  CH     CB                 
      2.18242197E-03    2           22        22                       # HL0    -->  GM     GM                 
      4.73928303E-02    2           21        21                       # HL0    -->  GL     GL                 
      2.77047884E-03    3           24        11       -12             # HL0    -->  W+     E-     ANUE        
      2.77047884E-03    3           24        13       -14             # HL0    -->  W+     MU-    ANUM        
      2.77047884E-03    3           24        15       -16             # HL0    -->  W+     TAU-   ANUT        
      8.31143651E-03    3           24        -2         1             # HL0    -->  W+     UB     DN          
      8.31143651E-03    3           24        -4         3             # HL0    -->  W+     CB     ST          
      2.77047884E-03    3          -24       -11        12             # HL0    -->  W-     E+     NUE         
      2.77047884E-03    3          -24       -13        14             # HL0    -->  W-     MU+    NUM         
      2.77047884E-03    3          -24       -15        16             # HL0    -->  W-     TAU+   NUT         
      8.31143651E-03    3          -24         2        -1             # HL0    -->  W-     UP     DB          
      8.31143651E-03    3          -24         4        -3             # HL0    -->  W-     CH     SB          
      2.16942033E-04    3           23        12       -12             # HL0    -->  Z0     NUE    ANUE        
      2.16942033E-04    3           23        14       -14             # HL0    -->  Z0     NUM    ANUM        
      2.16942033E-04    3           23        16       -16             # HL0    -->  Z0     NUT    ANUT        
      1.09184744E-04    3           23        11       -11             # HL0    -->  Z0     E-     E+          
      1.09184744E-04    3           23        13       -13             # HL0    -->  Z0     MU-    MU+         
      1.09184744E-04    3           23        15       -15             # HL0    -->  Z0     TAU-   TAU+        
      3.74057679E-04    3           23         2        -2             # HL0    -->  Z0     UP     UB          
      3.74057679E-04    3           23         4        -4             # HL0    -->  Z0     CH     CB          
      4.81878466E-04    3           23         1        -1             # HL0    -->  Z0     DN     DB          
      4.81878466E-04    3           23         3        -3             # HL0    -->  Z0     ST     SB          
      4.81878466E-04    3           23         5        -5             # HL0    -->  Z0     BT     BB          
#         PDG         Width
DECAY        35  2.02854466E+00   # HH0   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      1.00296331E-08    2           11       -11                       # HH0    -->  E-     E+                 
      4.23469086E-04    2           13       -13                       # HH0    -->  MU-    MU+                
      1.21279962E-01    2           15       -15                       # HH0    -->  TAU-   TAU+               
      1.23556692E-05    2            1        -1                       # HH0    -->  DN     DB                 
      4.99231601E-03    2            3        -3                       # HH0    -->  ST     SB                 
      8.03130567E-01    2            5        -5                       # HH0    -->  BT     BB                 
      9.01258859E-11    2            2        -2                       # HH0    -->  UP     UB                 
      1.37183315E-06    2            4        -4                       # HH0    -->  CH     CB                 
      3.67517099E-02    2            6        -6                       # HH0    -->  TP     TB                 
      6.64652816E-07    2           22        22                       # HH0    -->  GM     GM                 
      7.37986993E-05    2           21        21                       # HH0    -->  GL     GL                 
      5.13162930E-04    2           24       -24                       # HH0    -->  W+     W-                 
      2.53374950E-04    2           23        23                       # HH0    -->  Z0     Z0                 
      6.80878386E-03    2      1000022   1000022                       # HH0    -->  Z1SS   Z1SS               
      2.18835846E-02    2      1000022   1000023                       # HH0    -->  Z1SS   Z2SS               
      6.36873709E-04    2      1000023   1000023                       # HH0    -->  Z2SS   Z2SS               
      1.24182913E-03    2      1000024  -1000024                       # HH0    -->  W1SS+  W1SS-              
      1.82321866E-03    2           25        25                       # HH0    -->  HL0    HL0                
      6.55611584E-05    2      2000011  -2000011                       # HH0    -->  ER-    ER+                
      6.54805553E-05    2      2000013  -2000013                       # HH0    -->  MUR-   MUR+               
      4.17565898E-05    2      1000015  -1000015                       # HH0    -->  TAU1-  TAU1+              
      4.12083381E-08    2      1000015  -2000015                       # HH0    -->  TAU1-  TAU2+              
      4.12083381E-08    2      2000015  -1000015                       # HH0    -->  TAU2-  TAU1+              
#         PDG         Width
DECAY        36  2.23700142E+00   # HA0   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      9.03903352E-09    2           11       -11                       # HA0    -->  E-     E+                 
      3.81644262E-04    2           13       -13                       # HA0    -->  MU-    MU+                
      1.09306306E-01    2           15       -15                       # HA0    -->  TAU-   TAU+               
      1.11360541E-05    2            1        -1                       # HA0    -->  DN     DB                 
      4.49952949E-03    2            3        -3                       # HA0    -->  ST     SB                 
      7.23979950E-01    2            5        -5                       # HA0    -->  BT     BB                 
      7.03838793E-11    2            2        -2                       # HA0    -->  UP     UB                 
      1.07227197E-06    2            4        -4                       # HA0    -->  CH     CB                 
      4.74659018E-02    2            6        -6                       # HA0    -->  TP     TB                 
      1.24006863E-06    2           22        22                       # HA0    -->  GM     GM                 
      1.23412188E-04    2           21        21                       # HA0    -->  GL     GL                 
      9.40446183E-03    2      1000022   1000022                       # HA0    -->  Z1SS   Z1SS               
      5.02452701E-02    2      1000022   1000023                       # HA0    -->  Z1SS   Z2SS               
      1.77778006E-02    2      1000023   1000023                       # HA0    -->  Z2SS   Z2SS               
      3.63872387E-02    2      1000024  -1000024                       # HA0    -->  W1SS+  W1SS-              
      4.15000191E-04    2           25        23                       # HA0    -->  HL0    Z0                 
#         PDG         Width
DECAY        37  1.70242620E+00   # H+    decays
#          BR          NDA       ID1       ID2       ID3       ID4
      1.20920207E-08    2           12       -11                       # H+     -->  NUE    E+                 
      5.10546903E-04    2           14       -13                       # H+     -->  NUM    MU+                
      1.46225274E-01    2           16       -15                       # H+     -->  NUT    TAU+               
      1.36160606E-05    2            2        -1                       # H+     -->  UP     DB                 
      5.50277950E-03    2            4        -3                       # H+     -->  CH     SB                 
      7.61600614E-01    2            6        -5                       # H+     -->  TP     BB                 
      8.54436979E-02    2      1000024   1000022                       # H+     -->  W1SS+  Z1SS               
      1.16423806E-04    2      1000024   1000023                       # H+     -->  W1SS+  Z2SS               
      5.86284033E-04    2           25        24                       # H+     -->  HL0    W+                 
      7.02149805E-07    2     -1000015   1000016                       # H+     -->  TAU1+  NUTL               
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

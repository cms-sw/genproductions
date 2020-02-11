
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
     1     2.50000000E+05   # Lambda scale of soft SSB
     2     5.00000000E+05   # M_mess overall messenger scale
     3     1.50000000E+01   # tan(beta)
     4     1.00000000E+00   # sign(mu)
     5     1.00000000E+00   # N_5 messenger index
     6     4.65363007E+02   # c_grav gravitino mass factor
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
        25     1.17999733E+02   #  h^0            
        35     1.24002979E+03   #  H^0            
        36     1.23185693E+03   #  A^0            
        37     1.24259900E+03   #  H^+            
   1000001     2.61508447E+03   #  dnl            
   1000002     2.61384961E+03   #  upl            
   1000003     2.61508447E+03   #  stl            
   1000004     2.61384985E+03   #  chl            
   1000005     2.46693018E+03   #  b1             
   1000006     2.26128564E+03   #  t1             
   1000011     8.81597290E+02   #  el-            
   1000012     8.72897217E+02   #  nuel           
   1000013     8.81597290E+02   #  mul-           
   1000014     8.72897217E+02   #  numl           
   1000015     4.35219086E+02   #  tau1           
   1000016     8.67999146E+02   #  nutl           
   1000021     1.91488025E+03   #  glss           
   1000022     3.57533295E+02   #  z1ss           
   1000023     6.83307983E+02   #  z2ss           
   1000024     6.83784058E+02   #  w1ss           
   1000025    -9.05128601E+02   #  z3ss           
   1000035     9.19473755E+02   #  z4ss           
   1000037     9.19835999E+02   #  w2ss           
   1000039     1.39936174E-05   #  gvss
   2000001     2.47962573E+03   #  dnr            
   2000002     2.49211694E+03   #  upr            
   2000003     2.47962573E+03   #  str            
   2000004     2.49211719E+03   #  chr            
   2000005     2.51501929E+03   #  b2             
   2000006     2.53192505E+03   #  t2             
   2000011     4.34054993E+02   #  er-            
   2000013     4.34054993E+02   #  mur-           
   2000015     8.77285889E+02   #  tau2           
Block ALPHA   # Effective Higgs mixing parameter
         -6.75075799E-02   # alpha
Block STOPMIX   # stop mixing matrix
  1  1     4.28860486E-02   # O_{11}
  1  2     9.99079943E-01   # O_{12}
  2  1    -9.99079943E-01   # O_{21}
  2  2     4.28860486E-02   # O_{22}
Block SBOTMIX   # sbottom mixing matrix
  1  1     1.65292487E-01   # O_{11}
  1  2     9.86244619E-01   # O_{12}
  2  1    -9.86244619E-01   # O_{21}
  2  2     1.65292487E-01   # O_{22}
Block STAUMIX   # stau mixing matrix
  1  1     3.76267545E-02   # O_{11}
  1  2     9.99291837E-01   # O_{12}
  2  1    -9.99291837E-01   # O_{21}
  2  2     3.76267545E-02   # O_{22}
Block NMIX   # neutralino mixing matrix
  1  1     9.97984231E-01   #
  1  2    -6.89227972E-03   #
  1  3     5.77147081E-02   #
  1  4    -2.54761092E-02   #
  2  1     2.19855569E-02   #
  2  2     9.68040943E-01   #
  2  3    -1.96418479E-01   #
  2  4     1.54379860E-01   #
  3  1     2.25289762E-02   #
  3  2    -3.08329072E-02   #
  3  3    -7.05587387E-01   #
  3  4    -7.07593262E-01   #
  4  1     5.51053658E-02   #
  4  2    -2.48794243E-01   #
  4  3    -6.78406477E-01   #
  4  4     6.89078569E-01   #
Block UMIX   # chargino U mixing matrix
  1  1    -9.62361097E-01   # U_{11}
  1  2     2.71774113E-01   # U_{12}
  2  1    -2.71774113E-01   # U_{21}
  2  2    -9.62361097E-01   # U_{22}
Block VMIX   # chargino V mixing matrix
  1  1    -9.77084339E-01   # V_{11}
  1  2     2.12852612E-01   # V_{12}
  2  1    -2.12852612E-01   # V_{21}
  2  2    -9.77084339E-01   # V_{22}
Block GAUGE Q=  2.31750708E+03   #
     1     3.57524991E-01   # g`
     2     6.52378619E-01   # g_2
     3     1.21928000E+00   # g_3
Block YU Q=  2.31750708E+03   #
  3  3     8.41834545E-01   # y_t
Block YD Q=  2.31750708E+03   #
  3  3     1.87577948E-01   # y_b
Block YE Q=  2.31750708E+03   #
  3  3     1.52813390E-01   # y_tau
Block HMIX Q=  2.31750708E+03   # Higgs mixing parameters
     1     8.93735779E+02   # mu(Q)
     2     1.43580379E+01   # tan(beta)(Q)
     3     2.51580200E+02   # Higgs vev at Q
     4     1.51747150E+06   # m_A^2(Q)
Block MSOFT Q=  2.31750708E+03   # DRbar SUSY breaking parameters
     1     3.65135162E+02   # M_1(Q)          
     2     6.70938660E+02   # M_2(Q)          
     3     1.73744238E+03   # M_3(Q)          
    21     6.81539125E+05   # MHd^2(Q)        
    22    -7.05620250E+05   # MHu^2(Q)        
    31     8.73744751E+02   # MeL(Q)          
    32     8.73744751E+02   # MmuL(Q)         
    33     8.68951111E+02   # MtauL(Q)        
    34     4.32798096E+02   # MeR(Q)          
    35     4.32798096E+02   # MmuR(Q)         
    36     4.26433502E+02   # MtauR(Q)        
    41     2.52878857E+03   # MqL1(Q)         
    42     2.52878857E+03   # MqL2(Q)         
    43     2.43536743E+03   # MqL3(Q)         
    44     2.40509375E+03   # MuR(Q)          
    45     2.40509375E+03   # McR(Q)          
    46     2.20535034E+03   # MtR(Q)          
    47     2.39202271E+03   # MdR(Q)          
    48     2.39202271E+03   # MsR(Q)          
    49     2.38333545E+03   # MbR(Q)          
Block AU Q=  2.31750708E+03   #
  1  1    -5.15050049E+02   # A_u
  2  2    -5.15050049E+02   # A_c
  3  3    -5.15050049E+02   # A_t
Block AD Q=  2.31750708E+03   #
  1  1    -5.74386963E+02   # A_d
  2  2    -5.74386963E+02   # A_s
  3  3    -5.74386963E+02   # A_b
Block AE Q=  2.31750708E+03   #
  1  1    -6.59766846E+01   # A_e
  2  2    -6.59766846E+01   # A_mu
  3  3    -6.59766846E+01   # A_tau
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
DECAY   1000021  4.84759137E-02   # GLSS  decays
#          BR          NDA       ID1       ID2       ID3       ID4
      5.51658832E-02    3      1000024         1        -2             # GLSS   -->  W1SS+  DN     UB          
      5.51658832E-02    3     -1000024         2        -1             # GLSS   -->  W1SS-  UP     DB          
      5.51658645E-02    3      1000024         3        -4             # GLSS   -->  W1SS+  ST     CB          
      5.51658645E-02    3     -1000024         4        -3             # GLSS   -->  W1SS-  CH     SB          
      5.54163605E-02    3      1000024         5        -6             # GLSS   -->  W1SS+  BT     TB          
      5.54163605E-02    3     -1000024         6        -5             # GLSS   -->  W1SS-  TP     BB          
      1.86104991E-03    3      1000037         1        -2             # GLSS   -->  W2SS+  DN     UB          
      1.86104991E-03    3     -1000037         2        -1             # GLSS   -->  W2SS-  UP     DB          
      1.86104933E-03    3      1000037         3        -4             # GLSS   -->  W2SS+  ST     CB          
      1.86104933E-03    3     -1000037         4        -3             # GLSS   -->  W2SS-  CH     SB          
      9.18762013E-02    3      1000037         5        -6             # GLSS   -->  W2SS+  BT     TB          
      9.18762013E-02    3     -1000037         6        -5             # GLSS   -->  W2SS-  TP     BB          
      2.35371044E-06    2      1000022        21                       # GLSS   -->  Z1SS   GL                 
      3.42734084E-02    3      1000022         2        -2             # GLSS   -->  Z1SS   UP     UB          
      1.01328362E-02    3      1000022         1        -1             # GLSS   -->  Z1SS   DN     DB          
      1.01328362E-02    3      1000022         3        -3             # GLSS   -->  Z1SS   ST     SB          
      3.42733860E-02    3      1000022         4        -4             # GLSS   -->  Z1SS   CH     CB          
      1.08577749E-02    3      1000022         5        -5             # GLSS   -->  Z1SS   BT     BB          
      4.69427630E-02    3      1000022         6        -6             # GLSS   -->  Z1SS   TP     TB          
      2.40617053E-04    2      1000023        21                       # GLSS   -->  Z2SS   GL                 
      2.77903322E-02    3      1000023         2        -2             # GLSS   -->  Z2SS   UP     UB          
      2.72511635E-02    3      1000023         1        -1             # GLSS   -->  Z2SS   DN     DB          
      2.72511635E-02    3      1000023         3        -3             # GLSS   -->  Z2SS   ST     SB          
      2.77903173E-02    3      1000023         4        -4             # GLSS   -->  Z2SS   CH     CB          
      3.48168537E-02    3      1000023         5        -5             # GLSS   -->  Z2SS   BT     BB          
      2.15395857E-02    3      1000023         6        -6             # GLSS   -->  Z2SS   TP     TB          
      2.88726646E-03    2      1000025        21                       # GLSS   -->  Z3SS   GL                 
      7.58192027E-06    3      1000025         2        -2             # GLSS   -->  Z3SS   UP     UB          
      9.19087324E-06    3      1000025         1        -1             # GLSS   -->  Z3SS   DN     DB          
      9.19087324E-06    3      1000025         3        -3             # GLSS   -->  Z3SS   ST     SB          
      7.58191709E-06    3      1000025         4        -4             # GLSS   -->  Z3SS   CH     CB          
      4.07392485E-03    3      1000025         5        -5             # GLSS   -->  Z3SS   BT     BB          
      6.37389347E-02    3      1000025         6        -6             # GLSS   -->  Z3SS   TP     TB          
      2.68866681E-03    2      1000035        21                       # GLSS   -->  Z4SS   GL                 
      9.34417767E-04    3      1000035         2        -2             # GLSS   -->  Z4SS   UP     UB          
      1.06454617E-03    3      1000035         1        -1             # GLSS   -->  Z4SS   DN     DB          
      1.06454617E-03    3      1000035         3        -3             # GLSS   -->  Z4SS   ST     SB          
      9.34417301E-04    3      1000035         4        -4             # GLSS   -->  Z4SS   CH     CB          
      4.95864172E-03    3      1000035         5        -5             # GLSS   -->  Z4SS   BT     BB          
      8.16327780E-02    3      1000035         6        -6             # GLSS   -->  Z4SS   TP     TB          
      3.12254346E-12    2      1000039        21                       # GLSS   -->  GVSS   GL                 
#         PDG         Width
DECAY   1000002  6.01964798E+01   # UPL   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      5.44844242E-03    2      1000022         2                       # UPL    -->  Z1SS   UP                 
      1.49910167E-01    2      1000023         2                       # UPL    -->  Z2SS   UP                 
      1.00965786E-04    2      1000025         2                       # UPL    -->  Z3SS   UP                 
      7.99622666E-03    2      1000035         2                       # UPL    -->  Z4SS   UP                 
      5.20970166E-01    2      1000021         2                       # UPL    -->  GLSS   UP                 
      3.02860230E-01    2      1000024         1                       # UPL    -->  W1SS+  DN                 
      1.27138430E-02    2      1000037         1                       # UPL    -->  W2SS+  DN                 
#         PDG         Width
DECAY   1000001  6.02270889E+01   # DNL   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      6.33560866E-03    2      1000022         1                       # DNL    -->  Z1SS   DN                 
      1.47450849E-01    2      1000023         1                       # DNL    -->  Z2SS   DN                 
      1.73070453E-04    2      1000025         1                       # DNL    -->  Z3SS   DN                 
      9.40840319E-03    2      1000035         1                       # DNL    -->  Z4SS   DN                 
      5.22068501E-01    2      1000021         1                       # DNL    -->  GLSS   DN                 
      2.93831825E-01    2     -1000024         2                       # DNL    -->  W1SS-  UP                 
      2.07317173E-02    2     -1000037         2                       # DNL    -->  W2SS-  UP                 
#         PDG         Width
DECAY   1000003  6.02270775E+01   # STL   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      6.33560959E-03    2      1000022         3                       # STL    -->  Z1SS   ST                 
      1.47450879E-01    2      1000023         3                       # STL    -->  Z2SS   ST                 
      1.73070497E-04    2      1000025         3                       # STL    -->  Z3SS   ST                 
      9.40840505E-03    2      1000035         3                       # STL    -->  Z4SS   ST                 
      5.22068560E-01    2      1000021         3                       # STL    -->  GLSS   ST                 
      2.93831706E-01    2     -1000024         4                       # STL    -->  W1SS-  CH                 
      2.07317099E-02    2     -1000037         4                       # STL    -->  W2SS-  CH                 
#         PDG         Width
DECAY   1000004  6.01964188E+01   # CHL   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      5.44844614E-03    2      1000022         4                       # CHL    -->  Z1SS   CH                 
      1.49910271E-01    2      1000023         4                       # CHL    -->  Z2SS   CH                 
      1.00965837E-04    2      1000025         4                       # CHL    -->  Z3SS   CH                 
      7.99623039E-03    2      1000035         4                       # CHL    -->  Z4SS   CH                 
      5.20969629E-01    2      1000021         4                       # CHL    -->  GLSS   CH                 
      3.02860588E-01    2      1000024         3                       # CHL    -->  W1SS+  ST                 
      1.27138570E-02    2      1000037         3                       # CHL    -->  W2SS+  ST                 
#         PDG         Width
DECAY   1000005  2.72881966E+01   # BT1   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      4.95845005E-02    2      1000022         5                       # BT1    -->  Z1SS   BT                 
      1.86964292E-02    2      1000023         5                       # BT1    -->  Z2SS   BT                 
      2.34666541E-02    2      1000025         5                       # BT1    -->  Z3SS   BT                 
      1.58909354E-02    2      1000035         5                       # BT1    -->  Z4SS   BT                 
      8.04683208E-01    2      1000021         5                       # BT1    -->  GLSS   BT                 
      3.70124206E-02    2     -1000024         6                       # BT1    -->  W1SS-  TP                 
      5.06086983E-02    2     -1000037         6                       # BT1    -->  W2SS-  TP                 
      5.71832388E-05    2          -24   1000006                       # BT1    -->  W-     TP1                
#         PDG         Width
DECAY   1000006  5.64742622E+01   # TP1   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      1.60070285E-01    2      1000021         6                       # TP1    -->  GLSS   TP                 
      8.49514827E-02    2      1000022         6                       # TP1    -->  Z1SS   TP                 
      1.38634071E-02    2      1000023         6                       # TP1    -->  Z2SS   TP                 
      1.89450249E-01    2      1000025         6                       # TP1    -->  Z3SS   TP                 
      1.71991140E-01    2      1000035         6                       # TP1    -->  Z4SS   TP                 
      2.69031748E-02    2      1000024         5                       # TP1    -->  W1SS+  BT                 
      3.52770239E-01    2      1000037         5                       # TP1    -->  W2SS+  BT                 
#         PDG         Width
DECAY   2000002  2.88733673E+01   # UPR   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      1.86410576E-01    2      1000022         2                       # UPR    -->  Z1SS   UP                 
      8.06635944E-05    2      1000023         2                       # UPR    -->  Z2SS   UP                 
      7.46273290E-05    2      1000025         2                       # UPR    -->  Z3SS   UP                 
      4.42156103E-04    2      1000035         2                       # UPR    -->  Z4SS   UP                 
      8.12991977E-01    2      1000021         2                       # UPR    -->  GLSS   UP                 
#         PDG         Width
DECAY   2000001  2.40342674E+01   # DNR   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      5.56814261E-02    2      1000022         1                       # DNR    -->  Z1SS   DN                 
      2.40651389E-05    2      1000023         1                       # DNR    -->  Z2SS   DN                 
      2.22324852E-05    2      1000025         1                       # DNR    -->  Z3SS   DN                 
      1.31709268E-04    2      1000035         1                       # DNR    -->  Z4SS   DN                 
      9.44140553E-01    2      1000021         1                       # DNR    -->  GLSS   DN                 
#         PDG         Width
DECAY   2000003  2.40342674E+01   # STR   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      5.56814261E-02    2      1000022         3                       # STR    -->  Z1SS   ST                 
      2.40651389E-05    2      1000023         3                       # STR    -->  Z2SS   ST                 
      2.22324852E-05    2      1000025         3                       # STR    -->  Z3SS   ST                 
      1.31709268E-04    2      1000035         3                       # STR    -->  Z4SS   ST                 
      9.44140553E-01    2      1000021         3                       # STR    -->  GLSS   ST                 
#         PDG         Width
DECAY   2000004  2.88732967E+01   # CHR   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      1.86410889E-01    2      1000022         4                       # CHR    -->  Z1SS   CH                 
      8.06637545E-05    2      1000023         4                       # CHR    -->  Z2SS   CH                 
      7.46274600E-05    2      1000025         4                       # CHR    -->  Z3SS   CH                 
      4.42156830E-04    2      1000035         4                       # CHR    -->  Z4SS   CH                 
      8.12991679E-01    2      1000021         4                       # CHR    -->  GLSS   CH                 
#         PDG         Width
DECAY   2000005  7.76647720E+01   # BT2   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      4.56690742E-03    2      1000022         5                       # BT2    -->  Z1SS   BT                 
      1.03475645E-01    2      1000023         5                       # BT2    -->  Z2SS   BT                 
      9.46096517E-03    2      1000025         5                       # BT2    -->  Z3SS   BT                 
      1.74965579E-02    2      1000035         5                       # BT2    -->  Z4SS   BT                 
      3.20129663E-01    2      1000021         5                       # BT2    -->  GLSS   BT                 
      2.14172855E-01    2     -1000024         6                       # BT2    -->  W1SS-  TP                 
      3.29266995E-01    2     -1000037         6                       # BT2    -->  W2SS-  TP                 
      1.43040472E-03    2          -24   1000006                       # BT2    -->  W-     TP1                
#         PDG         Width
DECAY   2000006  7.90398483E+01   # TP2   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      3.05560440E-01    2      1000021         6                       # TP2    -->  GLSS   TP                 
      2.17111588E-01    2      1000024         5                       # TP2    -->  W1SS+  BT                 
      3.06803733E-02    2      1000037         5                       # TP2    -->  W2SS+  BT                 
      8.59211199E-04    2           23   1000006                       # TP2    -->  Z0     TP1                
      2.92354682E-03    2           25   1000006                       # TP2    -->  HL0    TP1                
      4.09033103E-03    2      1000022         6                       # TP2    -->  Z1SS   TP                 
      1.11858658E-01    2      1000023         6                       # TP2    -->  Z2SS   TP                 
      1.61570773E-01    2      1000025         6                       # TP2    -->  Z3SS   TP                 
      1.65345013E-01    2      1000035         6                       # TP2    -->  Z4SS   TP                 
#         PDG         Width
DECAY   1000011  2.41930294E+00   # EL-   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      3.14126581E-01    2      1000022        11                       # EL-    -->  Z1SS   E-                 
      2.34881341E-01    2      1000023        11                       # EL-    -->  Z2SS   E-                 
      4.50991452E-01    2     -1000024        12                       # EL-    -->  W1SS-  NUE                
      2.49711320E-07    3      1000015        11       -15             # EL-    -->  TAU1-  E-     TAU+        
      2.69448947E-07    3     -1000015        11        15             # EL-    -->  TAU1+  E-     TAU-        
      1.29415845E-15    2           11   1000039                       # EL-    -->  E-     GVSS               
#         PDG         Width
DECAY   1000013  2.41930270E+00   # MUL-  decays
#          BR          NDA       ID1       ID2       ID3       ID4
      3.14126611E-01    2      1000022        13                       # MUL-   -->  Z1SS   MU-                
      2.34881341E-01    2      1000023        13                       # MUL-   -->  Z2SS   MU-                
      4.50991511E-01    2     -1000024        14                       # MUL-   -->  W1SS-  NUM                
      2.49711348E-07    3      1000015        13       -15             # MUL-   -->  TAU1-  MU-    TAU+        
      2.69448975E-07    3     -1000015        13        15             # MUL-   -->  TAU1+  MU-    TAU-        
      1.29415856E-15    2           13   1000039                       # MUL-   -->  MU-    GVSS               
#         PDG         Width
DECAY   1000015  2.32983649E-01   # TAU1- decays
#          BR          NDA       ID1       ID2       ID3       ID4
      1.00000000E+00    2      1000022        15                       # TAU1-  -->  Z1SS   TAU-               
      3.94014025E-16    2           15   1000039                       # TAU1-  -->  TAU-   GVSS               
#         PDG         Width
DECAY   1000012  2.33576202E+00   # NUEL  decays
#          BR          NDA       ID1       ID2       ID3       ID4
      3.36090803E-01    2      1000022        12                       # NUEL   -->  Z1SS   NUE                
      2.15542153E-01    2      1000023        12                       # NUEL   -->  Z2SS   NUE                
      4.48364526E-01    2      1000024        11                       # NUEL   -->  W1SS+  E-                 
      4.10165683E-07    3      1000015        12       -15             # NUEL   -->  TAU1-  NUE    TAU+        
      3.31887549E-07    3     -1000015        12        15             # NUEL   -->  TAU1+  NUE    TAU-        
      1.75750677E-06    3     -1000015        11        16             # NUEL   -->  TAU1+  E-     NUT         
      1.27559678E-15    2           12   1000039                       # NUEL   -->  NUE    GVSS               
#         PDG         Width
DECAY   1000014  2.33576179E+00   # NUML  decays
#          BR          NDA       ID1       ID2       ID3       ID4
      3.36090863E-01    2      1000022        14                       # NUML   -->  Z1SS   NUM                
      2.15542167E-01    2      1000023        14                       # NUML   -->  Z2SS   NUM                
      4.48364496E-01    2      1000024        13                       # NUML   -->  W1SS+  MU-                
      4.10165740E-07    3      1000015        14       -15             # NUML   -->  TAU1-  NUM    TAU+        
      3.31887577E-07    3     -1000015        14        15             # NUML   -->  TAU1+  NUM    TAU-        
      1.75989908E-06    3     -1000015        13        16             # NUML   -->  TAU1+  MU-    NUT         
      1.27559689E-15    2           14   1000039                       # NUML   -->  NUM    GVSS               
#         PDG         Width
DECAY   1000016  2.50592327E+00   # NUTL  decays
#          BR          NDA       ID1       ID2       ID3       ID4
      3.10091347E-01    2      1000022        16                       # NUTL   -->  Z1SS   NUT                
      1.92686424E-01    2      1000023        16                       # NUTL   -->  Z2SS   NUT                
      4.02074337E-01    2      1000024        15                       # NUTL   -->  W1SS+  TAU-               
      9.51466635E-02    2           24   1000015                       # NUTL   -->  W+     TAU1-              
      9.85379870E-07    3     -1000015        16        15             # NUTL   -->  TAU1+  NUT    TAU-        
      2.87206916E-07    3      1000015        16       -15             # NUTL   -->  TAU1-  NUT    TAU+        
      1.15599277E-15    2           16   1000039                       # NUTL   -->  NUT    GVSS               
#         PDG         Width
DECAY   2000011  2.27289721E-01   # ER-   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      1.00000000E+00    2      1000022        11                       # ER-    -->  Z1SS   E-                 
      3.98538637E-16    2           11   1000039                       # ER-    -->  E-     GVSS               
#         PDG         Width
DECAY   2000013  2.27289468E-01   # MUR-  decays
#          BR          NDA       ID1       ID2       ID3       ID4
      1.00000000E+00    2      1000022        13                       # MUR-   -->  Z1SS   MU-                
      3.98538875E-16    2           13   1000039                       # MUR-   -->  MU-    GVSS               
#         PDG         Width
DECAY   2000015  2.62751508E+00   # TAU2- decays
#          BR          NDA       ID1       ID2       ID3       ID4
      2.87490100E-01    2      1000022        15                       # TAU2-  -->  Z1SS   TAU-               
      2.08264172E-01    2      1000023        15                       # TAU2-  -->  Z2SS   TAU-               
      3.98410231E-01    2     -1000024        16                       # TAU2-  -->  W1SS-  NUT                
      4.77339886E-02    2           23   1000015                       # TAU2-  -->  Z0     TAU1-              
      5.81015833E-02    2           25   1000015                       # TAU2-  -->  HL0    TAU1-              
#         PDG         Width
DECAY   1000022  3.28910239E-17   # Z1SS  decays
#          BR          NDA       ID1       ID2       ID3       ID4
      7.92751074E-01    2      1000039        22                       # Z1SS   -->  GVSS   GM                 
      1.76879764E-02    3      1000039        11       -11             # Z1SS   -->  GVSS   E-     E+          
      1.89408511E-01    2      1000039        23                       # Z1SS   -->  GVSS   Z0                 
      1.52524037E-04    2      1000039        25                       # Z1SS   -->  GVSS   HL0                
#         PDG         Width
DECAY   1000023  4.74291220E-02   # Z2SS  decays
#          BR          NDA       ID1       ID2       ID3       ID4
      2.28228555E-06    2      1000022        22                       # Z2SS   -->  Z1SS   GM                 
      5.11024557E-02    2      1000022        23                       # Z2SS   -->  Z1SS   Z0                 
      5.90143429E-07    3      1000022         2        -2             # Z2SS   -->  Z1SS   UP     UB          
      6.69918052E-07    3      1000022         1        -1             # Z2SS   -->  Z1SS   DN     DB          
      6.69918052E-07    3      1000022         3        -3             # Z2SS   -->  Z1SS   ST     SB          
      5.90143145E-07    3      1000022         4        -4             # Z2SS   -->  Z1SS   CH     CB          
      2.31309878E-06    3      1000022         5        -5             # Z2SS   -->  Z1SS   BT     BB          
      3.39259946E-04    3      1000022        11       -11             # Z2SS   -->  Z1SS   E-     E+          
      3.39259946E-04    3      1000022        13       -13             # Z2SS   -->  Z1SS   MU-    MU+         
      3.48687812E-04    3      1000022        15       -15             # Z2SS   -->  Z1SS   TAU-   TAU+        
      3.62225983E-04    3      1000022        12       -12             # Z2SS   -->  Z1SS   NUE    ANUE        
      3.62225983E-04    3      1000022        14       -14             # Z2SS   -->  Z1SS   NUM    ANUM        
      3.76032986E-04    3      1000022        16       -16             # Z2SS   -->  Z1SS   NUT    ANUT        
      6.83032095E-01    2      1000022        25                       # Z2SS   -->  Z1SS   HL0                
      6.30113110E-03    2      2000011       -11                       # Z2SS   -->  ER-    E+                 
      6.30113110E-03    2     -2000011        11                       # Z2SS   -->  ER+    E-                 
      6.30113017E-03    2      2000013       -13                       # Z2SS   -->  MUR-   MU+                
      6.30113017E-03    2     -2000013        13                       # Z2SS   -->  MUR+   MU-                
      1.19262993E-01    2      1000015       -15                       # Z2SS   -->  TAU1-  TAU+               
      1.19262993E-01    2     -1000015        15                       # Z2SS   -->  TAU1+  TAU-               
      4.35320598E-15    2      1000039        22                       # Z2SS   -->  GVSS   GM                 
      1.01803957E-16    3      1000039        11       -11             # Z2SS   -->  GVSS   E-     E+          
      1.23013468E-14    2      1000039        23                       # Z2SS   -->  GVSS   Z0                 
      1.62111333E-16    2      1000039        25                       # Z2SS   -->  GVSS   HL0                
#         PDG         Width
DECAY   1000025  3.48313117E+00   # Z3SS  decays
#          BR          NDA       ID1       ID2       ID3       ID4
      9.43443439E-08    2      1000022        22                       # Z3SS   -->  Z1SS   GM                 
      2.95569208E-07    2      1000023        22                       # Z3SS   -->  Z2SS   GM                 
      2.67792344E-01    2      1000024       -24                       # Z3SS   -->  W1SS+  W-                 
      2.67792344E-01    2     -1000024        24                       # Z3SS   -->  W1SS-  W+                 
      1.40709102E-01    2      1000022        23                       # Z3SS   -->  Z1SS   Z0                 
      2.61112809E-01    2      1000023        23                       # Z3SS   -->  Z2SS   Z0                 
      2.31338115E-10    3      1000022         2        -2             # Z3SS   -->  Z1SS   UP     UB          
      6.36439443E-11    3      1000022         1        -1             # Z3SS   -->  Z1SS   DN     DB          
      6.36439443E-11    3      1000022         3        -3             # Z3SS   -->  Z1SS   ST     SB          
      2.31338032E-10    3      1000022         4        -4             # Z3SS   -->  Z1SS   CH     CB          
      1.28994850E-06    3      1000022         5        -5             # Z3SS   -->  Z1SS   BT     BB          
      2.24644651E-07    3      1000022        15       -15             # Z3SS   -->  Z1SS   TAU-   TAU+        
      1.66849382E-11    3      1000023         2        -2             # Z3SS   -->  Z2SS   UP     UB          
      2.80600508E-11    3      1000023         1        -1             # Z3SS   -->  Z2SS   DN     DB          
      2.80600508E-11    3      1000023         3        -3             # Z3SS   -->  Z2SS   ST     SB          
      1.66849312E-11    3      1000023         4        -4             # Z3SS   -->  Z2SS   CH     CB          
      7.58395657E-08    3      1000023         5        -5             # Z3SS   -->  Z2SS   BT     BB          
      1.23270851E-08    3      1000023        15       -15             # Z3SS   -->  Z2SS   TAU-   TAU+        
      2.10888255E-02    2      1000022        25                       # Z3SS   -->  Z1SS   HL0                
      3.46663245E-03    2      1000023        25                       # Z3SS   -->  Z2SS   HL0                
      4.90349862E-07    2      1000011       -11                       # Z3SS   -->  EL-    E+                 
      4.90349862E-07    2     -1000011        11                       # Z3SS   -->  EL+    E-                 
      4.90345087E-07    2      1000013       -13                       # Z3SS   -->  MUL-   MU+                
      4.90345087E-07    2     -1000013        13                       # Z3SS   -->  MUL+   MU-                
      1.98888098E-04    2      2000011       -11                       # Z3SS   -->  ER-    E+                 
      1.98888098E-04    2     -2000011        11                       # Z3SS   -->  ER+    E-                 
      1.98888098E-04    2      2000013       -13                       # Z3SS   -->  MUR-   MU+                
      1.98888098E-04    2     -2000013        13                       # Z3SS   -->  MUR+   MU-                
      1.84839126E-02    2      1000015       -15                       # Z3SS   -->  TAU1-  TAU+               
      1.84839126E-02    2     -1000015        15                       # Z3SS   -->  TAU1+  TAU-               
      1.18734555E-04    2      2000015       -15                       # Z3SS   -->  TAU2-  TAU+               
      1.18734555E-04    2     -2000015        15                       # Z3SS   -->  TAU2+  TAU-               
      4.99793805E-06    2      1000012       -12                       # Z3SS   -->  NUEL   ANUE               
      4.99793805E-06    2     -1000012        12                       # Z3SS   -->  ANUEL  NUE                
      4.99793805E-06    2      1000014       -14                       # Z3SS   -->  NUML   ANUM               
      4.99793805E-06    2     -1000014        14                       # Z3SS   -->  ANUML  NUM                
      6.59590523E-06    2      1000016       -16                       # Z3SS   -->  NUTL   ANUT               
      6.59590523E-06    2     -1000016        16                       # Z3SS   -->  ANUTL  NUT                
      2.45435038E-20    2      1000039        22                       # Z3SS   -->  GVSS   GM                 
      5.85412946E-22    3      1000039        11       -11             # Z3SS   -->  GVSS   E-     E+          
      2.15234751E-16    2      1000039        23                       # Z3SS   -->  GVSS   Z0                 
      2.71866606E-16    2      1000039        25                       # Z3SS   -->  GVSS   HL0                
#         PDG         Width
DECAY   1000035  4.07562637E+00   # Z4SS  decays
#          BR          NDA       ID1       ID2       ID3       ID4
      3.17573701E-08    2      1000022        22                       # Z4SS   -->  Z1SS   GM                 
      7.85405732E-08    2      1000023        22                       # Z4SS   -->  Z2SS   GM                 
      5.38367628E-10    2      1000025        22                       # Z4SS   -->  Z3SS   GM                 
      2.91030109E-01    2      1000024       -24                       # Z4SS   -->  W1SS+  W-                 
      2.91030109E-01    2     -1000024        24                       # Z4SS   -->  W1SS-  W+                 
      1.84919480E-02    2      1000022        23                       # Z4SS   -->  Z1SS   Z0                 
      4.85634478E-03    2      1000023        23                       # Z4SS   -->  Z2SS   Z0                 
      7.47165441E-09    3      1000022         2        -2             # Z4SS   -->  Z1SS   UP     UB          
      6.52819931E-09    3      1000022         1        -1             # Z4SS   -->  Z1SS   DN     DB          
      6.52819931E-09    3      1000022         3        -3             # Z4SS   -->  Z1SS   ST     SB          
      7.47165085E-09    3      1000022         4        -4             # Z4SS   -->  Z1SS   CH     CB          
      1.04973276E-06    3      1000022         5        -5             # Z4SS   -->  Z1SS   BT     BB          
      1.81694006E-07    3      1000022        15       -15             # Z4SS   -->  Z1SS   TAU-   TAU+        
      4.47598314E-09    3      1000023         2        -2             # Z4SS   -->  Z2SS   UP     UB          
      5.16750109E-09    3      1000023         1        -1             # Z4SS   -->  Z2SS   DN     DB          
      5.16750109E-09    3      1000023         3        -3             # Z4SS   -->  Z2SS   ST     SB          
      4.47598136E-09    3      1000023         4        -4             # Z4SS   -->  Z2SS   CH     CB          
      7.36268717E-08    3      1000023         5        -5             # Z4SS   -->  Z2SS   BT     BB          
      1.09366018E-08    3      1000023        15       -15             # Z4SS   -->  Z2SS   TAU-   TAU+        
      1.74758981E-08    3      1000025         2        -2             # Z4SS   -->  Z3SS   UP     UB          
      2.25361987E-08    3      1000025         1        -1             # Z4SS   -->  Z3SS   DN     DB          
      2.25361987E-08    3      1000025         3        -3             # Z4SS   -->  Z3SS   ST     SB          
      1.74758981E-08    3      1000025         4        -4             # Z4SS   -->  Z3SS   CH     CB          
      4.90987695E-09    3      1000025         5        -5             # Z4SS   -->  Z3SS   BT     BB          
      5.11220932E-09    3      1000025        11       -11             # Z4SS   -->  Z3SS   E-     E+          
      5.11220932E-09    3      1000025        13       -13             # Z4SS   -->  Z3SS   MU-    MU+         
      4.20287538E-09    3      1000025        15       -15             # Z4SS   -->  Z3SS   TAU-   TAU+        
      1.01716893E-08    3      1000025        12       -12             # Z4SS   -->  Z3SS   NUE    ANUE        
      1.01716893E-08    3      1000025        14       -14             # Z4SS   -->  Z3SS   NUM    ANUM        
      1.01716893E-08    3      1000025        16       -16             # Z4SS   -->  Z3SS   NUT    ANUT        
      1.19505405E-01    2      1000022        25                       # Z4SS   -->  Z1SS   HL0                
      2.37599552E-01    2      1000023        25                       # Z4SS   -->  Z2SS   HL0                
      1.47605126E-04    2      1000011       -11                       # Z4SS   -->  EL-    E+                 
      1.47605126E-04    2     -1000011        11                       # Z4SS   -->  EL+    E-                 
      1.47604587E-04    2      1000013       -13                       # Z4SS   -->  MUL-   MU+                
      1.47604587E-04    2     -1000013        13                       # Z4SS   -->  MUL+   MU-                
      1.05223386E-03    2      2000011       -11                       # Z4SS   -->  ER-    E+                 
      1.05223386E-03    2     -2000011        11                       # Z4SS   -->  ER+    E-                 
      1.05223386E-03    2      2000013       -13                       # Z4SS   -->  MUR-   MU+                
      1.05223386E-03    2     -2000013        13                       # Z4SS   -->  MUR+   MU-                
      1.47987204E-02    2      1000015       -15                       # Z4SS   -->  TAU1-  TAU+               
      1.47987204E-02    2     -1000015        15                       # Z4SS   -->  TAU1+  TAU-               
      3.84982093E-04    2      2000015       -15                       # Z4SS   -->  TAU2-  TAU+               
      3.84982093E-04    2     -2000015        15                       # Z4SS   -->  TAU2+  TAU-               
      3.60596430E-04    2      1000012       -12                       # Z4SS   -->  NUEL   ANUE               
      3.60596430E-04    2     -1000012        12                       # Z4SS   -->  ANUEL  NUE                
      3.60596430E-04    2      1000014       -14                       # Z4SS   -->  NUML   ANUM               
      3.60596430E-04    2     -1000014        14                       # Z4SS   -->  ANUML  NUM                
      4.38022602E-04    2      1000016       -16                       # Z4SS   -->  NUTL   ANUT               
      4.38022602E-04    2     -1000016        16                       # Z4SS   -->  ANUTL  NUT                
      4.85250534E-18    2      1000039        22                       # Z4SS   -->  GVSS   GM                 
      1.15868694E-19    3      1000039        11       -11             # Z4SS   -->  GVSS   E-     E+          
      2.99115048E-16    2      1000039        23                       # Z4SS   -->  GVSS   Z0                 
      1.82673931E-16    2      1000039        25                       # Z4SS   -->  GVSS   HL0                
#         PDG         Width
DECAY   1000024  4.21073064E-02   # W1SS+ decays
#          BR          NDA       ID1       ID2       ID3       ID4
      1.42800081E-06    3      1000022         2        -1             # W1SS+  -->  Z1SS   UP     DB          
      1.42800047E-06    3      1000022         4        -3             # W1SS+  -->  Z1SS   CH     SB          
      8.00273381E-04    3      1000022       -11        12             # W1SS+  -->  Z1SS   E+     NUE         
      8.00273381E-04    3      1000022       -13        14             # W1SS+  -->  Z1SS   MU+    NUM         
      8.27086333E-04    3      1000022       -15        16             # W1SS+  -->  Z1SS   TAU+   NUT         
      7.49020815E-01    2      1000022        24                       # W1SS+  -->  Z1SS   W+                 
      8.87552456E-13    3      1000023       -11        12             # W1SS+  -->  Z2SS   E+     NUE         
      8.87552456E-13    3      1000023       -13        14             # W1SS+  -->  Z2SS   MU+    NUM         
      2.48548627E-01    2     -1000015        16                       # W1SS+  -->  TAU1+  NUT                
#         PDG         Width
DECAY   1000037  3.68577957E+00   # W2SS+ decays
#          BR          NDA       ID1       ID2       ID3       ID4
      1.16908856E-08    3      1000022         2        -1             # W2SS+  -->  Z1SS   UP     DB          
      1.16908829E-08    3      1000022         4        -3             # W2SS+  -->  Z1SS   CH     SB          
      1.90867638E-07    3      1000022       -15        16             # W2SS+  -->  Z1SS   TAU+   NUT         
      1.36962697E-01    2      1000022        24                       # W2SS+  -->  Z1SS   W+                 
      3.62633856E-09    3      1000023         2        -1             # W2SS+  -->  Z2SS   UP     DB          
      3.62633790E-09    3      1000023         4        -3             # W2SS+  -->  Z2SS   CH     SB          
      1.79552746E-08    3      1000023       -15        16             # W2SS+  -->  Z2SS   TAU+   NUT         
      2.76769191E-01    2      1000023        24                       # W2SS+  -->  Z2SS   W+                 
      7.55331584E-08    3      1000025         2        -1             # W2SS+  -->  Z3SS   UP     DB          
      7.55331584E-08    3      1000025         4        -3             # W2SS+  -->  Z3SS   CH     SB          
      2.51776058E-08    3      1000025       -11        12             # W2SS+  -->  Z3SS   E+     NUE         
      2.51776058E-08    3      1000025       -13        14             # W2SS+  -->  Z3SS   MU+    NUM         
      2.51776076E-08    3      1000025       -15        16             # W2SS+  -->  Z3SS   TAU+   NUT         
      4.70765051E-04    2      1000012       -11                       # W2SS+  -->  NUEL   E+                 
      4.70763916E-04    2      1000014       -13                       # W2SS+  -->  NUML   MU+                
      1.19844917E-03    2      1000016       -15                       # W2SS+  -->  NUTL   TAU+               
      5.14292275E-04    2     -1000011        12                       # W2SS+  -->  EL+    NUE                
      5.14292275E-04    2     -1000013        14                       # W2SS+  -->  MUL+   NUM                
      3.07627581E-02    2     -1000015        16                       # W2SS+  -->  TAU1+  NUT                
      6.73996401E-04    2     -2000015        16                       # W2SS+  -->  TAU2+  NUT                
      2.76280433E-01    2      1000024        23                       # W2SS+  -->  W1SS+  Z0                 
      5.33041566E-09    3      1000024         1        -1             # W2SS+  -->  W1SS+  DN     DB          
      5.33041344E-09    3      1000024         3        -3             # W2SS+  -->  W1SS+  ST     SB          
      8.41248582E-09    3      1000024         2        -2             # W2SS+  -->  W1SS+  UP     UB          
      8.41248582E-09    3      1000024         4        -4             # W2SS+  -->  W1SS+  CH     CB          
      2.75381953E-01    2      1000024        25                       # W2SS+  -->  W1SS+  HL0                
#         PDG         Width
DECAY        25  3.18664592E-03   # HL0   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      6.47133191E-09    2           11       -11                       # HL0    -->  E-     E+                 
      2.73230020E-04    2           13       -13                       # HL0    -->  MU-    MU+                
      7.81511739E-02    2           15       -15                       # HL0    -->  TAU-   TAU+               
      8.11906284E-06    2            1        -1                       # HL0    -->  DN     DB                 
      3.28050833E-03    2            3        -3                       # HL0    -->  ST     SB                 
      7.02422082E-01    2            5        -5                       # HL0    -->  BT     BB                 
      2.52582731E-06    2            2        -2                       # HL0    -->  UP     UB                 
      4.73612025E-02    2            4        -4                       # HL0    -->  CH     CB                 
      2.58712471E-03    2           22        22                       # HL0    -->  GM     GM                 
      5.43262549E-02    2           21        21                       # HL0    -->  GL     GL                 
      5.70514612E-03    3           24        11       -12             # HL0    -->  W+     E-     ANUE        
      5.70514612E-03    3           24        13       -14             # HL0    -->  W+     MU-    ANUM        
      5.70514612E-03    3           24        15       -16             # HL0    -->  W+     TAU-   ANUT        
      1.71154384E-02    3           24        -2         1             # HL0    -->  W+     UB     DN          
      1.71154384E-02    3           24        -4         3             # HL0    -->  W+     CB     ST          
      5.70514612E-03    3          -24       -11        12             # HL0    -->  W-     E+     NUE         
      5.70514612E-03    3          -24       -13        14             # HL0    -->  W-     MU+    NUM         
      5.70514612E-03    3          -24       -15        16             # HL0    -->  W-     TAU+   NUT         
      1.71154384E-02    3          -24         2        -1             # HL0    -->  W-     UP     DB          
      1.71154384E-02    3          -24         4        -3             # HL0    -->  W-     CH     SB          
      6.08342991E-04    3           23        12       -12             # HL0    -->  Z0     NUE    ANUE        
      6.08342991E-04    3           23        14       -14             # HL0    -->  Z0     NUM    ANUM        
      6.08342991E-04    3           23        16       -16             # HL0    -->  Z0     NUT    ANUT        
      3.06172908E-04    3           23        11       -11             # HL0    -->  Z0     E-     E+          
      3.06172908E-04    3           23        13       -13             # HL0    -->  Z0     MU-    MU+         
      3.06172908E-04    3           23        15       -15             # HL0    -->  Z0     TAU-   TAU+        
      1.04892242E-03    3           23         2        -2             # HL0    -->  Z0     UP     UB          
      1.04892242E-03    3           23         4        -4             # HL0    -->  Z0     CH     CB          
      1.35127059E-03    3           23         1        -1             # HL0    -->  Z0     DN     DB          
      1.35127059E-03    3           23         3        -3             # HL0    -->  Z0     ST     SB          
      1.35127059E-03    3           23         5        -5             # HL0    -->  Z0     BT     BB          
#         PDG         Width
DECAY        35  4.30090523E+00   # HH0   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      1.10228191E-08    2           11       -11                       # HH0    -->  E-     E+                 
      4.65403311E-04    2           13       -13                       # HH0    -->  MU-    MU+                
      1.33297011E-01    2           15       -15                       # HH0    -->  TAU-   TAU+               
      1.34801312E-05    2            1        -1                       # HH0    -->  DN     DB                 
      5.44665521E-03    2            3        -3                       # HH0    -->  ST     SB                 
      7.95889497E-01    2            5        -5                       # HH0    -->  BT     BB                 
      8.76281964E-11    2            2        -2                       # HH0    -->  UP     UB                 
      1.20440041E-06    2            4        -4                       # HH0    -->  CH     CB                 
      5.83376177E-02    2            6        -6                       # HH0    -->  TP     TB                 
      1.66107881E-07    2           22        22                       # HH0    -->  GM     GM                 
      2.26253560E-05    2           21        21                       # HH0    -->  GL     GL                 
      1.24127968E-04    2           24       -24                       # HH0    -->  W+     W-                 
      6.32566080E-05    2           23        23                       # HH0    -->  Z0     Z0                 
      1.27426290E-03    2      1000022   1000022                       # HH0    -->  Z1SS   Z1SS               
      4.60397918E-03    2      1000022   1000023                       # HH0    -->  Z1SS   Z2SS               
      4.28673957E-04    2           25        25                       # HH0    -->  HL0    HL0                
      1.18878525E-05    2      2000011  -2000011                       # HH0    -->  ER-    ER+                
      1.18728030E-05    2      2000013  -2000013                       # HH0    -->  MUR-   MUR+               
      8.36988420E-06    2      1000015  -1000015                       # HH0    -->  TAU1-  TAU1+              
#         PDG         Width
DECAY        36  4.33829069E+00   # HA0   decays
#          BR          NDA       ID1       ID2       ID3       ID4
      1.08571756E-08    2           11       -11                       # HA0    -->  E-     E+                 
      4.58409544E-04    2           13       -13                       # HA0    -->  MU-    MU+                
      1.31294966E-01    2           15       -15                       # HA0    -->  TAU-   TAU+               
      1.32782598E-05    2            1        -1                       # HA0    -->  DN     DB                 
      5.36508905E-03    2            3        -3                       # HA0    -->  ST     SB                 
      7.83993244E-01    2            5        -5                       # HA0    -->  BT     BB                 
      8.39233891E-11    2            2        -2                       # HA0    -->  UP     UB                 
      1.15436330E-06    2            4        -4                       # HA0    -->  CH     CB                 
      5.97617365E-02    2            6        -6                       # HA0    -->  TP     TB                 
      5.30730688E-07    2           22        22                       # HA0    -->  GM     GM                 
      7.03835394E-05    2           21        21                       # HA0    -->  GL     GL                 
      2.11015344E-03    2      1000022   1000022                       # HA0    -->  Z1SS   Z1SS               
      1.68111753E-02    2      1000022   1000023                       # HA0    -->  Z1SS   Z2SS               
      1.19890035E-04    2           25        23                       # HA0    -->  HL0    Z0                 
#         PDG         Width
DECAY        37  3.95852423E+00   # H+    decays
#          BR          NDA       ID1       ID2       ID3       ID4
      1.20025332E-08    2           12       -11                       # H+     -->  NUE    E+                 
      5.06768585E-04    2           14       -13                       # H+     -->  NUM    MU+                
      1.45145699E-01    2           16       -15                       # H+     -->  NUT    TAU+               
      1.35152950E-05    2            2        -1                       # H+     -->  UP     DB                 
      5.46198944E-03    2            4        -3                       # H+     -->  CH     SB                 
      8.26016009E-01    2            6        -5                       # H+     -->  TP     BB                 
      2.27223039E-02    2      1000024   1000022                       # H+     -->  W1SS+  Z1SS               
      1.33699679E-04    2           25        24                       # H+     -->  HL0    W+                 
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

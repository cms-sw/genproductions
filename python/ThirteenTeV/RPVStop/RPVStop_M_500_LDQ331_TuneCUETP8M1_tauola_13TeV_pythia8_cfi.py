SLHA_TABLE = """
# SOFTSUSY3.2.4
# B.C. Allanach, Comput. Phys. Commun. 143 (2002) 305-331, hep-ph/0104145
# B.C. Allanach and M.A. Bernhardt, arXiv:0903.1805
# B.C. Allanach, M. Hanussek and C.H. Kom, arXiv:1109.3735
Block SPINFO         # Program information
     1   SOFTSUSY    # spectrum calculator
     2   3.2.4         # version number
Block MODSEL  # Select model
     1    1   # sugra
     4    1   # R-parity violating
Block SMINPUTS   # Standard Model inputs
     1    1.27925000e+02   # alpha_em^(-1)(MZ) SM MSbar
     2    1.16637000e-05   # G_Fermi
     3    1.17600000e-01   # alpha_s(MZ)MSbar
     4    9.11876000e+01   # MZ(pole)
     5    4.20000000e+00   # mb(mb)
     6    1.73300000e+02   # Mtop(pole)
     7    1.77699000e+00   # Mtau(pole)
Block MINPAR  # SUSY breaking input parameters
     3    2.50000000e+01   # tanb
     4    1.00000000e+00   # sign(mu)
     1    0.00000000e+00   # m0
     2    1.04000000e+03   # m12
     5    0.00000000e+00   # A0
Block RVLAMLLEIN                  # GUT scale LLE couplings 
  1 2 1   3.20000000e-02   # lambda_{121}
  2 1 1  -3.20000000e-02   # lambda_{211}
# Low energy data in SOFTSUSY: MIXING=1 TOLERANCE=1.00000000e-03
# mgut=1.19751896e+16 GeV
Block MASS   # Mass spectrum
#PDG code      mass              particle
        12     0.00000000e+00   # Mnu1(pole) normal hierarchy output
        14    -6.42418814e-49   # Mnu2(pole) normal hierarchy output
        16    -8.53937218e-12   # Mnu3(pole) normal hierarchy output
        24     8.05658637e+01   # MW
        25     1.19947771e+02   # h0
        35     1.12839107e+03   # H0
        36     1.12827155e+03   # A0
        37     1.13127113e+03   # H+
   1000001     2.05257742e+03   # ~d_L
   1000002     2.05117612e+03   # ~u_L
   1000003     2.05256092e+03   # ~s_L
   1000004     2.05115961e+03   # ~c_L
   1000005     1.86347413e+03   # ~b_1
   1000006     5.00000000e+02   # ~t_1
   1000011     6.89348255e+02   # ~e_L
   1000012     6.84513161e+02   # ~nue_L
   1000013     6.89297086e+02   # ~mu_L
   1000014     6.84481384e+02   # ~numu_L
   1000015     3.37046647e+02   # ~stau_1
   1000016     6.72974252e+02   # ~nu_tau_L
   1000021     2.25970059e+03   # ~g
   1000022     4.43273800e+02   # ~neutralino(1)
   1000023     8.33326197e+02   # ~neutralino(2)
   1000024     8.33511959e+02   # ~chargino(1)
   1000025    -1.16506398e+03   # ~neutralino(3)
   1000035     1.17431114e+03   # ~neutralino(4)
   1000037     1.17426378e+03   # ~chargino(2)
   2000001     1.95725891e+03   # ~d_R
   2000002     1.96643438e+03   # ~u_R
   2000003     1.95722882e+03   # ~s_R
   2000004     1.96642936e+03   # ~c_R
   2000005     1.91226273e+03   # ~b_2
   2000006     5.89630362e+03   # ~t_2
   2000011     3.89848754e+02   # ~e_R
   2000013     3.90994434e+02   # ~mu_R
   2000015     6.83829650e+02   # ~stau_2
   1000039     1.00000000e+19   # ~gravitino
# Higgs mixing
Block alpha   # Effective Higgs mixing parameter
          -4.19274059e-02   # alpha
Block stopmix  # stop mixing matrix
  1  1     2.79281177e-01   # O_{11}
  1  2     9.60209365e-01   # O_{12}
  2  1     9.60209365e-01   # O_{21}
  2  2    -2.79281177e-01   # O_{22}
Block sbotmix  # sbottom mixing matrix
  1  1     9.20381430e-01   # O_{11}
  1  2     3.91021767e-01   # O_{12}
  2  1    -3.91021767e-01   # O_{21}
  2  2     9.20381430e-01   # O_{22}
Block staumix  # stau mixing matrix
  1  1     1.51237634e-01   # O_{11}
  1  2     9.88497435e-01   # O_{12}
  2  1     9.88497435e-01   # O_{21}
  2  2    -1.51237634e-01   # O_{22}
Block nmix  # neutralino mixing matrix
  1  1     9.98752152e-01   # N_{1,1}
  1  2    -4.17675711e-03   # N_{1,2}
  1  3     4.58882321e-02   # N_{1,3}
  1  4    -1.92604001e-02   # N_{1,4}
  2  1     1.24061780e-02   # N_{2,1}
  2  2     9.85198015e-01   # N_{2,2}
  2  3    -1.37960495e-01   # N_{2,3}
  2  4     1.00984452e-01   # N_{2,4}
  3  1    -1.86861773e-02   # N_{3,1}
  3  2     2.65932151e-02   # N_{3,2}
  3  3     7.06044732e-01   # N_{3,3}
  3  4     7.07420995e-01   # N_{3,4}
  4  1    -4.46212022e-02   # N_{4,1}
  4  2     1.69293315e-01   # N_{4,2}
  4  3     6.93081531e-01   # N_{4,3}
  4  4    -6.99275849e-01   # N_{4,4}
Block Umix  # chargino U mixing matrix 
  1  1     9.81030146e-01   # U_{1,1}
  1  2    -1.93855234e-01   # U_{1,2}
  2  1     1.93855234e-01   # U_{2,1}
  2  2     9.81030146e-01   # U_{2,2}
Block Vmix  # chargino V mixing matrix 
  1  1     9.89698462e-01   # V_{1,1}
  1  2    -1.43167573e-01   # V_{1,2}
  2  1     1.43167573e-01   # V_{2,1}
  2  2     9.89698462e-01   # V_{2,2}
Block RVLAMLLE Q= 9.11876000e+01 # R-Parity violating LLE couplings 
  1 1 1    0.00000000e+00   # lambda_{111}
  1 1 2    0.00000000e+00   # lambda_{112}
  1 1 3    0.00000000e+00   # lambda_{113}
  1 2 1    0.00000000e+02   # lambda_{121}
  1 2 2    0.00000000e+00   # lambda_{122}
  1 2 3    0.00000000e+00   # lambda_{123}
  1 3 1    0.00000000e+00   # lambda_{131}
  1 3 2    0.00000000e+00   # lambda_{132}
  1 3 3    0.000000000+00   # lambda_{133}
  2 1 1    0.00000000e+00   # lambda_{211}
  2 1 2    0.00000000e+00   # lambda_{212}
  2 1 3    0.00000000e+00   # lambda_{213}
  2 2 1    0.00000000e+00   # lambda_{221}
  2 2 2    0.00000000e+00   # lambda_{222}
  2 2 3    0.00000000e+00   # lambda_{223}
  2 3 1    0.00000000e+00   # lambda_{231}
  2 3 2    0.00000000e+00   # lambda_{232}
  2 3 3    0.000000000+00   # lambda_{233}
  3 1 1    0.00000000e+00   # lambda_{311}
  3 1 2    0.00000000e+00   # lambda_{312}
  3 1 3    0.00000000e+00   # lambda_{313}
  3 2 1    0.00000000e+00   # lambda_{321}
  3 2 2    0.00000000e+00   # lambda_{322}
  3 2 3    0.00000000e+00   # lambda_{323}
  3 3 1    0.00000000e+00   # lambda_{331}
  3 3 2    0.00000000e+00   # lambda_{332}
  3 3 3    0.00000000e+00   # lambda_{333}
Block RVLAMLQD Q= 9.11876000e+01 # R-Parity violating LQD couplings 
  1 1 1    0.00000000e+00   # lambda'_{111}
  1 1 2    0.00000000e+00   # lambda'_{112}
  1 1 3    0.00000000e+00   # lambda'_{113}
  1 2 1    0.00000000e+00   # lambda'_{121}
  1 2 2    0.000000000+00   # lambda'_{122}
  1 2 3    0.00000000e+00   # lambda'_{123}
  1 3 1    0.00000000e+00   # lambda'_{131}
  1 3 2    0.00000000e+00   # lambda'_{132}
  1 3 3    0.00000000e+00   # lambda'_{133}
  2 1 1    0.00000000e+00   # lambda'_{211}
  2 1 2    0.00000000e+00   # lambda'_{212}
  2 1 3    0.00000000e+00   # lambda'_{213}
  2 2 1    0.00000000e+00   # lambda'_{221}
  2 2 2    0.00000000e+00   # lambda'_{222}
  2 2 3    0.00000000e+00   # lambda'_{223}
  2 3 1    0.00000000e+00   # lambda'_{231}
  2 3 2    0.00000000e+00   # lambda'_{232}
  2 3 3    0.00000000e+00   # lambda'_{233}
  3 1 1    0.00000000e+00   # lambda'_{311}
  3 1 2    0.00000000e+00   # lambda'_{312}
  3 1 3    0.00000000e+00   # lambda'_{313}
  3 2 1    0.00000000e+00   # lambda'_{321}
  3 2 2    0.00000000e+00   # lambda'_{322}
  3 2 3    0.00000000e+00   # lambda'_{323}
  3 3 1    1.00000000e+00   # lambda'_{331}
  3 3 2    0.00000000e+00   # lambda'_{332}
  3 3 3    0.00000000e+00   # lambda'_{333}
Block RVLAMUDD Q= 9.11876000e+01 # R-Parity violating UDD couplings 
  1 1 1    0.00000000e+00   # lambda''_{111}
  1 1 2    0.00000000e+00   # lambda''_{112}
  1 1 3    0.00000000e+00   # lambda''_{113}
  1 2 1    0.00000000e+00   # lambda''_{121}
  1 2 2    0.00000000e+00   # lambda''_{122}
  1 2 3    0.00000000e+00   # lambda''_{123}
  1 3 1    0.00000000e+00   # lambda''_{131}
  1 3 2    0.00000000e+00   # lambda''_{132}
  1 3 3    0.00000000e+00   # lambda''_{133}
  2 1 1    0.00000000e+00   # lambda''_{211}
  2 1 2    0.00000000e+00   # lambda''_{212}
  2 1 3    0.00000000e+00   # lambda''_{213}
  2 2 1    0.00000000e+00   # lambda''_{221}
  2 2 2    0.00000000e+00   # lambda''_{222}
  2 2 3    0.00000000e+00   # lambda''_{223}
  2 3 1    0.00000000e+00   # lambda''_{231}
  2 3 2    0.00000000e+00   # lambda''_{232}
  2 3 3    0.00000000e+00   # lambda''_{233}
  3 1 1    0.00000000e+00   # lambda''_{311}
  3 1 2    0.00000000e+00   # lambda''_{312}
  3 1 3    0.00000000e+00   # lambda''_{313}
  3 2 1    0.00000000e+00   # lambda''_{321}
  3 2 2    0.00000000e+00   # lambda''_{322}
  3 2 3    0.00000000e+00   # lambda''_{323}
  3 3 1    0.00000000e+00   # lambda''_{331}
  3 3 2    0.00000000e+00   # lambda''_{332}
  3 3 3    0.00000000e+00   # lambda''_{333}
Block RVT Q= 9.11876000e+01 # R-Parity violating LLE soft terms 
  1 1 1    0.00000000e+00   # T_{111}
  1 1 2    0.00000000e+00   # T_{112}
  1 1 3    0.00000000e+00   # T_{113}
  1 2 1   -3.13417017e+01   # T_{121}
  1 2 2    4.53713224e-23   # T_{122}
  1 2 3    0.00000000e+00   # T_{123}
  1 3 1    0.00000000e+00   # T_{131}
  1 3 2    0.00000000e+00   # T_{132}
  1 3 3    2.48753355e-22   # T_{133}
  2 1 1    3.13417017e+01   # T_{211}
  2 1 2   -4.53713224e-23   # T_{212}
  2 1 3    0.00000000e+00   # T_{213}
  2 2 1    0.00000000e+00   # T_{221}
  2 2 2    0.00000000e+00   # T_{222}
  2 2 3    0.00000000e+00   # T_{223}
  2 3 1    0.00000000e+00   # T_{231}
  2 3 2    0.00000000e+00   # T_{232}
  2 3 3   -1.72180354e-04   # T_{233}
  3 1 1    0.00000000e+00   # T_{311}
  3 1 2    0.00000000e+00   # T_{312}
  3 1 3   -2.48753355e-22   # T_{313}
  3 2 1    0.00000000e+00   # T_{321}
  3 2 2    0.00000000e+00   # T_{322}
  3 2 3    1.72180354e-04   # T_{323}
  3 3 1    0.00000000e+00   # T_{331}
  3 3 2    0.00000000e+00   # T_{332}
  3 3 3    0.00000000e+00   # T_{333}
Block RVTP Q= 9.11876000e+01 # R-Parity violating LQD soft terms 
  1 1 1    1.11961414e-24   # T'_{111}
  1 1 2    3.82100494e-50   # T'_{112}
  1 1 3    0.00000000e+00   # T'_{113}
  1 2 1    1.77606747e-51   # T'_{121}
  1 2 2    2.45132647e-23   # T'_{122}
  1 2 3    0.00000000e+00   # T'_{123}
  1 3 1    0.00000000e+00   # T'_{131}
  1 3 2    0.00000000e+00   # T'_{132}
  1 3 3    9.11150487e-22   # T'_{133}
  2 1 1   -7.74941233e-07   # T'_{211}
  2 1 2   -2.64470962e-32   # T'_{212}
  2 1 3    0.00000000e+00   # T'_{213}
  2 2 1   -1.22930558e-33   # T'_{221}
  2 2 2   -1.69668629e-05   # T'_{222}
  2 2 3    0.00000000e+00   # T'_{223}
  2 3 1    0.00000000e+00   # T'_{231}
  2 3 2    0.00000000e+00   # T'_{232}
  2 3 3   -6.30653937e-04   # T'_{233}
  3 1 1    0.00000000e+00   # T'_{311}
  3 1 2    0.00000000e+00   # T'_{312}
  3 1 3    0.00000000e+00   # T'_{313}
  3 2 1    0.00000000e+00   # T'_{321}
  3 2 2    0.00000000e+00   # T'_{322}
  3 2 3    0.00000000e+00   # T'_{323}
  3 3 1    0.00000000e+00   # T'_{331}
  3 3 2    0.00000000e+00   # T'_{332}
  3 3 3    0.00000000e+00   # T'_{333}
Block RVTPP Q= 9.11876000e+01 # R-Parity violating UDD soft terms 
  1 1 1    0.00000000e+00   # T''_{111}
  1 1 2    0.00000000e+00   # T''_{112}
  1 1 3    0.00000000e+00   # T''_{113}
  1 2 1    0.00000000e+00   # T''_{121}
  1 2 2    0.00000000e+00   # T''_{122}
  1 2 3    0.00000000e+00   # T''_{123}
  1 3 1    0.00000000e+00   # T''_{131}
  1 3 2    0.00000000e+00   # T''_{132}
  1 3 3    0.00000000e+00   # T''_{133}
  2 1 1    0.00000000e+00   # T''_{211}
  2 1 2    0.00000000e+00   # T''_{212}
  2 1 3    0.00000000e+00   # T''_{213}
  2 2 1    0.00000000e+00   # T''_{221}
  2 2 2    0.00000000e+00   # T''_{222}
  2 2 3    0.00000000e+00   # T''_{223}
  2 3 1    0.00000000e+00   # T''_{231}
  2 3 2    0.00000000e+00   # T''_{232}
  2 3 3    0.00000000e+00   # T''_{233}
  3 1 1    0.00000000e+00   # T''_{311}
  3 1 2    0.00000000e+00   # T''_{312}
  3 1 3    0.00000000e+00   # T''_{313}
  3 2 1    0.00000000e+00   # T''_{321}
  3 2 2    0.00000000e+00   # T''_{322}
  3 2 3    0.00000000e+00   # T''_{323}
  3 3 1    0.00000000e+00   # T''_{331}
  3 3 2    0.00000000e+00   # T''_{332}
  3 3 3    0.00000000e+00   # T''_{333}
Block RVKAPPA Q= 9.11876000e+01 # R-Parity violating kappa 
     1    8.32106790e-22   # kappa_{1}
     2   -5.75933628e-04   # kappa_{2}
     3    0.00000000e+00   # kappa_{3}
Block RVD Q= 9.11876000e+01 # R-Parity violating D 
     1   -4.88333045e-19   # D_{1}
     2    3.38030551e-01   # D_{2}
     3    0.00000000e+00   # D_{3}
Block RVSNVEV Q= 1.70273821e+03 # sneutrino VEVs D 
     1    1.14230019e-22   # SneutrinoVev_{1}
     2    1.98508447e-04   # SneutrinoVev_{2}
     3    0.00000000e+00   # SneutrinoVev_{3}
Block RVM2LH1 Q= 9.11876000e+01 # M2LH1 
     1    9.72084737e-19   # M2LH1_{1}
     2   -6.72929399e-01   # M2LH1_{2}
     3    0.00000000e+00   # M2LH1_{3}
Block RVNMIX Q= 9.11876000e+01 # neutrino-neutralino mixing matrix 
  1 1    0.00000000e+00   # N_{11}
  1 2    1.00000000e+00   # N_{12}
  1 3    4.28960393e-19   # N_{13}
  1 4   -1.71942418e-26   # N_{14}
  1 5   -4.08813208e-26   # N_{15}
  1 6    4.29464036e-25   # N_{16}
  1 7   -4.28801436e-25   # N_{17}
  2 1    0.00000000e+00   # N_{21}
  2 2   -4.28960393e-19   # N_{22}
  2 3    1.00000000e+00   # N_{23}
  2 4   -1.00292285e-07   # N_{24}
  2 5    1.33303664e-07   # N_{25}
  2 6   -3.00144791e-07   # N_{26}
  2 7    2.82091479e-07   # N_{27}
  3 1    1.00000000e+00   # N_{31}
  3 2    0.00000000e+00   # N_{32}
  3 3    0.00000000e+00   # N_{33}
  3 4    0.00000000e+00   # N_{34}
  3 5    0.00000000e+00   # N_{35}
  3 6    0.00000000e+00   # N_{36}
  3 7    0.00000000e+00   # N_{37}
  4 1    0.00000000e+00   # N_{41}
  4 2    1.04394519e-26   # N_{42}
  4 3    8.02858604e-08   # N_{43}
  4 4    9.98747776e-01   # N_{44}
  4 5    1.24491057e-02   # N_{45}
  4 6   -1.86931555e-02   # N_{46}
  4 7    4.47041917e-02   # N_{47}
  5 1    0.00000000e+00   # N_{51}
  5 2   -1.09872363e-26   # N_{52}
  5 3   -7.61318948e-08   # N_{53}
  5 4   -4.22292405e-03   # N_{54}
  5 5    9.85264268e-01   # N_{55}
  5 6    2.66016901e-02   # N_{56}
  5 7   -1.68904820e-01   # N_{57}
  6 1    0.00000000e+00   # N_{61}
  6 2   -7.89902027e-25   # N_{62}
  6 3    4.30405542e-07   # N_{63}
  6 4    4.59557286e-02   # N_{64}
  6 5   -1.37690051e-01   # N_{65}
  6 6    7.06044508e-01   # N_{66}
  6 7   -6.93131065e-01   # N_{67}
  7 1    0.00000000e+00   # N_{71}
  7 2   -2.94724002e-29   # N_{72}
  7 3   -3.03419619e-10   # N_{73}
  7 4   -1.93162359e-02   # N_{74}
  7 5    1.00701494e-01   # N_{75}
  7 6    7.07420715e-01   # N_{76}
  7 7    6.99315396e-01   # N_{77}
Block gauge Q= 1.70273821e+03  
     1     3.64249287e-01   # g'(Q)MSSM DRbar
     2     6.38859054e-01   # g(Q)MSSM DRbar
     3     1.03204524e+00   # g3(Q)MSSM DRbar
Block yu Q= 1.70273821e+03  
  1  1     7.11571130e-06   # YU_{11} (Q)MSSM DRbar
  2  2     3.25224982e-03   # YU_{22} (Q)MSSM DRbar
  3  3     8.30688405e-01   # YU_{33} (Q)MSSM DRbar
Block yd Q= 1.70273821e+03  
  1  1     3.42365706e-04   # YD_{11} (Q)MSSM DRbar
  2  2     7.49603532e-03   # YD_{22} (Q)MSSM DRbar
  3  3     3.07818157e-01   # YD_{33} (Q)MSSM DRbar
Block ye Q= 1.70273821e+03  
  1  1     7.01703518e-05   # YE_{11} (Q)MSSM DRbar
  2  2     1.43194064e-02   # YE_{22} (Q)MSSM DRbar
  3  3     2.56942413e-01   # YE_{33} (Q)MSSM DRbar
Block UPMNS Q= 1.70273821e+03 # neutrino mixing matrix:
  1  1     0.00000000e+00   # UPMNS_{11} matrix element
  1  2     1.00000000e+00   # UPMNS_{12} matrix element
  1  3     1.87375058e-18   # UPMNS_{13} matrix element
  2  1     0.00000000e+00   # UPMNS_{21} matrix element
  2  2     1.87375058e-18   # UPMNS_{22} matrix element
  2  3    -1.00000000e+00   # UPMNS_{23} matrix element
  3  1     1.00000000e+00   # UPMNS_{31} matrix element
  3  2     0.00000000e+00   # UPMNS_{32} matrix element
  3  3     0.00000000e+00   # UPMNS_{33} matrix element
Block hmix Q= 1.70273821e+03  # Higgs mixing parameters
     1     1.15893883e+03   # mu(Q)MSSM DRbar
     2     2.41572131e+01   # tan beta(Q)MSSM DRbar
     3     2.44007782e+02   # higgs vev(Q)MSSM DRbar
     4     1.49059154e+06   # mA^2(Q)MSSM DRbar
Block msoft Q= 1.70273821e+03 # MSSM DRbar SUSY breaking parameters
     1     4.52211639e+02   # M_1(Q)
     2     8.21799298e+02   # M_2(Q)
     3     2.20692798e+03   # M_3(Q)
    21     1.48682242e+05   # mH1^2(Q)
    22    -1.34464042e+06   # mH2^2(Q)
    31     6.76764511e+02   # meL(Q)
    32     6.76732474e+02   # mmuL(Q)
    33     6.66857111e+02   # mtauL(Q)
    34     3.78074193e+02   # meR(Q)
    35     3.79253489e+02   # mmuR(Q)
    36     3.39978037e+02   # mtauR(Q)
    41     1.98518187e+03   # mqL1(Q)
    42     1.98516509e+03   # mqL2(Q)
    43     1.82006542e+03   # mqL3(Q)
    44     1.90322814e+03   # muR(Q)
    45     1.90322305e+03   # mcR(Q)
    46     1.58879849e+03   # mtR(Q)
    47     1.89289941e+03   # mdR(Q)
    48     1.89286890e+03   # msR(Q)
    49     1.84364589e+03   # mbR(Q)
Block au Q= 1.70273821e+03  
  1  1    -2.21574740e+03   # Au(Q)MSSM DRbar
  2  2    -2.21572733e+03   # Ac(Q)MSSM DRbar
  3  3    -1.72697589e+03   # At(Q)MSSM DRbar
Block ad Q= 1.70273821e+03  
  1  1    -2.60972596e+03   # Ad(Q)MSSM DRbar
  2  2    -2.60968470e+03   # As(Q)MSSM DRbar
  3  3    -2.39067424e+03   # Ab(Q)MSSM DRbar
Block ae Q= 1.70273821e+03  
  1  1    -5.34812142e+02   # Ae(Q)MSSM DRbar
  2  2    -5.35386053e+02   # Amu(Q)MSSM DRbar
  3  3    -5.15700586e+02   # Atau(Q)MSSM DRbar
DECAY   1000006     5.19649555E+01   # stop1
#                    stop1 2-body decays
#          BR         NDA      ID1       ID2
1.00000000E-00    2     -15        1
"""

import FWCore.ParameterSet.Config as cms

from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.Pythia8CUEP8M1Settings_cfi import *
from GeneratorInterface.ExternalDecays.TauolaSettings_cff import *

generator = cms.EDFilter("Pythia8GeneratorFilter",
    comEnergy = cms.double(13000.0),
    filterEfficiency = cms.untracked.double(1.),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    pythiaPylistVerbosity = cms.untracked.int32(0),
    SLHATableForPythia8 = cms.string('%s' % SLHA_TABLE),
    ExternalDecays = cms.PSet(Tauola = cms.untracked.PSet(TauolaPolar, TauolaDefaultInputCards ),
                              parameterSets = cms.vstring('Tauola')),
    PythiaParameters = cms.PSet(
        pythia8CommonSettingsBlock,
        pythia8CUEP8M1SettingsBlock,
        processParameters = cms.vstring(
            'SUSY:all = off',
            'SUSY:gg2squarkantisquark = on',
            'SUSY:qqbar2squarkantisquark = on',
            'SUSY:idA = 1000006',
            'SUSY:idB = 1000006',
        ),
        parameterSets = cms.vstring(
            'pythia8CommonSettings',
            'pythia8CUEP8M1Settings',
            'processParameters'
        )
    )
)

ProductionFilterSequence = cms.Sequence(generator)

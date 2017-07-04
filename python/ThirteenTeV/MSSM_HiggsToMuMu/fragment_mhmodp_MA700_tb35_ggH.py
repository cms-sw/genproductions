COM_ENERGY = 13000.0 # GeV
CROSS_SECTION = 1 # pb
PROCESS = 'HiggsBSM:gg2H2 = on'
SLHA_TABLE = """BLOCK SPINFO
     1   FeynHiggs
     2   2.12.0
     2   built on ott 13, 2016
BLOCK MODSEL
         1                  0   # Model
         2                  1   # GridPts
         3                  0   # Content
         4                  0   # RPV
         5                  0   # CPV
         6                  0   # FV
BLOCK SMINPUTS
         1     1.28952828E+02   # invAlfaMZ
         2     1.16637000E-05   # GF
         3     1.19000000E-01   # AlfasMZ
         4     9.11876000E+01   # MZ
         5     4.16000000E+00   # Mb
         6     1.73200000E+02   # Mt
         7     1.77703000E+00   # Mtau
        11     5.10998902E-04   # Me
        13     1.05658357E-01   # Mmu
        21     6.00000000E-03   # Md
        22     3.00000000E-03   # Mu
        23     9.50000000E-02   # Ms
        24     1.28600000E+00   # Mc
BLOCK MINPAR
         3     3.50000000E+01   # TB
BLOCK EXTPAR
         0     0.00000000E+00   # Q
         1     9.54716519E+01   # M1
         2     2.00000000E+02   # M2
         3     1.50000000E+03   # M3
        11     1.50571429E+03   # At
        12     1.50571429E+03   # Ab
        13     1.50571429E+03   # Atau
        23     2.00000000E+02   # MUE
        25     3.50000000E+01   # TB
        26     7.00000000E+02   # MA0
        27     7.04601901E+02   # MHp
        31     5.00000000E+02   # MSL(1)
        32     5.00000000E+02   # MSL(2)
        33     1.00000000E+03   # MSL(3)
        34     5.00000000E+02   # MSE(1)
        35     5.00000000E+02   # MSE(2)
        36     1.00000000E+03   # MSE(3)
        41     1.50000000E+03   # MSQ(1)
        42     1.50000000E+03   # MSQ(2)
        43     1.00000000E+03   # MSQ(3)
        44     1.50000000E+03   # MSU(1)
        45     1.50000000E+03   # MSU(2)
        46     1.00000000E+03   # MSU(3)
        47     1.50000000E+03   # MSD(1)
        48     1.50000000E+03   # MSD(2)
        49     1.00000000E+03   # MSD(3)
BLOCK MASS
   1000012     4.95831819E+02   # MSf(1,1,1)
   1000011     5.02297238E+02   # MSf(1,2,1)
   2000011     5.01844888E+02   # MSf(2,2,1)
   1000002     1.49902682E+03   # MSf(1,3,1)
   2000002     1.49958921E+03   # MSf(2,3,1)
   1000001     1.50117796E+03   # MSf(1,4,1)
   2000001     1.50020517E+03   # MSf(2,4,1)
   1000014     4.95831819E+02   # MSf(1,1,2)
   1000013     5.02841028E+02   # MSf(1,2,2)
   2000013     5.01300040E+02   # MSf(2,2,2)
   1000004     1.49902736E+03   # MSf(1,3,2)
   2000004     1.49958977E+03   # MSf(2,3,2)
   1000003     1.50122273E+03   # MSf(1,4,2)
   2000003     1.50016037E+03   # MSf(2,4,2)
   1000016     9.97922438E+02   # MSf(1,1,3)
   1000015     9.96148789E+02   # MSf(1,2,3)
   2000015     1.00590492E+03   # MSf(2,2,3)
   1000006     8.76425392E+02   # MSf(1,3,3)
   2000006     1.13477936E+03   # MSf(2,3,3)
   1000005     9.90691930E+02   # MSf(1,4,3)
   2000005     1.01129043E+03   # MSf(2,4,3)
        25     1.25129112E+02   # Mh0
        35     6.99800049E+02   # MHH
        36     7.00000000E+02   # MA0
        37     7.05109732E+02   # MHp
   1000022     8.84634445E+01   # MNeu(1)
   1000023     1.52251009E+02   # MNeu(2)
   1000025    -2.10570836E+02   # MNeu(3)
   1000035     2.65328034E+02   # MNeu(4)
   1000024     1.49028243E+02   # MCha(1)
   1000037     2.65929050E+02   # MCha(2)
   1000021     1.50000000E+03   # MGl
BLOCK DMASS
         0     1.73200000E+02   # Q
        25     7.09964455E-01   # Delta Mh0
        35     1.58751220E-02   # Delta MHH
        36     0.00000000E+00   # Delta MA0
        37     3.90531956E-02   # Delta MHp
BLOCK NMIX
     1   1     9.33994702E-01   # ZNeu(1,1)
     1   2    -1.09656980E-01   # ZNeu(1,2)
     1   3     3.08336463E-01   # ZNeu(1,3)
     1   4    -1.43380155E-01   # ZNeu(1,4)
     2   1    -3.12041908E-01   # ZNeu(2,1)
     2   2    -6.93332183E-01   # ZNeu(2,2)
     2   3     5.13911163E-01   # ZNeu(2,3)
     2   4    -3.97260177E-01   # ZNeu(2,4)
     3   1     9.78754166E-02   # ZNeu(3,1)
     3   2    -1.36323085E-01   # ZNeu(3,2)
     3   3    -6.77823330E-01   # ZNeu(3,3)
     3   4    -7.15815585E-01   # ZNeu(3,4)
     4   1    -1.43889355E-01   # ZNeu(4,1)
     4   2     6.99057828E-01   # ZNeu(4,2)
     4   3     4.25886693E-01   # ZNeu(4,3)
     4   4    -5.56088600E-01   # ZNeu(4,4)
BLOCK UMIX
     1   1    -6.05777723E-01   # UCha(1,1)
     1   2     7.95633930E-01   # UCha(1,2)
     2   1     7.95633930E-01   # UCha(2,1)
     2   2     6.05777723E-01   # UCha(2,2)
BLOCK VMIX
     1   1    -7.95633930E-01   # VCha(1,1)
     1   2     6.05777723E-01   # VCha(1,2)
     2   1     6.05777723E-01   # VCha(2,1)
     2   2     7.95633930E-01   # VCha(2,2)
BLOCK STAUMIX
     1   1     6.98837569E-01   # USf(1,1)
     1   2     7.15280401E-01   # USf(1,2)
     2   1     7.15280401E-01   # USf(2,1)
     2   2    -6.98837569E-01   # USf(2,2)
BLOCK STOPMIX
     1   1     7.08253316E-01   # USf(1,1)
     1   2    -7.05958385E-01   # USf(1,2)
     2   1     7.05958385E-01   # USf(2,1)
     2   2     7.08253316E-01   # USf(2,2)
BLOCK SBOTMIX
     1   1     6.81625021E-01   # USf(1,1)
     1   2     7.31701667E-01   # USf(1,2)
     2   1     7.31701667E-01   # USf(2,1)
     2   2    -6.81625021E-01   # USf(2,2)
BLOCK ALPHA
              -3.03185655E-02   # Alpha
BLOCK DALPHA
               3.00244935E-05   # Delta Alpha
BLOCK HMIX Q= -0.99900000E+03
         1     2.00000000E+02   # MUE
         2     3.50000000E+01   # TB
BLOCK MSOFT Q=  0.00000000E+00
         1     9.54716519E+01   # M1
         2     2.00000000E+02   # M2
         3     1.50000000E+03   # M3
        31     5.00000000E+02   # MSL(1)
        32     5.00000000E+02   # MSL(2)
        33     1.00000000E+03   # MSL(3)
        34     5.00000000E+02   # MSE(1)
        35     5.00000000E+02   # MSE(2)
        36     1.00000000E+03   # MSE(3)
        41     1.50000000E+03   # MSQ(1)
        42     1.50000000E+03   # MSQ(2)
        43     1.00000000E+03   # MSQ(3)
        44     1.50000000E+03   # MSU(1)
        45     1.50000000E+03   # MSU(2)
        46     1.00000000E+03   # MSU(3)
        47     1.50000000E+03   # MSD(1)
        48     1.50000000E+03   # MSD(2)
        49     1.00000000E+03   # MSD(3)
BLOCK AE Q=  0.00000000E+00
     1   1     0.00000000E+00   # Af(1,1)
     2   2     0.00000000E+00   # Af(2,2)
     3   3     1.50571429E+03   # Af(3,3)
BLOCK AU Q=  0.00000000E+00
     1   1     0.00000000E+00   # Af(1,1)
     2   2     0.00000000E+00   # Af(2,2)
     3   3     1.50571429E+03   # Af(3,3)
BLOCK AD Q=  0.00000000E+00
     1   1     0.00000000E+00   # Af(1,1)
     2   2     0.00000000E+00   # Af(2,2)
     3   3     1.50571429E+03   # Af(3,3)
BLOCK YE Q=  0.00000000E+00
     1   1     1.02767518E-04   # Yf(1,1)
     2   2     2.12490616E-02   # Yf(2,2)
     3   3     3.57380343E-01   # Yf(3,3)
BLOCK YU Q=  0.00000000E+00
     1   1     1.72380888E-05   # Yf(1,1)
     2   2     7.38939405E-03   # Yf(2,2)
     3   3     9.95212324E-01   # Yf(3,3)
BLOCK YD Q=  0.00000000E+00
     1   1     1.16496188E-03   # Yf(1,1)
     2   2     1.84438794E-02   # Yf(2,2)
     3   3     7.52835453E-01   # Yf(3,3)
BLOCK VCKMIN
         1     2.25300000E-01   # lambda
         2     8.08000000E-01   # A
         3     1.32000000E-01   # rhobar
         4     3.41000000E-01   # etabar
BLOCK MSL2 Q=  0.00000000E+00
     1   1     2.50000000E+05   # MSL2(1,1)
     2   2     2.50000000E+05   # MSL2(2,2)
     3   3     1.00000000E+06   # MSL2(3,3)
BLOCK MSE2 Q=  0.00000000E+00
     1   1     2.50000000E+05   # MSE2(1,1)
     2   2     2.50000000E+05   # MSE2(2,2)
     3   3     1.00000000E+06   # MSE2(3,3)
BLOCK MSQ2 Q=  0.00000000E+00
     1   1     2.25000000E+06   # MSQ2(1,1)
     2   2     2.25000000E+06   # MSQ2(2,2)
     3   3     1.00000000E+06   # MSQ2(3,3)
BLOCK MSU2 Q=  0.00000000E+00
     1   1     2.25000000E+06   # MSU2(1,1)
     2   2     2.25000000E+06   # MSU2(2,2)
     3   3     1.00000000E+06   # MSU2(3,3)
BLOCK MSD2 Q=  0.00000000E+00
     1   1     2.25000000E+06   # MSD2(1,1)
     2   2     2.25000000E+06   # MSD2(2,2)
     3   3     1.00000000E+06   # MSD2(3,3)
BLOCK TE Q=  0.00000000E+00
     1   1     0.00000000E+00   # Tf(1,1)
     2   2     0.00000000E+00   # Tf(2,2)
     3   3     5.38112689E+02   # Tf(3,3)
BLOCK TU Q=  0.00000000E+00
     1   1     0.00000000E+00   # Tf(1,1)
     2   2     0.00000000E+00   # Tf(2,2)
     3   3     1.49850541E+03   # Tf(3,3)
BLOCK TD Q=  0.00000000E+00
     1   1     0.00000000E+00   # Tf(1,1)
     2   2     0.00000000E+00   # Tf(2,2)
     3   3     1.13355510E+03   # Tf(3,3)
BLOCK SELMIX
     1   1     9.99968990E-01   # UASf(1,1)
     1   4    -7.87520591E-03   # UASf(1,4)
     2   2     8.04210562E-01   # UASf(2,2)
     2   5    -5.94344489E-01   # UASf(2,5)
     3   3     6.98837569E-01   # UASf(3,3)
     3   6     7.15280401E-01   # UASf(3,6)
     4   1     7.87520591E-03   # UASf(4,1)
     4   4     9.99968990E-01   # UASf(4,4)
     5   2     5.94344489E-01   # UASf(5,2)
     5   5     8.04210562E-01   # UASf(5,5)
     6   3     7.15280401E-01   # UASf(6,3)
     6   6    -6.98837569E-01   # UASf(6,6)
BLOCK USQMIX
     1   1     1.00000000E+00   # UASf(1,1)
     1   4     1.01654749E-05   # UASf(1,4)
     2   2     9.99990506E-01   # UASf(2,2)
     2   5     4.35747613E-03   # UASf(2,5)
     3   3     7.08253316E-01   # UASf(3,3)
     3   6    -7.05958385E-01   # UASf(3,6)
     4   1    -1.01654749E-05   # UASf(4,1)
     4   4     1.00000000E+00   # UASf(4,4)
     5   2    -4.35747613E-03   # UASf(5,2)
     5   5     9.99990506E-01   # UASf(5,5)
     6   3     7.05958385E-01   # UASf(6,3)
     6   6     7.08253316E-01   # UASf(6,6)
BLOCK DSQMIX
     1   1     9.99903542E-01   # UASf(1,1)
     1   4    -1.38891136E-02   # UASf(1,4)
     2   2     9.78606924E-01   # UASf(2,2)
     2   5    -2.05738883E-01   # UASf(2,5)
     3   3     6.81625021E-01   # UASf(3,3)
     3   6     7.31701667E-01   # UASf(3,6)
     4   1     1.38891136E-02   # UASf(4,1)
     4   4     9.99903542E-01   # UASf(4,4)
     5   2     2.05738883E-01   # UASf(5,2)
     5   5     9.78606924E-01   # UASf(5,5)
     6   3     7.31701667E-01   # UASf(6,3)
     6   6    -6.81625021E-01   # UASf(6,6)
BLOCK CVHMIX
     1   1     9.99999703E-01   # UH(1,1)
     1   2     7.70992385E-04   # UH(1,2)
     1   3     0.00000000E+00   # UH(1,3)
     2   1    -7.70992385E-04   # UH(2,1)
     2   2     9.99999703E-01   # UH(2,2)
     2   3     0.00000000E+00   # UH(2,3)
     3   1     0.00000000E+00   # UH(3,1)
     3   2     0.00000000E+00   # UH(3,2)
     3   3     1.00000000E+00   # UH(3,3)
DECAY        25     4.11472715E-03   # Gamma(h0)
     2.24425795E-03   2        22        22   # BR(h0 -> photon photon)
     1.44673937E-03   2        22        23   # BR(h0 -> photon Z)
     2.65000231E-02   2        23        23   # BR(h0 -> Z Z)
     2.18351824E-01   2       -24        24   # BR(h0 -> W W)
     6.87778011E-02   2        21        21   # BR(h0 -> gluon gluon)
     5.46256698E-09   2       -11        11   # BR(h0 -> Electron electron)
     2.42984665E-04   2       -13        13   # BR(h0 -> Muon muon)
     6.96965811E-02   2       -15        15   # BR(h0 -> Tau tau)
     1.96082481E-07   2        -2         2   # BR(h0 -> Up up)
     2.71595199E-02   2        -4         4   # BR(h0 -> Charm charm)
     8.89698773E-07   2        -1         1   # BR(h0 -> Down down)
     2.23431501E-04   2        -3         3   # BR(h0 -> Strange strange)
     5.85355747E-01   2        -5         5   # BR(h0 -> Bottom bottom)
DECAY        35     1.73679072E+01   # Gamma(HH)
    -4.41641245E-07   2        22        22   # BR(HH -> photon photon)
    -1.57812086E-06   2        22        23   # BR(HH -> photon Z)
    -1.04994192E-05   2        23        23   # BR(HH -> Z Z)
    -1.93912910E-05   2       -24        24   # BR(HH -> W W)
    -5.51081817E-05   2        21        21   # BR(HH -> gluon gluon)
    -8.08865115E-09   2       -11        11   # BR(HH -> Electron electron)
     3.59984339E-04   2       -13        13   # BR(HH -> Muon muon)
    -1.00277182E-01   2       -15        15   # BR(HH -> Tau tau)
    -1.44864753E-13   2        -2         2   # BR(HH -> Up up)
    -2.00500554E-08   2        -4         4   # BR(HH -> Charm charm)
    -9.69699184E-04   2        -6         6   # BR(HH -> Top top)
    -8.89097104E-07   2        -1         1   # BR(HH -> Down down)
    -2.23268857E-04   2        -3         3   # BR(HH -> Strange strange)
    -5.19825162E-01   2        -5         5   # BR(HH -> Bottom bottom)
    -9.44861949E-02   2  -1000024   1000024   # BR(HH -> Chargino1 chargino1)
    -6.27774507E-02   2  -1000037   1000024   # BR(HH -> Chargino2 chargino1)
    -6.27774507E-02   2  -1000024   1000037   # BR(HH -> Chargino1 chargino2)
    -1.09508950E-02   2  -1000037   1000037   # BR(HH -> Chargino2 chargino2)
    -1.02556148E-02   2   1000022   1000022   # BR(HH -> neutralino1 neutralino1)
    -2.95127518E-02   2   1000022   1000023   # BR(HH -> neutralino1 neutralino2)
    -1.93836709E-02   2   1000022   1000025   # BR(HH -> neutralino1 neutralino3)
    -1.83712833E-05   2   1000022   1000035   # BR(HH -> neutralino1 neutralino4)
    -1.67986194E-02   2   1000023   1000023   # BR(HH -> neutralino2 neutralino2)
    -1.04382955E-02   2   1000023   1000025   # BR(HH -> neutralino2 neutralino3)
    -2.55200331E-03   2   1000023   1000035   # BR(HH -> neutralino2 neutralino4)
    -2.92789740E-03   2   1000025   1000025   # BR(HH -> neutralino3 neutralino3)
    -4.60643978E-02   2   1000025   1000035   # BR(HH -> neutralino3 neutralino4)
    -9.20805874E-03   2   1000035   1000035   # BR(HH -> neutralino4 neutralino4)
    -1.05095758E-04   2        25        25   # BR(HH -> h0 h0)
DECAY        36     1.71556532E+01   # Gamma(A0)
     9.89024430E-07   2        22        22   # BR(A0 -> photon photon)
     2.11356498E-06   2        22        23   # BR(A0 -> photon Z)
     8.29884868E-05   2        21        21   # BR(A0 -> gluon gluon)
     8.04560369E-09   2       -11        11   # BR(A0 -> Electron electron)
     3.58068507E-04   2       -13        13   # BR(A0 -> Muon muon)
     9.97447357E-02   2       -15        15   # BR(A0 -> Tau tau)
     1.30374091E-13   2        -2         2   # BR(A0 -> Up up)
     1.80492559E-08   2        -4         4   # BR(A0 -> Charm charm)
     1.15778261E-03   2        -6         6   # BR(A0 -> Top top)
     8.84319878E-07   2        -1         1   # BR(A0 -> Down down)
     2.22069226E-04   2        -3         3   # BR(A0 -> Strange strange)
     5.17085461E-01   2        -5         5   # BR(A0 -> Bottom bottom)
     1.25173023E-01   2  -1000024   1000024   # BR(A0 -> Chargino1 chargino1)
     3.77153432E-02   2  -1000037   1000024   # BR(A0 -> Chargino2 chargino1)
     3.77153432E-02   2  -1000024   1000037   # BR(A0 -> Chargino1 chargino2)
     3.22024692E-02   2  -1000037   1000037   # BR(A0 -> Chargino2 chargino2)
     1.17210761E-02   2   1000022   1000022   # BR(A0 -> neutralino1 neutralino1)
     3.63745499E-02   2   1000022   1000023   # BR(A0 -> neutralino1 neutralino2)
     1.41957321E-02   2   1000022   1000025   # BR(A0 -> neutralino1 neutralino3)
     9.37518761E-05   2   1000022   1000035   # BR(A0 -> neutralino1 neutralino4)
     2.29906470E-02   2   1000023   1000023   # BR(A0 -> neutralino2 neutralino2)
     6.34506998E-03   2   1000023   1000025   # BR(A0 -> neutralino2 neutralino3)
     3.95102498E-03   2   1000023   1000035   # BR(A0 -> neutralino2 neutralino4)
     4.10614402E-03   2   1000025   1000025   # BR(A0 -> neutralino3 neutralino3)
     2.31351479E-02   2   1000025   1000035   # BR(A0 -> neutralino3 neutralino4)
     2.56082386E-02   2   1000035   1000035   # BR(A0 -> neutralino4 neutralino4)
     1.73204425E-05   2        23        25   # BR(A0 -> Z h0)
     5.32220909E-37   2        25        25   # BR(A0 -> h0 h0)
DECAY        37     1.61837153E+01   # Gamma(Hp)
     8.83333646E-09   2       -11        12   # BR(Hp -> Electron nu_e)
     3.77652641E-04   2       -13        14   # BR(Hp -> Muon nu_mu)
     1.06824001E-01   2       -15        16   # BR(Hp -> Tau nu_tau)
     9.02774066E-07   2        -1         2   # BR(Hp -> Down up)
     1.03774662E-05   2        -3         2   # BR(Hp -> Strange up)
     5.74783962E-06   2        -5         2   # BR(Hp -> Bottom up)
     4.23561219E-08   2        -1         4   # BR(Hp -> Down charm)
     2.25924391E-04   2        -3         4   # BR(Hp -> Strange charm)
     8.04898154E-04   2        -5         4   # BR(Hp -> Bottom charm)
     9.78119272E-08   2        -1         6   # BR(Hp -> Down top)
     2.42375910E-06   2        -3         6   # BR(Hp -> Strange top)
     4.93176109E-01   2        -5         6   # BR(Hp -> Bottom top)
     5.20407901E-02   2   1000022   1000024   # BR(Hp -> neutralino1 chargino1)
     1.52297739E-03   2   1000022   1000037   # BR(Hp -> neutralino1 chargino2)
     8.75696552E-03   2   1000023   1000024   # BR(Hp -> neutralino2 chargino1)
     1.40536638E-01   2   1000023   1000037   # BR(Hp -> neutralino2 chargino2)
     5.60952932E-02   2   1000024   1000025   # BR(Hp -> chargino1 neutralino3)
     5.18951813E-02   2   1000025   1000037   # BR(Hp -> neutralino3 chargino2)
     8.68281748E-02   2   1000024   1000035   # BR(Hp -> chargino1 neutralino4)
     8.76746267E-04   2   1000035   1000037   # BR(Hp -> neutralino4 chargino2)
     1.90464130E-05   2        24        25   # BR(Hp -> W h0)
     3.51884185E-10   2        24        35   # BR(Hp -> W HH)
     2.90505307E-10   2        24        36   # BR(Hp -> W A0)
DECAY         6     1.37127534E+00   # Gamma(top)
     1.00000000E+00   2         5        24   # BR(top -> bottom W)
"""

import FWCore.ParameterSet.Config as cms

from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.Pythia8CUEP8M1Settings_cfi import *

generator = cms.EDFilter("Pythia8GeneratorFilter",
                         pythiaPylistVerbosity = cms.untracked.int32(1),                        
                         filterEfficiency = cms.untracked.double(1),
                         pythiaHepMCVerbosity = cms.untracked.bool(False),
                         SLHATableForPythia8 = cms.string('%s' % SLHA_TABLE),
                         comEnergy = cms.double(COM_ENERGY),
                         crossSection = cms.untracked.double(CROSS_SECTION),                         
                         maxEventsToPrint = cms.untracked.int32(1),
                         PythiaParameters = cms.PSet(
                             pythia8CommonSettingsBlock,
                             pythia8CUEP8M1SettingsBlock,
                             processParameters = cms.vstring(
                                 'Higgs:useBSM = on', 
                                 PROCESS, 
                                 'SLHA:allowUserOverride = off', 
                                 'SLHA:minMassSM = 100.', 
                                 'PhaseSpace:mHatMin = 56.0'
                             ),
                             parameterSets = cms.vstring(
                                 'pythia8CommonSettings',
                                 'pythia8CUEP8M1Settings',
                                 'processParameters'
                                 )
                             )
                         )

ProductionFilterSequence = cms.Sequence(generator)

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
        26     9.00000000E+02   # MA0
        27     9.03583886E+02   # MHp
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
        25     1.25131965E+02   # Mh0
        35     8.99709090E+02   # MHH
        36     9.00000000E+02   # MA0
        37     9.03856874E+02   # MHp
   1000022     8.84634445E+01   # MNeu(1)
   1000023     1.52251009E+02   # MNeu(2)
   1000025    -2.10570836E+02   # MNeu(3)
   1000035     2.65328034E+02   # MNeu(4)
   1000024     1.49028243E+02   # MCha(1)
   1000037     2.65929050E+02   # MCha(2)
   1000021     1.50000000E+03   # MGl
BLOCK DMASS
         0     1.73200000E+02   # Q
        25     7.06942848E-01   # Delta Mh0
        35     2.19398237E-02   # Delta MHH
        36     0.00000000E+00   # Delta MA0
        37     2.15435501E-02   # Delta MHp
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
              -2.96138887E-02   # Alpha
BLOCK DALPHA
               1.79061376E-05   # Delta Alpha
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
     1   1     9.99999895E-01   # UH(1,1)
     1   2     4.59027505E-04   # UH(1,2)
     1   3     0.00000000E+00   # UH(1,3)
     2   1    -4.59027505E-04   # UH(2,1)
     2   2     9.99999895E-01   # UH(2,2)
     2   3     0.00000000E+00   # UH(2,3)
     3   1     0.00000000E+00   # UH(3,1)
     3   2     0.00000000E+00   # UH(3,2)
     3   3     1.00000000E+00   # UH(3,3)
DECAY        25     4.00428449E-03   # Gamma(h0)
     2.30579596E-03   2        22        22   # BR(h0 -> photon photon)
     1.48692973E-03   2        22        23   # BR(h0 -> photon Z)
     2.72406690E-02   2        23        23   # BR(h0 -> Z Z)
     2.24441861E-01   2       -24        24   # BR(h0 -> W W)
     7.08366961E-02   2        21        21   # BR(h0 -> gluon gluon)
     5.35790849E-09   2       -11        11   # BR(h0 -> Electron electron)
     2.38329266E-04   2       -13        13   # BR(h0 -> Muon muon)
     6.84083109E-02   2       -15        15   # BR(h0 -> Tau tau)
     2.01503141E-07   2        -2         2   # BR(h0 -> Up up)
     2.79103393E-02   2        -4         4   # BR(h0 -> Charm charm)
     8.75072836E-07   2        -1         1   # BR(h0 -> Down down)
     2.19758857E-04   2        -3         3   # BR(h0 -> Strange strange)
     5.76910228E-01   2        -5         5   # BR(h0 -> Bottom bottom)
DECAY        35     2.32687188E+01   # Gamma(HH)
    -2.49899633E-07   2        22        22   # BR(HH -> photon photon)
    -9.59673513E-07   2        22        23   # BR(HH -> photon Z)
    -6.87477562E-06   2        23        23   # BR(HH -> Z Z)
    -1.03154437E-05   2       -24        24   # BR(HH -> W W)
    -3.23282936E-05   2        21        21   # BR(HH -> gluon gluon)
    -7.72299749E-09   2       -11        11   # BR(HH -> Electron electron)
     3.43737285E-04   2       -13        13   # BR(HH -> Muon muon)
    -9.59505081E-02   2       -15        15   # BR(HH -> Tau tau)
    -1.33036975E-13   2        -2         2   # BR(HH -> Up up)
    -1.84140751E-08   2        -4         4   # BR(HH -> Charm charm)
    -1.02473687E-03   2        -6         6   # BR(HH -> Top top)
    -8.21892619E-07   2        -1         1   # BR(HH -> Down down)
    -2.06393687E-04   2        -3         3   # BR(HH -> Strange strange)
    -4.79354519E-01   2        -5         5   # BR(HH -> Bottom bottom)
    -1.02924260E-01   2  -1000024   1000024   # BR(HH -> Chargino1 chargino1)
    -6.74252284E-02   2  -1000037   1000024   # BR(HH -> Chargino2 chargino1)
    -6.74252284E-02   2  -1000024   1000037   # BR(HH -> Chargino1 chargino2)
    -2.01396801E-02   2  -1000037   1000037   # BR(HH -> Chargino2 chargino2)
    -1.02496999E-02   2   1000022   1000022   # BR(HH -> neutralino1 neutralino1)
    -3.06842224E-02   2   1000022   1000023   # BR(HH -> neutralino1 neutralino2)
    -1.97324801E-02   2   1000022   1000025   # BR(HH -> neutralino1 neutralino3)
    -2.21435017E-05   2   1000022   1000035   # BR(HH -> neutralino1 neutralino4)
    -1.84233258E-02   2   1000023   1000023   # BR(HH -> neutralino2 neutralino2)
    -1.07393988E-02   2   1000023   1000025   # BR(HH -> neutralino2 neutralino3)
    -3.31164230E-03   2   1000023   1000035   # BR(HH -> neutralino2 neutralino4)
    -3.80082997E-03   2   1000025   1000025   # BR(HH -> neutralino3 neutralino3)
    -5.13042781E-02   2   1000025   1000035   # BR(HH -> neutralino3 neutralino4)
    -1.68289563E-02   2   1000035   1000035   # BR(HH -> neutralino4 neutralino4)
    -5.71540112E-05   2        25        25   # BR(HH -> h0 h0)
DECAY        36     2.29686922E+01   # Gamma(A0)
     5.97198849E-07   2        22        22   # BR(A0 -> photon photon)
     1.23924837E-06   2        22        23   # BR(A0 -> photon Z)
     5.69925854E-05   2        21        21   # BR(A0 -> gluon gluon)
     7.64987404E-09   2       -11        11   # BR(A0 -> Electron electron)
     3.40482683E-04   2       -13        13   # BR(A0 -> Muon muon)
     9.50432523E-02   2       -15        15   # BR(A0 -> Tau tau)
     1.24525009E-13   2        -2         2   # BR(A0 -> Up up)
     1.72365110E-08   2        -4         4   # BR(A0 -> Charm charm)
     1.13348530E-03   2        -6         6   # BR(A0 -> Top top)
     8.14073994E-07   2        -1         1   # BR(A0 -> Down down)
     2.04430332E-04   2        -3         3   # BR(A0 -> Strange strange)
     4.74827288E-01   2        -5         5   # BR(A0 -> Bottom bottom)
     1.25353993E-01   2  -1000024   1000024   # BR(A0 -> Chargino1 chargino1)
     4.87228448E-02   2  -1000037   1000024   # BR(A0 -> Chargino2 chargino1)
     4.87228448E-02   2  -1000024   1000037   # BR(A0 -> Chargino1 chargino2)
     3.83706769E-02   2  -1000037   1000037   # BR(A0 -> Chargino2 chargino2)
     1.14066942E-02   2   1000022   1000022   # BR(A0 -> neutralino1 neutralino1)
     3.60225926E-02   2   1000022   1000023   # BR(A0 -> neutralino1 neutralino2)
     1.55706274E-02   2   1000022   1000025   # BR(A0 -> neutralino1 neutralino3)
     9.98422444E-05   2   1000022   1000035   # BR(A0 -> neutralino1 neutralino4)
     2.30737696E-02   2   1000023   1000023   # BR(A0 -> neutralino2 neutralino2)
     7.47701165E-03   2   1000023   1000025   # BR(A0 -> neutralino2 neutralino3)
     4.25455391E-03   2   1000023   1000035   # BR(A0 -> neutralino2 neutralino4)
     4.36276005E-03   2   1000025   1000025   # BR(A0 -> neutralino3 neutralino3)
     3.44861523E-02   2   1000025   1000035   # BR(A0 -> neutralino3 neutralino4)
     3.04565449E-02   2   1000035   1000035   # BR(A0 -> neutralino4 neutralino4)
     1.04845497E-05   2        23        25   # BR(A0 -> Z h0)
     1.25594195E-36   2        25        25   # BR(A0 -> h0 h0)
DECAY        37     2.23775513E+01   # Gamma(Hp)
     8.18904764E-09   2       -11        12   # BR(Hp -> Electron nu_e)
     3.50107293E-04   2       -13        14   # BR(Hp -> Muon nu_mu)
     9.90329294E-02   2       -15        16   # BR(Hp -> Tau nu_tau)
     8.08331202E-07   2        -1         2   # BR(Hp -> Down up)
     9.32565813E-06   2        -3         2   # BR(Hp -> Strange up)
     5.16540802E-06   2        -5         2   # BR(Hp -> Bottom up)
     3.80632159E-08   2        -1         4   # BR(Hp -> Down charm)
     2.02290052E-04   2        -3         4   # BR(Hp -> Strange charm)
     7.23339288E-04   2        -5         4   # BR(Hp -> Bottom charm)
     9.23721694E-08   2        -1         6   # BR(Hp -> Down top)
     2.28900557E-06   2        -3         6   # BR(Hp -> Strange top)
     4.64139392E-01   2        -5         6   # BR(Hp -> Bottom top)
     5.07104446E-02   2   1000022   1000024   # BR(Hp -> neutralino1 chargino1)
     1.64170811E-03   2   1000022   1000037   # BR(Hp -> neutralino1 chargino2)
     8.79227565E-03   2   1000023   1000024   # BR(Hp -> neutralino2 chargino1)
     1.56609222E-01   2   1000023   1000037   # BR(Hp -> neutralino2 chargino2)
     5.86686377E-02   2   1000024   1000025   # BR(Hp -> chargino1 neutralino3)
     6.16294680E-02   2   1000025   1000037   # BR(Hp -> neutralino3 chargino2)
     9.63139822E-02   2   1000024   1000035   # BR(Hp -> chargino1 neutralino4)
     1.15748730E-03   2   1000035   1000037   # BR(Hp -> neutralino4 chargino2)
     1.09888770E-05   2        24        25   # BR(Hp -> W h0)
     7.42856081E-11   2        24        35   # BR(Hp -> W HH)
     5.16562094E-11   2        24        36   # BR(Hp -> W A0)
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

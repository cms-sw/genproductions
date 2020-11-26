COM_ENERGY = 13000.0 # GeV
CROSS_SECTION = 1 # pb
PROCESS = 'HiggsBSM:gg2A3 = on'
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
         3     4.00000000E+01   # TB
BLOCK EXTPAR
         0     0.00000000E+00   # Q
         1     9.54716519E+01   # M1
         2     2.00000000E+02   # M2
         3     1.50000000E+03   # M3
        11     1.50500000E+03   # At
        12     1.50500000E+03   # Ab
        13     1.50500000E+03   # Atau
        23     2.00000000E+02   # MUE
        25     4.00000000E+01   # TB
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
   1000012     4.95830217E+02   # MSf(1,1,1)
   1000011     5.02298124E+02   # MSf(1,2,1)
   2000011     5.01845584E+02   # MSf(2,2,1)
   1000002     1.49902645E+03   # MSf(1,3,1)
   2000002     1.49958905E+03   # MSf(2,3,1)
   1000001     1.50117846E+03   # MSf(1,4,1)
   2000001     1.50020519E+03   # MSf(2,4,1)
   1000014     4.95830217E+02   # MSf(1,1,2)
   1000013     5.02942810E+02   # MSf(1,2,2)
   2000013     5.01199509E+02   # MSf(2,2,2)
   1000004     1.49902699E+03   # MSf(1,3,2)
   2000004     1.49958961E+03   # MSf(2,3,2)
   1000003     1.50123564E+03   # MSf(1,4,2)
   2000003     1.50014798E+03   # MSf(2,4,2)
   1000016     9.97921642E+02   # MSf(1,1,3)
   1000015     9.95256404E+02   # MSf(1,2,3)
   2000015     1.00678866E+03   # MSf(2,2,3)
   1000006     8.76424939E+02   # MSf(1,3,3)
   2000006     1.13477901E+03   # MSf(2,3,3)
   1000005     9.88976645E+02   # MSf(1,4,3)
   2000005     1.01296833E+03   # MSf(2,4,3)
        25     1.25184010E+02   # Mh0
        35     6.99726864E+02   # MHH
        36     7.00000000E+02   # MA0
        37     7.05187926E+02   # MHp
   1000022     8.85696665E+01   # MNeu(1)
   1000023     1.52403239E+02   # MNeu(2)
   1000025    -2.10646406E+02   # MNeu(3)
   1000035     2.65145152E+02   # MNeu(4)
   1000024     1.49281133E+02   # MCha(1)
   1000037     2.65787171E+02   # MCha(2)
   1000021     1.50000000E+03   # MGl
BLOCK DMASS
         0     1.73200000E+02   # Q
        25     7.15928673E-01   # Delta Mh0
        35     2.87837044E-02   # Delta MHH
        36     0.00000000E+00   # Delta MA0
        37     5.99387860E-02   # Delta MHp
BLOCK NMIX
     1   1     9.34616473E-01   # ZNeu(1,1)
     1   2    -1.08169841E-01   # ZNeu(1,2)
     1   3     3.07485524E-01   # ZNeu(1,3)
     1   4    -1.42281363E-01   # ZNeu(1,4)
     2   1    -3.10165758E-01   # ZNeu(2,1)
     2   2    -6.93221305E-01   # ZNeu(2,2)
     2   3     5.14891896E-01   # ZNeu(2,3)
     2   4    -3.97652814E-01   # ZNeu(2,4)
     3   1     9.81909017E-02   # ZNeu(3,1)
     3   2    -1.36771096E-01   # ZNeu(3,2)
     3   3    -6.77763154E-01   # ZNeu(3,3)
     3   4    -7.15743893E-01   # ZNeu(3,4)
     4   1    -1.43693415E-01   # ZNeu(4,1)
     4   2     6.99311930E-01   # ZNeu(4,2)
     4   3     4.25412853E-01   # ZNeu(4,3)
     4   4    -5.56182463E-01   # ZNeu(4,4)
BLOCK UMIX
     1   1    -6.05392853E-01   # UCha(1,1)
     1   2     7.95926814E-01   # UCha(1,2)
     2   1     7.95926814E-01   # UCha(2,1)
     2   2     6.05392853E-01   # UCha(2,2)
BLOCK VMIX
     1   1    -7.95926814E-01   # VCha(1,1)
     1   2     6.05392853E-01   # VCha(1,2)
     2   1     6.05392853E-01   # VCha(2,1)
     2   2     7.95926814E-01   # VCha(2,2)
BLOCK STAUMIX
     1   1     7.00114782E-01   # USf(1,1)
     1   2     7.14030316E-01   # USf(1,2)
     2   1     7.14030316E-01   # USf(2,1)
     2   2    -7.00114782E-01   # USf(2,2)
BLOCK STOPMIX
     1   1     7.08253754E-01   # USf(1,1)
     1   2    -7.05957945E-01   # USf(1,2)
     2   1     7.05957945E-01   # USf(2,1)
     2   2     7.08253754E-01   # USf(2,2)
BLOCK SBOTMIX
     1   1     6.85277580E-01   # USf(1,1)
     1   2     7.28281978E-01   # USf(1,2)
     2   1     7.28281978E-01   # USf(2,1)
     2   2    -6.85277580E-01   # USf(2,2)
BLOCK ALPHA
              -2.65738537E-02   # Alpha
BLOCK DALPHA
               2.57365748E-05   # Delta Alpha
BLOCK HMIX Q= -0.99900000E+03
         1     2.00000000E+02   # MUE
         2     4.00000000E+01   # TB
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
     3   3     1.50500000E+03   # Af(3,3)
BLOCK AU Q=  0.00000000E+00
     1   1     0.00000000E+00   # Af(1,1)
     2   2     0.00000000E+00   # Af(2,2)
     3   3     1.50500000E+03   # Af(3,3)
BLOCK AD Q=  0.00000000E+00
     1   1     0.00000000E+00   # Af(1,1)
     2   2     0.00000000E+00   # Af(2,2)
     3   3     1.50500000E+03   # Af(3,3)
BLOCK YE Q=  0.00000000E+00
     1   1     1.17437365E-04   # Yf(1,1)
     2   2     2.42823205E-02   # Yf(2,2)
     3   3     4.08395636E-01   # Yf(3,3)
BLOCK YU Q=  0.00000000E+00
     1   1     1.72364410E-05   # Yf(1,1)
     2   2     7.38868770E-03   # Yf(2,2)
     3   3     9.95117192E-01   # Yf(3,3)
BLOCK YD Q=  0.00000000E+00
     1   1     1.32471689E-03   # Yf(1,1)
     2   2     2.09729285E-02   # Yf(2,2)
     3   3     8.48176212E-01   # Yf(3,3)
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
     3   3     6.14635432E+02   # Tf(3,3)
BLOCK TU Q=  0.00000000E+00
     1   1     0.00000000E+00   # Tf(1,1)
     2   2     0.00000000E+00   # Tf(2,2)
     3   3     1.49765137E+03   # Tf(3,3)
BLOCK TD Q=  0.00000000E+00
     1   1     0.00000000E+00   # Tf(1,1)
     2   2     0.00000000E+00   # Tf(2,2)
     3   3     1.27650520E+03   # Tf(3,3)
BLOCK SELMIX
     1   1     9.99959530E-01   # UASf(1,1)
     1   4    -8.99653667E-03   # UASf(1,4)
     2   2     7.93582340E-01   # UASf(2,2)
     2   5    -6.08462874E-01   # UASf(2,5)
     3   3     7.00114782E-01   # UASf(3,3)
     3   6     7.14030316E-01   # UASf(3,6)
     4   1     8.99653667E-03   # UASf(4,1)
     4   4     9.99959530E-01   # UASf(4,4)
     5   2     6.08462874E-01   # UASf(5,2)
     5   5     7.93582340E-01   # UASf(5,5)
     6   3     7.14030316E-01   # UASf(6,3)
     6   6    -7.00114782E-01   # UASf(6,6)
BLOCK USQMIX
     1   1     1.00000000E+00   # UASf(1,1)
     1   4     8.89138756E-06   # UASf(1,4)
     2   2     9.99992737E-01   # UASf(2,2)
     2   5     3.81135842E-03   # UASf(2,5)
     3   3     7.08253754E-01   # UASf(3,3)
     3   6    -7.05957945E-01   # UASf(3,6)
     4   1    -8.89138756E-06   # UASf(4,1)
     4   4     1.00000000E+00   # UASf(4,4)
     5   2    -3.81135842E-03   # UASf(5,2)
     5   5     9.99992737E-01   # UASf(5,5)
     6   3     7.05957945E-01   # UASf(6,3)
     6   6     7.08253754E-01   # UASf(6,6)
BLOCK DSQMIX
     1   1     9.99875363E-01   # UASf(1,1)
     1   4    -1.57879047E-02   # UASf(1,4)
     2   2     9.73240216E-01   # UASf(2,2)
     2   5    -2.29790084E-01   # UASf(2,5)
     3   3     6.85277580E-01   # UASf(3,3)
     3   6     7.28281978E-01   # UASf(3,6)
     4   1     1.57879047E-02   # UASf(4,1)
     4   4     9.99875363E-01   # UASf(4,4)
     5   2     2.29790084E-01   # UASf(5,2)
     5   5     9.73240216E-01   # UASf(5,5)
     6   3     7.28281978E-01   # UASf(6,3)
     6   6    -6.85277580E-01   # UASf(6,6)
BLOCK CVHMIX
     1   1     9.99999743E-01   # UH(1,1)
     1   2     7.17617077E-04   # UH(1,2)
     1   3     0.00000000E+00   # UH(1,3)
     2   1    -7.17617077E-04   # UH(2,1)
     2   2     9.99999743E-01   # UH(2,2)
     2   3     0.00000000E+00   # UH(2,3)
     3   1     0.00000000E+00   # UH(3,1)
     3   2     0.00000000E+00   # UH(3,2)
     3   3     1.00000000E+00   # UH(3,3)
DECAY        25     4.12379898E-03   # Gamma(h0)
     2.24119103E-03   2        22        22   # BR(h0 -> photon photon)
     1.44951005E-03   2        22        23   # BR(h0 -> photon Z)
     2.66233445E-02   2        23        23   # BR(h0 -> Z Z)
     2.19130276E-01   2       -24        24   # BR(h0 -> W W)
     6.87059988E-02   2        21        21   # BR(h0 -> gluon gluon)
     5.47458571E-09   2       -11        11   # BR(h0 -> Electron electron)
     2.43519248E-04   2       -13        13   # BR(h0 -> Muon muon)
     6.97969999E-02   2       -15        15   # BR(h0 -> Tau tau)
     1.95727340E-07   2        -2         2   # BR(h0 -> Up up)
     2.71103594E-02   2        -4         4   # BR(h0 -> Charm charm)
     8.89998037E-07   2        -1         1   # BR(h0 -> Down down)
     2.23506095E-04   2        -3         3   # BR(h0 -> Strange strange)
     5.84474203E-01   2        -5         5   # BR(h0 -> Bottom bottom)
DECAY        35     2.03766804E+01   # Gamma(HH)
     3.51769850E-07   2        22        22   # BR(HH -> photon photon)
     1.32185914E-06   2        22        23   # BR(HH -> photon Z)
     7.24277168E-06   2        23        23   # BR(HH -> Z Z)
     1.33775254E-05   2       -24        24   # BR(HH -> W W)
     6.05276504E-05   2        21        21   # BR(HH -> gluon gluon)
     9.09737322E-09   2       -11        11   # BR(HH -> Electron electron)
     4.04877417E-04   2       -13        13   # BR(HH -> Muon muon)
     1.12243338E-01   2       -15        15   # BR(HH -> Tau tau)
     9.18712881E-14   2        -2         2   # BR(HH -> Up up)
     1.27151875E-08   2        -4         4   # BR(HH -> Charm charm)
     5.97681617E-04   2        -6         6   # BR(HH -> Top top)
     9.81250340E-07   2        -1         1   # BR(HH -> Down down)
     2.46406191E-04   2        -3         3   # BR(HH -> Strange strange)
     5.64517519E-01   2        -5         5   # BR(HH -> Bottom bottom)
     8.09271034E-02   2  -1000024   1000024   # BR(HH -> Chargino1 chargino1)
     5.31008955E-02   2  -1000037   1000024   # BR(HH -> Chargino2 chargino1)
     5.31008955E-02   2  -1000024   1000037   # BR(HH -> Chargino1 chargino2)
     9.45348555E-03   2  -1000037   1000037   # BR(HH -> Chargino2 chargino2)
     8.69054651E-03   2   1000022   1000022   # BR(HH -> neutralino1 neutralino1)
     2.52577847E-02   2   1000022   1000023   # BR(HH -> neutralino1 neutralino2)
     1.62879447E-02   2   1000022   1000025   # BR(HH -> neutralino1 neutralino3)
     1.72984457E-05   2   1000022   1000035   # BR(HH -> neutralino1 neutralino4)
     1.44982917E-02   2   1000023   1000023   # BR(HH -> neutralino2 neutralino2)
     8.79217660E-03   2   1000023   1000025   # BR(HH -> neutralino2 neutralino3)
     2.19476361E-03   2   1000023   1000035   # BR(HH -> neutralino2 neutralino4)
     2.49054185E-03   2   1000025   1000025   # BR(HH -> neutralino3 neutralino3)
     3.90883977E-02   2   1000025   1000035   # BR(HH -> neutralino3 neutralino4)
     7.93292684E-03   2   1000035   1000035   # BR(HH -> neutralino4 neutralino4)
     7.33000284E-05   2        25        25   # BR(HH -> h0 h0)
DECAY        36     2.00946961E+01   # Gamma(A0)
    -8.76793189E-07   2        22        22   # BR(A0 -> photon photon)
    -1.82807502E-06   2        22        23   # BR(A0 -> photon Z)
    -8.56812972E-05   2        21        21   # BR(A0 -> gluon gluon)
    -9.05128788E-09   2       -11        11   # BR(A0 -> Electron electron)
     4.02826397E-04   2       -13        13   # BR(A0 -> Muon muon)
    -1.11676141E-01   2       -15        15   # BR(A0 -> Tau tau)
    -8.23061311E-14   2        -2         2   # BR(A0 -> Up up)
    -1.13955275E-08   2        -4         4   # BR(A0 -> Charm charm)
    -7.09193422E-04   2        -6         6   # BR(A0 -> Top top)
    -9.76215227E-07   2        -1         1   # BR(A0 -> Down down)
    -2.45141811E-04   2        -3         3   # BR(A0 -> Strange strange)
    -5.61678978E-01   2        -5         5   # BR(A0 -> Bottom bottom)
    -1.06561393E-01   2  -1000024   1000024   # BR(A0 -> Chargino1 chargino1)
    -3.24141892E-02   2  -1000037   1000024   # BR(A0 -> Chargino2 chargino1)
    -3.24141892E-02   2  -1000024   1000037   # BR(A0 -> Chargino1 chargino2)
    -2.71310950E-02   2  -1000037   1000037   # BR(A0 -> Chargino2 chargino2)
    -9.88143051E-03   2   1000022   1000022   # BR(A0 -> neutralino1 neutralino1)
    -3.08862513E-02   2   1000022   1000023   # BR(A0 -> neutralino1 neutralino2)
    -1.21759787E-02   2   1000022   1000025   # BR(A0 -> neutralino1 neutralino3)
    -7.43813975E-05   2   1000022   1000035   # BR(A0 -> neutralino1 neutralino4)
    -1.96617848E-02   2   1000023   1000023   # BR(A0 -> neutralino2 neutralino2)
    -5.48749237E-03   2   1000023   1000025   # BR(A0 -> neutralino2 neutralino3)
    -3.39971988E-03   2   1000023   1000035   # BR(A0 -> neutralino2 neutralino4)
    -3.55536145E-03   2   1000025   1000025   # BR(A0 -> neutralino3 neutralino3)
    -1.98902907E-02   2   1000025   1000035   # BR(A0 -> neutralino3 neutralino4)
    -2.16528076E-02   2   1000035   1000035   # BR(A0 -> neutralino4 neutralino4)
    -1.19710918E-05   2        23        25   # BR(A0 -> Z h0)
    -4.77113615E-36   2        25        25   # BR(A0 -> h0 h0)
DECAY        37     1.88537021E+01   # Gamma(Hp)
     9.90463361E-09   2       -11        12   # BR(Hp -> Electron nu_e)
     4.23453930E-04   2       -13        14   # BR(Hp -> Muon nu_mu)
     1.19779497E-01   2       -15        16   # BR(Hp -> Tau nu_tau)
     1.00232301E-06   2        -1         2   # BR(Hp -> Down up)
     1.15215681E-05   2        -3         2   # BR(Hp -> Strange up)
     6.26445589E-06   2        -5         2   # BR(Hp -> Bottom up)
     4.65709741E-08   2        -1         4   # BR(Hp -> Down charm)
     2.50821905E-04   2        -3         4   # BR(Hp -> Strange charm)
     8.77242450E-04   2        -5         4   # BR(Hp -> Bottom charm)
     6.43143072E-08   2        -1         6   # BR(Hp -> Down top)
     1.72553480E-06   2        -3         6   # BR(Hp -> Strange top)
     5.36895887E-01   2        -5         6   # BR(Hp -> Bottom top)
     4.47544860E-02   2   1000022   1000024   # BR(Hp -> neutralino1 chargino1)
     1.26713746E-03   2   1000022   1000037   # BR(Hp -> neutralino1 chargino2)
     7.43987394E-03   2   1000023   1000024   # BR(Hp -> neutralino2 chargino1)
     1.20587973E-01   2   1000023   1000037   # BR(Hp -> neutralino2 chargino2)
     4.80480803E-02   2   1000024   1000025   # BR(Hp -> chargino1 neutralino3)
     4.44915614E-02   2   1000025   1000037   # BR(Hp -> neutralino3 chargino2)
     7.44007996E-02   2   1000024   1000035   # BR(Hp -> chargino1 neutralino4)
     7.49310450E-04   2   1000035   1000037   # BR(Hp -> neutralino4 chargino2)
     1.32404679E-05   2        24        25   # BR(Hp -> W h0)
     3.47573597E-10   2        24        35   # BR(Hp -> W HH)
     2.69012592E-10   2        24        36   # BR(Hp -> W A0)
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

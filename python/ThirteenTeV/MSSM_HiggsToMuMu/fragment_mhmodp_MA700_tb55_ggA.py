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
         3     5.50000000E+01   # TB
BLOCK EXTPAR
         0     0.00000000E+00   # Q
         1     9.54716519E+01   # M1
         2     2.00000000E+02   # M2
         3     1.50000000E+03   # M3
        11     1.50363636E+03   # At
        12     1.50363636E+03   # Ab
        13     1.50363636E+03   # Atau
        23     2.00000000E+02   # MUE
        25     5.50000000E+01   # TB
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
   1000012     4.95827751E+02   # MSf(1,1,1)
   1000011     5.02299507E+02   # MSf(1,2,1)
   2000011     5.01846637E+02   # MSf(2,2,1)
   1000002     1.49902587E+03   # MSf(1,3,1)
   2000002     1.49958881E+03   # MSf(2,3,1)
   1000001     1.50117936E+03   # MSf(1,4,1)
   2000001     1.50020511E+03   # MSf(2,4,1)
   1000014     4.95827751E+02   # MSf(1,1,2)
   1000013     5.03251123E+02   # MSf(1,2,2)
   2000013     5.00892375E+02   # MSf(2,2,2)
   1000004     1.49902642E+03   # MSf(1,3,2)
   2000004     1.49958936E+03   # MSf(2,3,2)
   1000003     1.50127998E+03   # MSf(1,4,2)
   2000003     1.50010442E+03   # MSf(2,4,2)
   1000016     9.97920417E+02   # MSf(1,1,3)
   1000015     9.92574292E+02   # MSf(1,2,3)
   2000015     1.00943421E+03   # MSf(2,2,3)
   1000006     8.76424240E+02   # MSf(1,3,3)
   2000006     1.13477848E+03   # MSf(2,3,3)
   1000005     9.84089749E+02   # MSf(1,4,3)
   2000005     1.01771670E+03   # MSf(2,4,3)
        25     1.25326784E+02   # Mh0
        35     6.99385216E+02   # MHH
        36     7.00000000E+02   # MA0
        37     7.05340654E+02   # MHp
   1000022     8.87716570E+01   # MNeu(1)
   1000023     1.52696696E+02   # MNeu(2)
   1000025    -2.10790620E+02   # MNeu(3)
   1000035     2.64793919E+02   # MNeu(4)
   1000024     1.49766113E+02   # MCha(1)
   1000037     2.65514196E+02   # MCha(2)
   1000021     1.50000000E+03   # MGl
BLOCK DMASS
         0     1.73200000E+02   # Q
        25     7.46800176E-01   # Delta Mh0
        35     1.24488878E-01   # Delta MHH
        36     0.00000000E+00   # Delta MA0
        37     1.30170143E-01   # Delta MHp
BLOCK NMIX
     1   1     9.35796211E-01   # ZNeu(1,1)
     1   2    -1.05327661E-01   # ZNeu(1,2)
     1   3     3.05844398E-01   # ZNeu(1,3)
     1   4    -1.40181095E-01   # ZNeu(1,4)
     2   1    -3.06572663E-01   # ZNeu(2,1)
     2   2    -6.92995235E-01   # ZNeu(2,2)
     2   3     5.16763046E-01   # ZNeu(2,3)
     2   4    -3.98405272E-01   # ZNeu(2,4)
     3   1     9.87889586E-02   # ZNeu(3,1)
     3   2    -1.37620630E-01   # ZNeu(3,2)
     3   3    -6.77651259E-01   # ZNeu(3,3)
     3   4    -7.15604692E-01   # ZNeu(3,4)
     4   1    -1.43315718E-01   # ZNeu(4,1)
     4   2     6.99803008E-01   # ZNeu(4,2)
     4   3     4.24504334E-01   # ZNeu(4,3)
     4   4    -5.56356383E-01   # ZNeu(4,4)
BLOCK UMIX
     1   1    -6.04661725E-01   # UCha(1,1)
     1   2     7.96482391E-01   # UCha(1,2)
     2   1     7.96482391E-01   # UCha(2,1)
     2   2     6.04661725E-01   # UCha(2,2)
BLOCK VMIX
     1   1    -7.96482391E-01   # VCha(1,1)
     1   2     6.04661725E-01   # VCha(1,2)
     2   1     6.04661725E-01   # VCha(2,1)
     2   2     7.96482391E-01   # VCha(2,2)
BLOCK STAUMIX
     1   1     7.02328839E-01   # USf(1,1)
     1   2     7.11852654E-01   # USf(1,2)
     2   1     7.11852654E-01   # USf(2,1)
     2   2    -7.02328839E-01   # USf(2,2)
BLOCK STOPMIX
     1   1     7.08254429E-01   # USf(1,1)
     1   2    -7.05957267E-01   # USf(1,2)
     2   1     7.05957267E-01   # USf(2,1)
     2   2     7.08254429E-01   # USf(2,2)
BLOCK SBOTMIX
     1   1     6.91592502E-01   # USf(1,1)
     1   2     7.22287901E-01   # USf(1,2)
     2   1     7.22287901E-01   # USf(2,1)
     2   2    -6.91592502E-01   # USf(2,2)
BLOCK ALPHA
              -1.94251140E-02   # Alpha
BLOCK DALPHA
               1.64549909E-05   # Delta Alpha
BLOCK HMIX Q= -0.99900000E+03
         1     2.00000000E+02   # MUE
         2     5.50000000E+01   # TB
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
     3   3     1.50363636E+03   # Af(3,3)
BLOCK AU Q=  0.00000000E+00
     1   1     0.00000000E+00   # Af(1,1)
     2   2     0.00000000E+00   # Af(2,2)
     3   3     1.50363636E+03   # Af(3,3)
BLOCK AD Q=  0.00000000E+00
     1   1     0.00000000E+00   # Af(1,1)
     2   2     0.00000000E+00   # Af(2,2)
     3   3     1.50363636E+03   # Af(3,3)
BLOCK YE Q=  0.00000000E+00
     1   1     1.61452620E-04   # Yf(1,1)
     2   2     3.33832782E-02   # Yf(2,2)
     3   3     5.61461380E-01   # Yf(3,3)
BLOCK YU Q=  0.00000000E+00
     1   1     1.72339050E-05   # Yf(1,1)
     2   2     7.38760060E-03   # Yf(2,2)
     3   3     9.94970781E-01   # Yf(3,3)
BLOCK YD Q=  0.00000000E+00
     1   1     1.79476263E-03   # Yf(1,1)
     2   2     2.84138444E-02   # Yf(2,2)
     3   3     1.11876274E+00   # Yf(3,3)
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
     3   3     8.44233747E+02   # Tf(3,3)
BLOCK TU Q=  0.00000000E+00
     1   1     0.00000000E+00   # Tf(1,1)
     2   2     0.00000000E+00   # Tf(2,2)
     3   3     1.49607425E+03   # Tf(3,3)
BLOCK TD Q=  0.00000000E+00
     1   1     0.00000000E+00   # Tf(1,1)
     2   2     0.00000000E+00   # Tf(2,2)
     3   3     1.68221233E+03   # Tf(3,3)
BLOCK SELMIX
     1   1     9.99923592E-01   # UASf(1,1)
     1   4    -1.23616228E-02   # UASf(1,4)
     2   2     7.71990185E-01   # UASf(2,2)
     2   5    -6.35634450E-01   # UASf(2,5)
     3   3     7.02328839E-01   # UASf(3,3)
     3   6     7.11852654E-01   # UASf(3,6)
     4   1     1.23616228E-02   # UASf(4,1)
     4   4     9.99923592E-01   # UASf(4,4)
     5   2     6.35634450E-01   # UASf(5,2)
     5   5     7.71990185E-01   # UASf(5,5)
     6   3     7.11852654E-01   # UASf(6,3)
     6   6    -7.02328839E-01   # UASf(6,6)
BLOCK USQMIX
     1   1     1.00000000E+00   # UASf(1,1)
     1   4     6.46265707E-06   # UASf(1,4)
     2   2     9.99996163E-01   # UASf(2,2)
     2   5     2.77029377E-03   # UASf(2,5)
     3   3     7.08254429E-01   # UASf(3,3)
     3   6    -7.05957267E-01   # UASf(3,6)
     4   1    -6.46265707E-06   # UASf(4,1)
     4   4     1.00000000E+00   # UASf(4,4)
     5   2    -2.77029377E-03   # UASf(5,2)
     5   5     9.99996163E-01   # UASf(5,5)
     6   3     7.05957267E-01   # UASf(6,3)
     6   6     7.08254429E-01   # UASf(6,6)
BLOCK DSQMIX
     1   1     9.99771555E-01   # UASf(1,1)
     1   4    -2.13737769E-02   # UASf(1,4)
     2   2     9.56031900E-01   # UASf(2,2)
     2   5    -2.93262690E-01   # UASf(2,5)
     3   3     6.91592502E-01   # UASf(3,3)
     3   6     7.22287901E-01   # UASf(3,6)
     4   1     2.13737769E-02   # UASf(4,1)
     4   4     9.99771555E-01   # UASf(4,4)
     5   2     2.93262690E-01   # UASf(5,2)
     5   5     9.56031900E-01   # UASf(5,5)
     6   3     7.22287901E-01   # UASf(6,3)
     6   6    -6.91592502E-01   # UASf(6,6)
BLOCK CVHMIX
     1   1     9.99999809E-01   # UH(1,1)
     1   2     6.18215688E-04   # UH(1,2)
     1   3     0.00000000E+00   # UH(1,3)
     2   1    -6.18215688E-04   # UH(2,1)
     2   2     9.99999809E-01   # UH(2,2)
     2   3     0.00000000E+00   # UH(2,3)
     3   1     0.00000000E+00   # UH(3,1)
     3   2     0.00000000E+00   # UH(3,2)
     3   3     1.00000000E+00   # UH(3,3)
DECAY        25     4.14782115E-03   # Gamma(h0)
     2.23471524E-03   2        22        22   # BR(h0 -> photon photon)
     1.45738283E-03   2        22        23   # BR(h0 -> photon Z)
     2.69437014E-02   2        23        23   # BR(h0 -> Z Z)
     2.21144073E-01   2       -24        24   # BR(h0 -> W W)
     6.85048929E-02   2        21        21   # BR(h0 -> gluon gluon)
     5.51484762E-09   2       -11        11   # BR(h0 -> Electron electron)
     2.45309954E-04   2       -13        13   # BR(h0 -> Muon muon)
     7.01451274E-02   2       -15        15   # BR(h0 -> Tau tau)
     1.94787333E-07   2        -2         2   # BR(h0 -> Up up)
     2.69802377E-02   2        -4         4   # BR(h0 -> Charm charm)
     8.91367278E-07   2        -1         1   # BR(h0 -> Down down)
     2.23848224E-04   2        -3         3   # BR(h0 -> Strange strange)
     5.82119620E-01   2        -5         5   # BR(h0 -> Bottom bottom)
DECAY        35     3.12281843E+01   # Gamma(HH)
     1.95035634E-07   2        22        22   # BR(HH -> photon photon)
     8.24434255E-07   2        22        23   # BR(HH -> photon Z)
     2.93412500E-06   2        23        23   # BR(HH -> Z Z)
     5.42110032E-06   2       -24        24   # BR(HH -> W W)
     7.07009744E-05   2        21        21   # BR(HH -> gluon gluon)
     1.15785082E-08   2       -11        11   # BR(HH -> Electron electron)
     5.15300172E-04   2       -13        13   # BR(HH -> Muon muon)
     1.40842699E-01   2       -15        15   # BR(HH -> Tau tau)
     2.86627074E-14   2        -2         2   # BR(HH -> Up up)
     3.96647892E-09   2        -4         4   # BR(HH -> Charm charm)
     1.63503249E-04   2        -6         6   # BR(HH -> Top top)
     1.18065317E-06   2        -1         1   # BR(HH -> Down down)
     2.96464740E-04   2        -3         3   # BR(HH -> Strange strange)
     6.48785979E-01   2        -5         5   # BR(HH -> Bottom bottom)
     5.32619021E-02   2  -1000024   1000024   # BR(HH -> Chargino1 chargino1)
     3.41263489E-02   2  -1000037   1000024   # BR(HH -> Chargino2 chargino1)
     3.41263489E-02   2  -1000024   1000037   # BR(HH -> Chargino1 chargino2)
     6.31009386E-03   2  -1000037   1000037   # BR(HH -> Chargino2 chargino2)
     5.60385326E-03   2   1000022   1000022   # BR(HH -> neutralino1 neutralino1)
     1.65982738E-02   2   1000022   1000023   # BR(HH -> neutralino1 neutralino2)
     1.03369154E-02   2   1000022   1000025   # BR(HH -> neutralino1 neutralino3)
     1.34959869E-05   2   1000022   1000035   # BR(HH -> neutralino1 neutralino4)
     9.68156853E-03   2   1000023   1000023   # BR(HH -> neutralino2 neutralino2)
     5.60393965E-03   2   1000023   1000025   # BR(HH -> neutralino2 neutralino3)
     1.45551339E-03   2   1000023   1000035   # BR(HH -> neutralino2 neutralino4)
     1.61711620E-03   2   1000025   1000025   # BR(HH -> neutralino3 neutralino3)
     2.52740704E-02   2   1000025   1000035   # BR(HH -> neutralino3 neutralino4)
     5.27478547E-03   2   1000035   1000035   # BR(HH -> neutralino4 neutralino4)
     3.05566727E-05   2        25        25   # BR(HH -> h0 h0)
DECAY        36     3.06301683E+01   # Gamma(A0)
    -6.51669023E-07   2        22        22   # BR(A0 -> photon photon)
    -1.25466485E-06   2        22        23   # BR(A0 -> photon Z)
    -9.08255572E-05   2        21        21   # BR(A0 -> gluon gluon)
    -1.15259695E-08   2       -11        11   # BR(A0 -> Electron electron)
     5.12962099E-04   2       -13        13   # BR(A0 -> Muon muon)
    -1.40205379E-01   2       -15        15   # BR(A0 -> Tau tau)
    -2.50363379E-14   2        -2         2   # BR(A0 -> Up up)
    -3.46754820E-09   2        -4         4   # BR(A0 -> Charm charm)
    -1.87149378E-04   2        -6         6   # BR(A0 -> Top top)
    -1.17513643E-06   2        -1         1   # BR(A0 -> Down down)
    -2.95079502E-04   2        -3         3   # BR(A0 -> Strange strange)
    -6.45828915E-01   2        -5         5   # BR(A0 -> Bottom bottom)
    -6.95246979E-02   2  -1000024   1000024   # BR(A0 -> Chargino1 chargino1)
    -2.15339439E-02   2  -1000037   1000024   # BR(A0 -> Chargino2 chargino1)
    -2.15339439E-02   2  -1000024   1000037   # BR(A0 -> Chargino1 chargino2)
    -1.73487063E-02   2  -1000037   1000037   # BR(A0 -> Chargino2 chargino2)
    -6.32731368E-03   2   1000022   1000022   # BR(A0 -> neutralino1 neutralino1)
    -2.00503264E-02   2   1000022   1000023   # BR(A0 -> neutralino1 neutralino2)
    -8.05776029E-03   2   1000022   1000025   # BR(A0 -> neutralino1 neutralino3)
    -4.20977062E-05   2   1000022   1000035   # BR(A0 -> neutralino1 neutralino4)
    -1.29392501E-02   2   1000023   1000023   # BR(A0 -> neutralino2 neutralino2)
    -3.68917082E-03   2   1000023   1000025   # BR(A0 -> neutralino2 neutralino3)
    -2.26404422E-03   2   1000023   1000035   # BR(A0 -> neutralino2 neutralino4)
    -2.39541786E-03   2   1000025   1000025   # BR(A0 -> neutralino3 neutralino3)
    -1.32227922E-02   2   1000025   1000035   # BR(A0 -> neutralino3 neutralino4)
    -1.39422441E-02   2   1000035   1000035   # BR(A0 -> neutralino4 neutralino4)
    -4.88328500E-06   2        23        25   # BR(A0 -> Z h0)
    -1.09006595E-36   2        25        25   # BR(A0 -> h0 h0)
DECAY        37     2.83533747E+01   # Gamma(Hp)
     1.24545988E-08   2       -11        12   # BR(Hp -> Electron nu_e)
     5.32472882E-04   2       -13        14   # BR(Hp -> Muon nu_mu)
     1.50616938E-01   2       -15        16   # BR(Hp -> Tau nu_tau)
     1.22398452E-06   2        -1         2   # BR(Hp -> Down up)
     1.40687404E-05   2        -3         2   # BR(Hp -> Strange up)
     7.25085061E-06   2        -5         2   # BR(Hp -> Bottom up)
     5.62952489E-08   2        -1         4   # BR(Hp -> Down charm)
     3.06259739E-04   2        -3         4   # BR(Hp -> Strange charm)
     1.01537212E-03   2        -5         4   # BR(Hp -> Bottom charm)
     2.26772552E-08   2        -1         6   # BR(Hp -> Down top)
     8.89252528E-07   2        -3         6   # BR(Hp -> Strange top)
     6.20730837E-01   2        -5         6   # BR(Hp -> Bottom top)
     2.98637673E-02   2   1000022   1000024   # BR(Hp -> neutralino1 chargino1)
     7.92550307E-04   2   1000022   1000037   # BR(Hp -> neutralino1 chargino2)
     4.84923898E-03   2   1000023   1000024   # BR(Hp -> neutralino2 chargino1)
     8.01252892E-02   2   1000023   1000037   # BR(Hp -> neutralino2 chargino2)
     3.18196591E-02   2   1000024   1000025   # BR(Hp -> chargino1 neutralino3)
     2.95163344E-02   2   1000025   1000037   # BR(Hp -> neutralino3 chargino2)
     4.93082867E-02   2   1000024   1000035   # BR(Hp -> chargino1 neutralino4)
     4.93990594E-04   2   1000035   1000037   # BR(Hp -> neutralino4 chargino2)
     5.47840193E-06   2        24        25   # BR(Hp -> W h0)
     3.56271721E-10   2        24        35   # BR(Hp -> W HH)
     2.06770692E-10   2        24        36   # BR(Hp -> W A0)
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

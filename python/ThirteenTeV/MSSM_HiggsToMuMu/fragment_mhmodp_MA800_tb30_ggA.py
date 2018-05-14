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
         3     3.00000000E+01   # TB
BLOCK EXTPAR
         0     0.00000000E+00   # Q
         1     9.54716519E+01   # M1
         2     2.00000000E+02   # M2
         3     1.50000000E+03   # M3
        11     1.50666667E+03   # At
        12     1.50666667E+03   # Ab
        13     1.50666667E+03   # Atau
        23     2.00000000E+02   # MUE
        25     3.00000000E+01   # TB
        26     8.00000000E+02   # MA0
        27     8.04029750E+02   # MHp
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
   1000012     4.95834286E+02   # MSf(1,1,1)
   1000011     5.02295880E+02   # MSf(1,2,1)
   2000011     5.01843810E+02   # MSf(2,2,1)
   1000002     1.49902739E+03   # MSf(1,3,1)
   2000002     1.49958945E+03   # MSf(2,3,1)
   1000001     1.50117722E+03   # MSf(1,4,1)
   2000001     1.50020509E+03   # MSf(2,4,1)
   1000014     4.95834286E+02   # MSf(1,1,2)
   1000013     5.02740033E+02   # MSf(1,2,2)
   2000013     5.01398885E+02   # MSf(2,2,2)
   1000004     1.49902793E+03   # MSf(1,3,2)
   2000004     1.49959001E+03   # MSf(2,3,2)
   1000003     1.50121083E+03   # MSf(1,4,2)
   2000003     1.50017147E+03   # MSf(2,4,2)
   1000016     9.97923664E+02   # MSf(1,1,3)
   1000015     9.97040283E+02   # MSf(1,2,3)
   2000015     1.00502007E+03   # MSf(2,2,3)
   1000006     8.76426091E+02   # MSf(1,3,3)
   2000006     1.13477990E+03   # MSf(2,3,3)
   1000005     9.92452622E+02   # MSf(1,4,3)
   2000005     1.00956179E+03   # MSf(2,4,3)
        25     1.25066010E+02   # Mh0
        35     7.99821723E+02   # MHH
        36     8.00000000E+02   # MA0
        37     8.04364050E+02   # MHp
   1000022     8.83213889E+01   # MNeu(1)
   1000023     1.52049632E+02   # MNeu(2)
   1000025    -2.10470056E+02   # MNeu(3)
   1000035     2.65570688E+02   # MNeu(4)
   1000024     1.48692309E+02   # MCha(1)
   1000037     2.66117031E+02   # MCha(2)
   1000021     1.50000000E+03   # MGl
BLOCK DMASS
         0     1.73200000E+02   # Q
        25     7.04648030E-01   # Delta Mh0
        35     9.49190088E-03   # Delta MHH
        36     0.00000000E+00   # Delta MA0
        37     1.39495321E-02   # Delta MHp
BLOCK NMIX
     1   1     9.33161766E-01   # ZNeu(1,1)
     1   2    -1.11637757E-01   # ZNeu(1,2)
     1   3     3.09461555E-01   # ZNeu(1,3)
     1   4    -1.44843625E-01   # ZNeu(1,4)
     2   1    -3.14536695E-01   # ZNeu(2,1)
     2   2    -6.93472054E-01   # ZNeu(2,2)
     2   3     5.12603053E-01   # ZNeu(2,3)
     2   4    -3.96738310E-01   # ZNeu(2,4)
     3   1     9.74524166E-02   # ZNeu(3,1)
     3   2    -1.35722538E-01   # ZNeu(3,2)
     3   3    -6.77905232E-01   # ZNeu(3,3)
     3   4    -7.15909852E-01   # ZNeu(3,4)
     4   1    -1.44148578E-01   # ZNeu(4,1)
     4   2     6.98722344E-01   # ZNeu(4,2)
     4   3     4.26516298E-01   # ZNeu(4,3)
     4   4    -5.55960539E-01   # ZNeu(4,4)
BLOCK UMIX
     1   1    -6.06292879E-01   # UCha(1,1)
     1   2     7.95241438E-01   # UCha(1,2)
     2   1     7.95241438E-01   # UCha(2,1)
     2   2     6.06292879E-01   # UCha(2,2)
BLOCK VMIX
     1   1    -7.95241438E-01   # VCha(1,1)
     1   2     6.06292879E-01   # VCha(1,2)
     2   1     6.06292879E-01   # VCha(2,1)
     2   2     7.95241438E-01   # VCha(2,2)
BLOCK STAUMIX
     1   1     6.96989496E-01   # USf(1,1)
     1   2     7.17081336E-01   # USf(1,2)
     2   1     7.17081336E-01   # USf(2,1)
     2   2    -6.96989496E-01   # USf(2,2)
BLOCK STOPMIX
     1   1     7.08252641E-01   # USf(1,1)
     1   2    -7.05959062E-01   # USf(1,2)
     2   1     7.05959062E-01   # USf(2,1)
     2   2     7.08252641E-01   # USf(2,2)
BLOCK SBOTMIX
     1   1     6.76329341E-01   # USf(1,1)
     1   2     7.36599364E-01   # USf(1,2)
     2   1     7.36599364E-01   # USf(2,1)
     2   2    -6.76329341E-01   # USf(2,2)
BLOCK ALPHA
              -3.48340486E-02   # Alpha
BLOCK DALPHA
               2.68020394E-05   # Delta Alpha
BLOCK HMIX Q= -0.99900000E+03
         1     2.00000000E+02   # MUE
         2     3.00000000E+01   # TB
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
     3   3     1.50666667E+03   # Af(3,3)
BLOCK AU Q=  0.00000000E+00
     1   1     0.00000000E+00   # Af(1,1)
     2   2     0.00000000E+00   # Af(2,2)
     3   3     1.50666667E+03   # Af(3,3)
BLOCK AD Q=  0.00000000E+00
     1   1     0.00000000E+00   # Af(1,1)
     2   2     0.00000000E+00   # Af(2,2)
     3   3     1.50666667E+03   # Af(3,3)
BLOCK YE Q=  0.00000000E+00
     1   1     8.80994160E-05   # Yf(1,1)
     2   2     1.82161635E-02   # Yf(2,2)
     3   3     3.06371119E-01   # Yf(3,3)
BLOCK YU Q=  0.00000000E+00
     1   1     1.72406273E-05   # Yf(1,1)
     2   2     7.39048222E-03   # Yf(2,2)
     3   3     9.95358881E-01   # Yf(3,3)
BLOCK YD Q=  0.00000000E+00
     1   1     1.00364128E-03   # Yf(1,1)
     2   2     1.58899845E-02   # Yf(2,2)
     3   3     6.54741643E-01   # Yf(3,3)
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
     3   3     4.61599152E+02   # Tf(3,3)
BLOCK TU Q=  0.00000000E+00
     1   1     0.00000000E+00   # Tf(1,1)
     2   2     0.00000000E+00   # Tf(2,2)
     3   3     1.49967405E+03   # Tf(3,3)
BLOCK TD Q=  0.00000000E+00
     1   1     0.00000000E+00   # Tf(1,1)
     2   2     0.00000000E+00   # Tf(2,2)
     3   3     9.86477409E+02   # Tf(3,3)
BLOCK SELMIX
     1   1     9.99977189E-01   # UASf(1,1)
     1   4    -6.75432353E-03   # UASf(1,4)
     2   2     8.17632607E-01   # UASf(2,2)
     2   5    -5.75740323E-01   # UASf(2,5)
     3   3     6.96989496E-01   # UASf(3,3)
     3   6     7.17081336E-01   # UASf(3,6)
     4   1     6.75432353E-03   # UASf(4,1)
     4   4     9.99977189E-01   # UASf(4,4)
     5   2     5.75740323E-01   # UASf(5,2)
     5   5     8.17632607E-01   # UASf(5,5)
     6   3     7.17081336E-01   # UASf(6,3)
     6   6    -6.96989496E-01   # UASf(6,6)
BLOCK USQMIX
     1   1     1.00000000E+00   # UASf(1,1)
     1   4     1.18667149E-05   # UASf(1,4)
     2   2     9.99987063E-01   # UASf(2,2)
     2   5     5.08666769E-03   # UASf(2,5)
     3   3     7.08252641E-01   # UASf(3,3)
     3   6    -7.05959062E-01   # UASf(3,6)
     4   1    -1.18667149E-05   # UASf(4,1)
     4   4     1.00000000E+00   # UASf(4,4)
     5   2    -5.08666769E-03   # UASf(5,2)
     5   5     9.99987063E-01   # UASf(5,5)
     6   3     7.05959062E-01   # UASf(6,3)
     6   6     7.08252641E-01   # UASf(6,6)
BLOCK DSQMIX
     1   1     9.99928333E-01   # UASf(1,1)
     1   4    -1.19719733E-02   # UASf(1,4)
     2   2     9.83626200E-01   # UASf(2,2)
     2   5    -1.80220692E-01   # UASf(2,5)
     3   3     6.76329341E-01   # UASf(3,3)
     3   6     7.36599364E-01   # UASf(3,6)
     4   1     1.19719733E-02   # UASf(4,1)
     4   4     9.99928333E-01   # UASf(4,4)
     5   2     1.80220692E-01   # UASf(5,2)
     5   5     9.83626200E-01   # UASf(5,5)
     6   3     7.36599364E-01   # UASf(6,3)
     6   6    -6.76329341E-01   # UASf(6,6)
BLOCK CVHMIX
     1   1     9.99999796E-01   # UH(1,1)
     1   2     6.38509365E-04   # UH(1,2)
     1   3     0.00000000E+00   # UH(1,3)
     2   1    -6.38509365E-04   # UH(2,1)
     2   2     9.99999796E-01   # UH(2,2)
     2   3     0.00000000E+00   # UH(2,3)
     3   1     0.00000000E+00   # UH(3,1)
     3   2     0.00000000E+00   # UH(3,2)
     3   3     1.00000000E+00   # UH(3,3)
DECAY        25     4.03891953E-03   # Gamma(h0)
     2.28428874E-03   2        22        22   # BR(h0 -> photon photon)
     1.46708165E-03   2        22        23   # BR(h0 -> photon Z)
     2.67856402E-02   2        23        23   # BR(h0 -> Z Z)
     2.20980742E-01   2       -24        24   # BR(h0 -> W W)
     7.00648415E-02   2        21        21   # BR(h0 -> gluon gluon)
     5.39322142E-09   2       -11        11   # BR(h0 -> Electron electron)
     2.39900057E-04   2       -13        13   # BR(h0 -> Muon muon)
     6.88865700E-02   2       -15        15   # BR(h0 -> Tau tau)
     1.99678565E-07   2        -2         2   # BR(h0 -> Up up)
     2.76575796E-02   2        -4         4   # BR(h0 -> Charm charm)
     8.81227430E-07   2        -1         1   # BR(h0 -> Down down)
     2.21304802E-04   2        -3         3   # BR(h0 -> Strange strange)
     5.81410965E-01   2        -5         5   # BR(h0 -> Bottom bottom)
DECAY        35     1.73526743E+01   # Gamma(HH)
     4.16400041E-07   2        22        22   # BR(HH -> photon photon)
     1.45206280E-06   2        22        23   # BR(HH -> photon Z)
     1.26068928E-05   2        23        23   # BR(HH -> Z Z)
     2.10739220E-05   2       -24        24   # BR(HH -> W W)
     3.55632090E-05   2        21        21   # BR(HH -> gluon gluon)
     6.71120848E-09   2       -11        11   # BR(HH -> Electron electron)
     2.98693396E-04   2       -13        13   # BR(HH -> Muon muon)
     8.36937228E-02   2       -15        15   # BR(HH -> Tau tau)
     2.26269010E-13   2        -2         2   # BR(HH -> Up up)
     3.13183174E-08   2        -4         4   # BR(HH -> Charm charm)
     1.69242149E-03   2        -6         6   # BR(HH -> Top top)
     7.38873370E-07   2        -1         1   # BR(HH -> Down down)
     1.85548510E-04   2        -3         3   # BR(HH -> Strange strange)
     4.38634942E-01   2        -5         5   # BR(HH -> Bottom bottom)
     1.15847700E-01   2  -1000024   1000024   # BR(HH -> Chargino1 chargino1)
     7.77102174E-02   2  -1000037   1000024   # BR(HH -> Chargino2 chargino1)
     7.77102174E-02   2  -1000024   1000037   # BR(HH -> Chargino1 chargino2)
     1.87336494E-02   2  -1000037   1000037   # BR(HH -> Chargino2 chargino2)
     1.21084994E-02   2   1000022   1000022   # BR(HH -> neutralino1 neutralino1)
     3.52156312E-02   2   1000022   1000023   # BR(HH -> neutralino1 neutralino2)
     2.34041785E-02   2   1000022   1000025   # BR(HH -> neutralino1 neutralino3)
     2.10494279E-05   2   1000022   1000035   # BR(HH -> neutralino1 neutralino4)
     2.04756681E-02   2   1000023   1000023   # BR(HH -> neutralino2 neutralino2)
     1.26481497E-02   2   1000023   1000025   # BR(HH -> neutralino2 neutralino3)
     3.47346418E-03   2   1000023   1000035   # BR(HH -> neutralino2 neutralino4)
     4.04379099E-03   2   1000025   1000025   # BR(HH -> neutralino3 neutralino3)
     5.81856934E-02   2   1000025   1000035   # BR(HH -> neutralino3 neutralino4)
     1.57318866E-02   2   1000035   1000035   # BR(HH -> neutralino4 neutralino4)
     1.12987244E-04   2        25        25   # BR(HH -> h0 h0)
DECAY        36     1.71741045E+01   # Gamma(A0)
    -8.40614334E-07   2        22        22   # BR(A0 -> photon photon)
    -1.80087560E-06   2        22        23   # BR(A0 -> photon Z)
    -6.52858230E-05   2        21        21   # BR(A0 -> gluon gluon)
    -6.65484153E-09   2       -11        11   # BR(A0 -> Electron electron)
     2.96184775E-04   2       -13        13   # BR(A0 -> Muon muon)
    -8.29918789E-02   2       -15        15   # BR(A0 -> Tau tau)
    -2.09013569E-13   2        -2         2   # BR(A0 -> Up up)
    -2.89312113E-08   2        -4         4   # BR(A0 -> Charm charm)
    -1.93426910E-03   2        -6         6   # BR(A0 -> Top top)
    -7.32641832E-07   2        -1         1   # BR(A0 -> Down down)
    -1.83983648E-04   2        -3         3   # BR(A0 -> Strange strange)
    -4.34972142E-01   2        -5         5   # BR(A0 -> Bottom bottom)
    -1.47094289E-01   2  -1000024   1000024   # BR(A0 -> Chargino1 chargino1)
    -5.12173545E-02   2  -1000037   1000024   # BR(A0 -> Chargino2 chargino1)
    -5.12173545E-02   2  -1000024   1000037   # BR(A0 -> Chargino1 chargino2)
    -4.29998928E-02   2  -1000037   1000037   # BR(A0 -> Chargino2 chargino2)
    -1.37144146E-02   2   1000022   1000022   # BR(A0 -> neutralino1 neutralino1)
    -4.25990192E-02   2   1000022   1000023   # BR(A0 -> neutralino1 neutralino2)
    -1.74459506E-02   2   1000022   1000025   # BR(A0 -> neutralino1 neutralino3)
    -1.25155128E-04   2   1000022   1000035   # BR(A0 -> neutralino1 neutralino4)
    -2.68887969E-02   2   1000023   1000023   # BR(A0 -> neutralino2 neutralino2)
    -8.06250285E-03   2   1000023   1000025   # BR(A0 -> neutralino2 neutralino3)
    -4.78909302E-03   2   1000023   1000035   # BR(A0 -> neutralino2 neutralino4)
    -4.89545260E-03   2   1000025   1000025   # BR(A0 -> neutralino3 neutralino3)
    -3.44899853E-02   2   1000025   1000035   # BR(A0 -> neutralino3 neutralino4)
    -3.39936507E-02   2   1000035   1000035   # BR(A0 -> neutralino4 neutralino4)
    -1.99335821E-05   2        23        25   # BR(A0 -> Z h0)
    -1.11152117E-36   2        25        25   # BR(A0 -> h0 h0)
DECAY        37     1.65865196E+01   # Gamma(Hp)
     7.22354031E-09   2       -11        12   # BR(Hp -> Electron nu_e)
     3.08828845E-04   2       -13        14   # BR(Hp -> Muon nu_mu)
     8.73565445E-02   2       -15        16   # BR(Hp -> Tau nu_tau)
     7.31879798E-07   2        -1         2   # BR(Hp -> Down up)
     8.42960341E-06   2        -3         2   # BR(Hp -> Strange up)
     4.75800455E-06   2        -5         2   # BR(Hp -> Bottom up)
     3.50871702E-08   2        -1         4   # BR(Hp -> Down charm)
     1.83175920E-04   2        -3         4   # BR(Hp -> Strange charm)
     6.66287745E-04   2        -5         4   # BR(Hp -> Bottom charm)
     1.50071626E-07   2        -1         6   # BR(Hp -> Down top)
     3.51562997E-06   2        -3         6   # BR(Hp -> Strange top)
     4.20700277E-01   2        -5         6   # BR(Hp -> Bottom top)
     5.95172697E-02   2   1000022   1000024   # BR(Hp -> neutralino1 chargino1)
     1.93488883E-03   2   1000022   1000037   # BR(Hp -> neutralino1 chargino2)
     1.03625760E-02   2   1000023   1000024   # BR(Hp -> neutralino2 chargino1)
     1.75010497E-01   2   1000023   1000037   # BR(Hp -> neutralino2 chargino2)
     6.73139939E-02   2   1000024   1000025   # BR(Hp -> chargino1 neutralino3)
     6.73765812E-02   2   1000025   1000037   # BR(Hp -> neutralino3 chargino2)
     1.08003687E-01   2   1000024   1000035   # BR(Hp -> chargino1 neutralino4)
     1.22655939E-03   2   1000035   1000037   # BR(Hp -> neutralino4 chargino2)
     2.12043356E-05   2        24        25   # BR(Hp -> W h0)
     1.57656769E-10   2        24        35   # BR(Hp -> W HH)
     1.29078586E-10   2        24        36   # BR(Hp -> W A0)
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

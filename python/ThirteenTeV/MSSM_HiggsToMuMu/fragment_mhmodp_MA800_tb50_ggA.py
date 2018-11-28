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
         3     5.00000000E+01   # TB
BLOCK EXTPAR
         0     0.00000000E+00   # Q
         1     9.54716519E+01   # M1
         2     2.00000000E+02   # M2
         3     1.50000000E+03   # M3
        11     1.50400000E+03   # At
        12     1.50400000E+03   # Ab
        13     1.50400000E+03   # Atau
        23     2.00000000E+02   # MUE
        25     5.00000000E+01   # TB
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
   1000012     4.95828333E+02   # MSf(1,1,1)
   1000011     5.02299176E+02   # MSf(1,2,1)
   2000011     5.01846393E+02   # MSf(2,2,1)
   1000002     1.49902601E+03   # MSf(1,3,1)
   2000002     1.49958886E+03   # MSf(2,3,1)
   1000001     1.50117912E+03   # MSf(1,4,1)
   2000001     1.50020515E+03   # MSf(2,4,1)
   1000014     4.95828333E+02   # MSf(1,1,2)
   1000013     5.03147984E+02   # MSf(1,2,2)
   2000013     5.00995402E+02   # MSf(2,2,2)
   1000004     1.49902656E+03   # MSf(1,3,2)
   2000004     1.49958942E+03   # MSf(2,3,2)
   1000003     1.50126434E+03   # MSf(1,4,2)
   2000003     1.50011988E+03   # MSf(2,4,2)
   1000016     9.97920706E+02   # MSf(1,1,3)
   1000015     9.93469140E+02   # MSf(1,2,3)
   2000015     1.00855324E+03   # MSf(2,2,3)
   1000006     8.76424405E+02   # MSf(1,3,3)
   2000006     1.13477860E+03   # MSf(2,3,3)
   1000005     9.85677032E+02   # MSf(1,4,3)
   2000005     1.01617952E+03   # MSf(2,4,3)
        25     1.25275293E+02   # Mh0
        35     7.99435350E+02   # MHH
        36     8.00000000E+02   # MA0
        37     8.04446892E+02   # MHp
   1000022     8.87178977E+01   # MNeu(1)
   1000023     1.52618078E+02   # MNeu(2)
   1000025    -2.10752172E+02   # MNeu(3)
   1000035     2.64887848E+02   # MNeu(4)
   1000024     1.49636507E+02   # MCha(1)
   1000037     2.65587260E+02   # MCha(2)
   1000021     1.50000000E+03   # MGl
BLOCK DMASS
         0     1.73200000E+02   # Q
        25     7.30855745E-01   # Delta Mh0
        35     9.38508533E-02   # Delta MHH
        36     0.00000000E+00   # Delta MA0
        37     8.00498899E-02   # Delta MHp
BLOCK NMIX
     1   1     9.35482570E-01   # ZNeu(1,1)
     1   2    -1.06085928E-01   # ZNeu(1,2)
     1   3     3.06284138E-01   # ZNeu(1,3)
     1   4    -1.40741478E-01   # ZNeu(1,4)
     2   1    -3.07532229E-01   # ZNeu(2,1)
     2   2    -6.93057380E-01   # ZNeu(2,2)
     2   3     5.16264244E-01   # ZNeu(2,3)
     2   4    -3.98204252E-01   # ZNeu(2,4)
     3   1     9.86300223E-02   # ZNeu(3,1)
     3   2    -1.37394830E-01   # ZNeu(3,2)
     3   3    -6.77680714E-01   # ZNeu(3,3)
     3   4    -7.15642110E-01   # ZNeu(3,4)
     4   1    -1.43416903E-01   # ZNeu(4,1)
     4   2     6.99671283E-01   # ZNeu(4,2)
     4   3     4.24747110E-01   # ZNeu(4,3)
     4   4    -5.56310687E-01   # ZNeu(4,4)
BLOCK UMIX
     1   1    -6.04856223E-01   # UCha(1,1)
     1   2     7.96334697E-01   # UCha(1,2)
     2   1     7.96334697E-01   # UCha(2,1)
     2   2     6.04856223E-01   # UCha(2,2)
BLOCK VMIX
     1   1    -7.96334697E-01   # VCha(1,1)
     1   2     6.04856223E-01   # VCha(1,2)
     2   1     6.04856223E-01   # VCha(2,1)
     2   2     7.96334697E-01   # VCha(2,2)
BLOCK STAUMIX
     1   1     7.01764984E-01   # USf(1,1)
     1   2     7.12408525E-01   # USf(1,2)
     2   1     7.12408525E-01   # USf(2,1)
     2   2    -7.01764984E-01   # USf(2,2)
BLOCK STOPMIX
     1   1     7.08254270E-01   # USf(1,1)
     1   2    -7.05957427E-01   # USf(1,2)
     2   1     7.05957427E-01   # USf(2,1)
     2   2     7.08254270E-01   # USf(2,2)
BLOCK SBOTMIX
     1   1     6.89986472E-01   # USf(1,1)
     1   2     7.23822263E-01   # USf(1,2)
     2   1     7.23822263E-01   # USf(2,1)
     2   2    -6.89986472E-01   # USf(2,2)
BLOCK ALPHA
              -2.10128106E-02   # Alpha
BLOCK DALPHA
               1.47127862E-05   # Delta Alpha
BLOCK HMIX Q= -0.99900000E+03
         1     2.00000000E+02   # MUE
         2     5.00000000E+01   # TB
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
     3   3     1.50400000E+03   # Af(3,3)
BLOCK AU Q=  0.00000000E+00
     1   1     0.00000000E+00   # Af(1,1)
     2   2     0.00000000E+00   # Af(2,2)
     3   3     1.50400000E+03   # Af(3,3)
BLOCK AD Q=  0.00000000E+00
     1   1     0.00000000E+00   # Af(1,1)
     2   2     0.00000000E+00   # Af(2,2)
     3   3     1.50400000E+03   # Af(3,3)
BLOCK YE Q=  0.00000000E+00
     1   1     1.46780202E-04   # Yf(1,1)
     2   2     3.03494878E-02   # Yf(2,2)
     3   3     5.10437147E-01   # Yf(3,3)
BLOCK YU Q=  0.00000000E+00
     1   1     1.72345030E-05   # Yf(1,1)
     2   2     7.38785694E-03   # Yf(2,2)
     3   3     9.95005305E-01   # Yf(3,3)
BLOCK YD Q=  0.00000000E+00
     1   1     1.63959806E-03   # Yf(1,1)
     2   2     2.59576088E-02   # Yf(2,2)
     3   3     1.03103583E+00   # Yf(3,3)
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
     3   3     7.67697469E+02   # Tf(3,3)
BLOCK TU Q=  0.00000000E+00
     1   1     0.00000000E+00   # Tf(1,1)
     2   2     0.00000000E+00   # Tf(2,2)
     3   3     1.49648798E+03   # Tf(3,3)
BLOCK TD Q=  0.00000000E+00
     1   1     0.00000000E+00   # Tf(1,1)
     2   2     0.00000000E+00   # Tf(2,2)
     3   3     1.55067789E+03   # Tf(3,3)
BLOCK SELMIX
     1   1     9.99936831E-01   # UASf(1,1)
     1   4    -1.12398458E-02   # UASf(1,4)
     2   2     7.77911165E-01   # UASf(2,2)
     2   5    -6.28374267E-01   # UASf(2,5)
     3   3     7.01764984E-01   # UASf(3,3)
     3   6     7.12408525E-01   # UASf(3,6)
     4   1     1.12398458E-02   # UASf(4,1)
     4   4     9.99936831E-01   # UASf(4,4)
     5   2     6.28374267E-01   # UASf(5,2)
     5   5     7.77911165E-01   # UASf(5,5)
     6   3     7.12408525E-01   # UASf(6,3)
     6   6    -7.01764984E-01   # UASf(6,6)
BLOCK USQMIX
     1   1     1.00000000E+00   # UASf(1,1)
     1   4     7.10990987E-06   # UASf(1,4)
     2   2     9.99995356E-01   # UASf(2,2)
     2   5     3.04773890E-03   # UASf(2,5)
     3   3     7.08254270E-01   # UASf(3,3)
     3   6    -7.05957427E-01   # UASf(3,6)
     4   1    -7.10990987E-06   # UASf(4,1)
     4   4     1.00000000E+00   # UASf(4,4)
     5   2    -3.04773890E-03   # UASf(5,2)
     5   5     9.99995356E-01   # UASf(5,5)
     6   3     7.05957427E-01   # UASf(6,3)
     6   6     7.08254270E-01   # UASf(6,6)
BLOCK DSQMIX
     1   1     9.99809268E-01   # UASf(1,1)
     1   4    -1.95301713E-02   # UASf(1,4)
     2   2     9.61868020E-01   # UASf(2,2)
     2   5    -2.73514006E-01   # UASf(2,5)
     3   3     6.89986472E-01   # UASf(3,3)
     3   6     7.23822263E-01   # UASf(3,6)
     4   1     1.95301713E-02   # UASf(4,1)
     4   4     9.99809268E-01   # UASf(4,4)
     5   2     2.73514006E-01   # UASf(5,2)
     5   5     9.61868020E-01   # UASf(5,5)
     6   3     7.23822263E-01   # UASf(6,3)
     6   6    -6.89986472E-01   # UASf(6,6)
BLOCK CVHMIX
     1   1     9.99999880E-01   # UH(1,1)
     1   2     4.89590683E-04   # UH(1,2)
     1   3     0.00000000E+00   # UH(1,3)
     2   1    -4.89590683E-04   # UH(2,1)
     2   2     9.99999880E-01   # UH(2,2)
     2   3     0.00000000E+00   # UH(2,3)
     3   1     0.00000000E+00   # UH(3,1)
     3   2     0.00000000E+00   # UH(3,2)
     3   3     1.00000000E+00   # UH(3,3)
DECAY        25     4.07093158E-03   # Gamma(h0)
     2.27377496E-03   2        22        22   # BR(h0 -> photon photon)
     1.47867008E-03   2        22        23   # BR(h0 -> photon Z)
     2.72774002E-02   2        23        23   # BR(h0 -> Z Z)
     2.24109973E-01   2       -24        24   # BR(h0 -> W W)
     6.98211654E-02   2        21        21   # BR(h0 -> gluon gluon)
     5.42986510E-09   2       -11        11   # BR(h0 -> Electron electron)
     2.41529865E-04   2       -13        13   # BR(h0 -> Muon muon)
     6.91618785E-02   2       -15        15   # BR(h0 -> Tau tau)
     1.98399121E-07   2        -2         2   # BR(h0 -> Up up)
     2.74804809E-02   2        -4         4   # BR(h0 -> Charm charm)
     8.81544623E-07   2        -1         1   # BR(h0 -> Down down)
     2.21382489E-04   2        -3         3   # BR(h0 -> Strange strange)
     5.77932659E-01   2        -5         5   # BR(h0 -> Bottom bottom)
DECAY        35     3.15821071E+01   # Gamma(HH)
     1.63611005E-07   2        22        22   # BR(HH -> photon photon)
     7.28976490E-07   2        22        23   # BR(HH -> photon Z)
     3.11470608E-06   2        23        23   # BR(HH -> Z Z)
     5.20871528E-06   2       -24        24   # BR(HH -> W W)
     5.31876274E-05   2        21        21   # BR(HH -> gluon gluon)
     1.06863310E-08   2       -11        11   # BR(HH -> Electron electron)
     4.75612958E-04   2       -13        13   # BR(HH -> Muon muon)
     1.30737623E-01   2       -15        15   # BR(HH -> Tau tau)
     3.99347666E-14   2        -2         2   # BR(HH -> Up up)
     5.52678565E-09   2        -4         4   # BR(HH -> Charm charm)
     2.61201486E-04   2        -6         6   # BR(HH -> Top top)
     1.09087900E-06   2        -1         1   # BR(HH -> Down down)
     2.73927734E-04   2        -3         3   # BR(HH -> Strange strange)
     6.08133068E-01   2        -5         5   # BR(HH -> Bottom bottom)
     6.48736543E-02   2  -1000024   1000024   # BR(HH -> Chargino1 chargino1)
     4.15030908E-02   2  -1000037   1000024   # BR(HH -> Chargino2 chargino1)
     4.15030908E-02   2  -1000024   1000037   # BR(HH -> Chargino1 chargino2)
     1.07542511E-02   2  -1000037   1000037   # BR(HH -> Chargino2 chargino2)
     6.50954674E-03   2   1000022   1000022   # BR(HH -> neutralino1 neutralino1)
     1.96476593E-02   2   1000022   1000023   # BR(HH -> neutralino1 neutralino2)
     1.21983599E-02   2   1000022   1000025   # BR(HH -> neutralino1 neutralino3)
     1.66430888E-05   2   1000022   1000035   # BR(HH -> neutralino1 neutralino4)
     1.17920305E-02   2   1000023   1000023   # BR(HH -> neutralino2 neutralino2)
     6.65269616E-03   2   1000023   1000025   # BR(HH -> neutralino2 neutralino3)
     1.97290538E-03   2   1000023   1000035   # BR(HH -> neutralino2 neutralino4)
     2.20746799E-03   2   1000025   1000025   # BR(HH -> neutralino3 neutralino3)
     3.14385409E-02   2   1000025   1000035   # BR(HH -> neutralino3 neutralino4)
     8.95616644E-03   2   1000035   1000035   # BR(HH -> neutralino4 neutralino4)
     2.89526707E-05   2        25        25   # BR(HH -> h0 h0)
DECAY        36     3.09847840E+01   # Gamma(A0)
    -5.49564364E-07   2        22        22   # BR(A0 -> photon photon)
    -1.07035121E-06   2        22        23   # BR(A0 -> photon Z)
    -7.33899403E-05   2        21        21   # BR(A0 -> gluon gluon)
    -1.06153844E-08   2       -11        11   # BR(A0 -> Electron electron)
     4.72455590E-04   2       -13        13   # BR(A0 -> Muon muon)
    -1.29871435E-01   2       -15        15   # BR(A0 -> Tau tau)
    -3.61885288E-14   2        -2         2   # BR(A0 -> Up up)
    -5.01018556E-09   2        -4         4   # BR(A0 -> Charm charm)
    -2.91467096E-04   2        -6         6   # BR(A0 -> Top top)
    -1.08352554E-06   2        -1         1   # BR(A0 -> Down down)
    -2.72081244E-04   2        -3         3   # BR(A0 -> Strange strange)
    -6.04087475E-01   2        -5         5   # BR(A0 -> Bottom bottom)
    -8.06991814E-02   2  -1000024   1000024   # BR(A0 -> Chargino1 chargino1)
    -2.91154068E-02   2  -1000037   1000024   # BR(A0 -> Chargino2 chargino1)
    -2.91154068E-02   2  -1000024   1000037   # BR(A0 -> Chargino1 chargino2)
    -2.26585007E-02   2  -1000037   1000037   # BR(A0 -> Chargino2 chargino2)
    -7.25267499E-03   2   1000022   1000022   # BR(A0 -> neutralino1 neutralino1)
    -2.31411485E-02   2   1000022   1000023   # BR(A0 -> neutralino1 neutralino2)
    -9.84319977E-03   2   1000022   1000025   # BR(A0 -> neutralino1 neutralino3)
    -5.27018481E-05   2   1000022   1000035   # BR(A0 -> neutralino1 neutralino4)
    -1.50031270E-02   2   1000023   1000023   # BR(A0 -> neutralino2 neutralino2)
    -4.69320237E-03   2   1000023   1000025   # BR(A0 -> neutralino2 neutralino3)
    -2.73266578E-03   2   1000023   1000035   # BR(A0 -> neutralino2 neutralino4)
    -2.86076622E-03   2   1000025   1000025   # BR(A0 -> neutralino3 neutralino3)
    -1.96075774E-02   2   1000025   1000035   # BR(A0 -> neutralino3 neutralino4)
    -1.81484425E-02   2   1000035   1000035   # BR(A0 -> neutralino4 neutralino4)
    -4.97541067E-06   2        23        25   # BR(A0 -> Z h0)
    -9.18471514E-37   2        25        25   # BR(A0 -> h0 h0)
DECAY        37     2.94269001E+01   # Gamma(Hp)
     1.13110540E-08   2       -11        12   # BR(Hp -> Electron nu_e)
     4.83582787E-04   2       -13        14   # BR(Hp -> Muon nu_mu)
     1.36788133E-01   2       -15        16   # BR(Hp -> Tau nu_tau)
     1.10183158E-06   2        -1         2   # BR(Hp -> Down up)
     1.26896029E-05   2        -3         2   # BR(Hp -> Strange up)
     6.65565684E-06   2        -5         2   # BR(Hp -> Bottom up)
     5.08702026E-08   2        -1         4   # BR(Hp -> Down charm)
     2.75703141E-04   2        -3         4   # BR(Hp -> Strange charm)
     9.32025662E-04   2        -5         4   # BR(Hp -> Bottom charm)
     3.05133753E-08   2        -1         6   # BR(Hp -> Down top)
     1.03205326E-06   2        -3         6   # BR(Hp -> Strange top)
     5.85821788E-01   2        -5         6   # BR(Hp -> Bottom top)
     3.37894425E-02   2   1000022   1000024   # BR(Hp -> neutralino1 chargino1)
     9.68740592E-04   2   1000022   1000037   # BR(Hp -> neutralino1 chargino2)
     5.62242165E-03   2   1000023   1000024   # BR(Hp -> neutralino2 chargino1)
     9.85580262E-02   2   1000023   1000037   # BR(Hp -> neutralino2 chargino2)
     3.76826606E-02   2   1000024   1000025   # BR(Hp -> chargino1 neutralino3)
     3.78404054E-02   2   1000025   1000037   # BR(Hp -> neutralino3 chargino2)
     6.05321286E-02   2   1000024   1000035   # BR(Hp -> chargino1 neutralino4)
     6.77985154E-04   2   1000035   1000037   # BR(Hp -> neutralino4 chargino2)
     5.38393369E-06   2        24        25   # BR(Hp -> W h0)
     1.45204683E-10   2        24        35   # BR(Hp -> W HH)
     7.99209757E-11   2        24        36   # BR(Hp -> W A0)
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

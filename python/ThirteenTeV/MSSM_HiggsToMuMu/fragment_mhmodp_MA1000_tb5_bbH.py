COM_ENERGY = 13000.0 # GeV
CROSS_SECTION = 1 # pb
PROCESS = 'HiggsBSM:gg2H2bbbar = on'
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
         3     5.00000000E+00   # TB
BLOCK EXTPAR
         0     0.00000000E+00   # Q
         1     9.54716519E+01   # M1
         2     2.00000000E+02   # M2
         3     1.50000000E+03   # M3
        11     1.54000000E+03   # At
        12     1.54000000E+03   # Ab
        13     1.54000000E+03   # Atau
        23     2.00000000E+02   # MUE
        25     5.00000000E+00   # TB
        26     1.00000000E+03   # MA0
        27     1.00322671E+03   # MHp
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
   1000012     4.96147383E+02   # MSf(1,1,1)
   1000011     5.02124333E+02   # MSf(1,2,1)
   2000011     5.01706018E+02   # MSf(2,2,1)
   1000002     1.49910023E+03   # MSf(1,3,1)
   2000002     1.49962019E+03   # MSf(2,3,1)
   1000001     1.50108899E+03   # MSf(1,4,1)
   2000001     1.50018986E+03   # MSf(2,4,1)
   1000014     4.96147383E+02   # MSf(1,1,2)
   1000013     5.02149324E+02   # MSf(1,2,2)
   2000013     5.01681028E+02   # MSf(2,2,2)
   1000004     1.49910022E+03   # MSf(1,3,2)
   2000004     1.49962131E+03   # MSf(2,3,2)
   1000003     1.50109009E+03   # MSf(1,4,2)
   2000003     1.50018877E+03   # MSf(2,4,2)
   1000016     9.98079268E+02   # MSf(1,1,3)
   1000015     1.00046976E+03   # MSf(1,2,3)
   2000015     1.00145112E+03   # MSf(2,2,3)
   1000006     8.76514793E+02   # MSf(1,3,3)
   2000006     1.13484824E+03   # MSf(2,3,3)
   1000005     9.99672858E+02   # MSf(1,4,3)
   2000005     1.00226018E+03   # MSf(2,4,3)
        25     1.20255691E+02   # Mh0
        35     1.00057357E+03   # MHH
        36     1.00000000E+03   # MA0
        37     1.00336885E+03   # MHp
   1000022     8.32246583E+01   # MNeu(1)
   1000023     1.46162492E+02   # MNeu(2)
   1000025    -2.07032524E+02   # MNeu(3)
   1000035     2.73117025E+02   # MNeu(4)
   1000024     1.38016688E+02   # MCha(1)
   1000037     2.71807047E+02   # MCha(2)
   1000021     1.50000000E+03   # MGl
BLOCK DMASS
         0     1.73200000E+02   # Q
        25     8.67953251E-01   # Delta Mh0
        35     5.00628807E-02   # Delta MHH
        36     0.00000000E+00   # Delta MA0
        37     2.57847880E-02   # Delta MHp
BLOCK NMIX
     1   1     9.02741035E-01   # ZNeu(1,1)
     1   2    -1.76906456E-01   # ZNeu(1,2)
     1   3     3.40847919E-01   # ZNeu(1,3)
     1   4    -1.93869609E-01   # ZNeu(1,4)
     2   1    -3.94247799E-01   # ZNeu(2,1)
     2   2    -6.93610991E-01   # ZNeu(2,2)
     2   3     4.67997695E-01   # ZNeu(2,3)
     2   4    -3.80066603E-01   # ZNeu(2,4)
     3   1     8.11445475E-02   # ZNeu(3,1)
     3   2    -1.12684390E-01   # ZNeu(3,2)
     3   3    -6.81879284E-01   # ZNeu(3,3)
     3   4    -7.18163235E-01   # ZNeu(3,4)
     4   1    -1.51798743E-01   # ZNeu(4,1)
     4   2     6.89137234E-01   # ZNeu(4,2)
     4   3     4.47036346E-01   # ZNeu(4,3)
     4   4    -5.49732225E-01   # ZNeu(4,4)
BLOCK UMIX
     1   1    -6.25444296E-01   # UCha(1,1)
     1   2     7.80268821E-01   # UCha(1,2)
     2   1     7.80268821E-01   # UCha(2,1)
     2   2     6.25444296E-01   # UCha(2,2)
BLOCK VMIX
     1   1    -7.80268821E-01   # VCha(1,1)
     1   2     6.25444296E-01   # VCha(1,2)
     2   1     6.25444296E-01   # VCha(2,1)
     2   2     7.80268821E-01   # VCha(2,2)
BLOCK STAUMIX
     1   1    -6.27000005E-01   # USf(1,1)
     1   2     7.79019251E-01   # USf(1,2)
     2   1     7.79019251E-01   # USf(2,1)
     2   2     6.27000005E-01   # USf(2,2)
BLOCK STOPMIX
     1   1     7.08166916E-01   # USf(1,1)
     1   2    -7.06045055E-01   # USf(1,2)
     2   1     7.06045055E-01   # USf(2,1)
     2   2     7.08166916E-01   # USf(2,2)
BLOCK SBOTMIX
     1   1    -4.89397966E-01   # USf(1,1)
     1   2     8.72060566E-01   # USf(1,2)
     2   1     8.72060566E-01   # USf(2,1)
     2   2     4.89397966E-01   # USf(2,2)
BLOCK ALPHA
              -2.01988607E-01   # Alpha
BLOCK DALPHA
               8.97904607E-05   # Delta Alpha
BLOCK HMIX Q= -0.99900000E+03
         1     2.00000000E+02   # MUE
         2     5.00000000E+00   # TB
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
     3   3     1.54000000E+03   # Af(3,3)
BLOCK AU Q=  0.00000000E+00
     1   1     0.00000000E+00   # Af(1,1)
     2   2     0.00000000E+00   # Af(2,2)
     3   3     1.54000000E+03   # Af(3,3)
BLOCK AD Q=  0.00000000E+00
     1   1     0.00000000E+00   # Af(1,1)
     2   2     0.00000000E+00   # Af(2,2)
     3   3     1.54000000E+03   # Af(3,3)
BLOCK YE Q=  0.00000000E+00
     1   1     1.49657094E-05   # Yf(1,1)
     2   2     3.09443379E-03   # Yf(2,2)
     3   3     5.20441717E-02   # Yf(3,3)
BLOCK YU Q=  0.00000000E+00
     1   1     1.75722993E-05   # Yf(1,1)
     2   2     7.53265896E-03   # Yf(2,2)
     3   3     1.01450741E+00   # Yf(3,3)
BLOCK YD Q=  0.00000000E+00
     1   1     1.74829084E-04   # Yf(1,1)
     2   2     2.76809607E-03   # Yf(2,2)
     3   3     1.19892640E-01   # Yf(3,3)
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
     3   3     8.01480244E+01   # Tf(3,3)
BLOCK TU Q=  0.00000000E+00
     1   1     0.00000000E+00   # Tf(1,1)
     2   2     0.00000000E+00   # Tf(2,2)
     3   3     1.56234141E+03   # Tf(3,3)
BLOCK TD Q=  0.00000000E+00
     1   1     0.00000000E+00   # Tf(1,1)
     2   2     0.00000000E+00   # Tf(2,2)
     3   3     1.84634666E+02   # Tf(3,3)
BLOCK SELMIX
     1   1     9.99999260E-01   # UASf(1,1)
     1   4    -1.21690415E-03   # UASf(1,4)
     2   2     9.72951221E-01   # UASf(2,2)
     2   5    -2.31010653E-01   # UASf(2,5)
     3   3    -6.27000005E-01   # UASf(3,3)
     3   6     7.79019251E-01   # UASf(3,6)
     4   1     1.21690415E-03   # UASf(4,1)
     4   4     9.99999260E-01   # UASf(4,4)
     5   2     2.31010653E-01   # UASf(5,2)
     5   5     9.72951221E-01   # UASf(5,5)
     6   3     7.79019251E-01   # UASf(6,3)
     6   6     6.27000005E-01   # UASf(6,6)
BLOCK USQMIX
     1   1     9.99999997E-01   # UASf(1,1)
     1   4     7.69624283E-05   # UASf(1,4)
     2   2     9.99457413E-01   # UASf(2,2)
     2   5     3.29375163E-02   # UASf(2,5)
     3   3     7.08166916E-01   # UASf(3,3)
     3   6    -7.06045055E-01   # UASf(3,6)
     4   1    -7.69624283E-05   # UASf(4,1)
     4   4     9.99999997E-01   # UASf(4,4)
     5   2    -3.29375163E-02   # UASf(5,2)
     5   5     9.99457413E-01   # UASf(5,5)
     6   3     7.06045055E-01   # UASf(6,3)
     6   6     7.08166916E-01   # UASf(6,6)
BLOCK DSQMIX
     1   1     9.99997553E-01   # UASf(1,1)
     1   4    -2.21213987E-03   # UASf(1,4)
     2   2     9.99388672E-01   # UASf(2,2)
     2   5    -3.49611623E-02   # UASf(2,5)
     3   3    -4.89397966E-01   # UASf(3,3)
     3   6     8.72060566E-01   # UASf(3,6)
     4   1     2.21213987E-03   # UASf(4,1)
     4   4     9.99997553E-01   # UASf(4,4)
     5   2     3.49611623E-02   # UASf(5,2)
     5   5     9.99388672E-01   # UASf(5,5)
     6   3     8.72060566E-01   # UASf(6,3)
     6   6     4.89397966E-01   # UASf(6,6)
BLOCK CVHMIX
     1   1     9.99998682E-01   # UH(1,1)
     1   2     1.62356041E-03   # UH(1,2)
     1   3     0.00000000E+00   # UH(1,3)
     2   1    -1.62356041E-03   # UH(2,1)
     2   2     9.99998682E-01   # UH(2,2)
     2   3     0.00000000E+00   # UH(2,3)
     3   1     0.00000000E+00   # UH(3,1)
     3   2     0.00000000E+00   # UH(3,2)
     3   3     1.00000000E+00   # UH(3,3)
DECAY        25     3.41454311E-03   # Gamma(h0)
     2.41522150E-03   2        22        22   # BR(h0 -> photon photon)
     1.13875533E-03   2        22        23   # BR(h0 -> photon Z)
     1.69004882E-02   2        23        23   # BR(h0 -> Z Z)
     1.53595322E-01   2       -24        24   # BR(h0 -> W W)
     7.39064728E-02   2        21        21   # BR(h0 -> gluon gluon)
     5.85460105E-09   2       -11        11   # BR(h0 -> Electron electron)
     2.60420035E-04   2       -13        13   # BR(h0 -> Muon muon)
     7.50180992E-02   2       -15        15   # BR(h0 -> Tau tau)
     2.28228310E-07   2        -2         2   # BR(h0 -> Up up)
     3.16090110E-02   2        -4         4   # BR(h0 -> Charm charm)
     9.70294425E-07   2        -1         1   # BR(h0 -> Down down)
     2.43674421E-04   2        -3         3   # BR(h0 -> Strange strange)
     6.44911331E-01   2        -5         5   # BR(h0 -> Bottom bottom)
DECAY        35     1.37252512E+01   # Gamma(HH)
    -1.53543836E-06   2        22        22   # BR(HH -> photon photon)
    -2.11724733E-06   2        22        23   # BR(HH -> photon Z)
    -3.23248025E-04   2        23        23   # BR(HH -> Z Z)
    -4.33620364E-04   2       -24        24   # BR(HH -> W W)
    -1.07773125E-04   2        21        21   # BR(HH -> gluon gluon)
    -2.77873475E-10   2       -11        11   # BR(HH -> Electron electron)
     1.23680623E-05   2       -13        13   # BR(HH -> Muon muon)
    -3.55999415E-03   2       -15        15   # BR(HH -> Tau tau)
    -1.36405623E-11   2        -2         2   # BR(HH -> Up up)
    -1.88817093E-06   2        -4         4   # BR(HH -> Charm charm)
    -1.20606376E-01   2        -6         6   # BR(HH -> Top top)
    -3.27632963E-08   2        -1         1   # BR(HH -> Down down)
    -8.22833932E-06   2        -3         3   # BR(HH -> Strange strange)
    -2.11326954E-02   2        -5         5   # BR(HH -> Bottom bottom)
    -1.42315157E-01   2  -1000024   1000024   # BR(HH -> Chargino1 chargino1)
    -1.75967164E-01   2  -1000037   1000024   # BR(HH -> Chargino2 chargino1)
    -1.75967164E-01   2  -1000024   1000037   # BR(HH -> Chargino1 chargino2)
    -2.40941496E-02   2  -1000037   1000037   # BR(HH -> Chargino2 chargino2)
    -2.17162878E-02   2   1000022   1000022   # BR(HH -> neutralino1 neutralino1)
    -4.28183757E-02   2   1000022   1000023   # BR(HH -> neutralino1 neutralino2)
    -6.70433978E-02   2   1000022   1000025   # BR(HH -> neutralino1 neutralino3)
    -2.90157202E-05   2   1000022   1000035   # BR(HH -> neutralino1 neutralino4)
    -1.77814112E-02   2   1000023   1000023   # BR(HH -> neutralino2 neutralino2)
    -3.06783571E-02   2   1000023   1000025   # BR(HH -> neutralino2 neutralino3)
    -4.12149522E-03   2   1000023   1000035   # BR(HH -> neutralino2 neutralino4)
    -7.24321164E-03   2   1000025   1000025   # BR(HH -> neutralino3 neutralino3)
    -1.19230914E-01   2   1000025   1000035   # BR(HH -> neutralino3 neutralino4)
    -2.23849047E-02   2   1000035   1000035   # BR(HH -> neutralino4 neutralino4)
    -2.35717542E-03   2        25        25   # BR(HH -> h0 h0)
DECAY        36     1.38027113E+01   # Gamma(A0)
     1.45961630E-06   2        22        22   # BR(A0 -> photon photon)
     2.12812489E-06   2        22        23   # BR(A0 -> photon Z)
     1.67817091E-04   2        21        21   # BR(A0 -> gluon gluon)
     2.71742901E-10   2       -11        11   # BR(A0 -> Electron electron)
     1.20951968E-05   2       -13        13   # BR(A0 -> Muon muon)
     3.48143814E-03   2       -15        15   # BR(A0 -> Tau tau)
     1.27886223E-11   2        -2         2   # BR(A0 -> Up up)
     1.77007479E-06   2        -4         4   # BR(A0 -> Charm charm)
     1.29021649E-01   2        -6         6   # BR(A0 -> Top top)
     3.20426795E-08   2        -1         1   # BR(A0 -> Down down)
     8.04736350E-06   2        -3         3   # BR(A0 -> Strange strange)
     2.06742418E-02   2        -5         5   # BR(A0 -> Bottom bottom)
     2.58980463E-01   2  -1000024   1000024   # BR(A0 -> Chargino1 chargino1)
     6.54389751E-02   2  -1000037   1000024   # BR(A0 -> Chargino2 chargino1)
     6.54389751E-02   2  -1000024   1000037   # BR(A0 -> Chargino1 chargino2)
     1.26057639E-01   2  -1000037   1000037   # BR(A0 -> Chargino2 chargino2)
     3.53366869E-02   2   1000022   1000022   # BR(A0 -> neutralino1 neutralino1)
     8.12051429E-02   2   1000022   1000023   # BR(A0 -> neutralino1 neutralino2)
     2.17445558E-02   2   1000022   1000025   # BR(A0 -> neutralino1 neutralino3)
     1.42627040E-03   2   1000022   1000035   # BR(A0 -> neutralino1 neutralino4)
     3.76286837E-02   2   1000023   1000023   # BR(A0 -> neutralino2 neutralino2)
     7.13094399E-03   2   1000023   1000025   # BR(A0 -> neutralino2 neutralino3)
     5.71105000E-03   2   1000023   1000035   # BR(A0 -> neutralino2 neutralino4)
     3.66845144E-03   2   1000025   1000025   # BR(A0 -> neutralino3 neutralino3)
     4.83626678E-02   2   1000025   1000035   # BR(A0 -> neutralino3 neutralino4)
     8.80311136E-02   2   1000035   1000035   # BR(A0 -> neutralino4 neutralino4)
     4.67701992E-04   2        23        25   # BR(A0 -> Z h0)
     1.63080149E-38   2        25        25   # BR(A0 -> h0 h0)
DECAY        37     1.36987704E+01   # Gamma(Hp)
     3.03060433E-10   2       -11        12   # BR(Hp -> Electron nu_e)
     1.29567775E-05   2       -13        14   # BR(Hp -> Muon nu_mu)
     3.66501790E-03   2       -15        16   # BR(Hp -> Tau nu_tau)
     3.13302517E-08   2        -1         2   # BR(Hp -> Down up)
     3.61888241E-07   2        -3         2   # BR(Hp -> Strange up)
     2.25698948E-07   2        -5         2   # BR(Hp -> Bottom up)
     8.07665946E-08   2        -1         4   # BR(Hp -> Down charm)
     9.55775219E-06   2        -3         4   # BR(Hp -> Strange charm)
     3.16084610E-05   2        -5         4   # BR(Hp -> Bottom charm)
     8.21551438E-06   2        -1         6   # BR(Hp -> Down top)
     1.79136174E-04   2        -3         6   # BR(Hp -> Strange top)
     1.48394760E-01   2        -5         6   # BR(Hp -> Bottom top)
     8.23166739E-02   2   1000022   1000024   # BR(Hp -> neutralino1 chargino1)
     8.29183753E-03   2   1000022   1000037   # BR(Hp -> neutralino1 chargino2)
     2.30461515E-02   2   1000023   1000024   # BR(Hp -> neutralino2 chargino1)
     2.95455370E-01   2   1000023   1000037   # BR(Hp -> neutralino2 chargino2)
     1.18272924E-01   2   1000024   1000025   # BR(Hp -> chargino1 neutralino3)
     1.22263295E-01   2   1000025   1000037   # BR(Hp -> neutralino3 chargino2)
     1.94853634E-01   2   1000024   1000035   # BR(Hp -> chargino1 neutralino4)
     2.66175282E-03   2   1000035   1000037   # BR(Hp -> neutralino4 chargino2)
     4.79031441E-04   2        24        25   # BR(Hp -> W h0)
     1.69000166E-11   2        24        35   # BR(Hp -> W HH)
     4.29478601E-11   2        24        36   # BR(Hp -> W A0)
     4.12569132E-11   2  -1000011   1000012   # BR(Hp -> Selectron1 snu_e1)
     2.86912464E-05   2  -2000011   1000012   # BR(Hp -> Selectron2 snu_e1)
     1.48385236E-06   2  -1000013   1000014   # BR(Hp -> Smuon1 snu_mu1)
     2.72019016E-05   2  -2000013   1000014   # BR(Hp -> Smuon2 snu_mu1)
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

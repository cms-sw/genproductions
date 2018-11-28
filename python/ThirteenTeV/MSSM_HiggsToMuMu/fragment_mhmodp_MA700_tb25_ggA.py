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
         3     2.50000000E+01   # TB
BLOCK EXTPAR
         0     0.00000000E+00   # Q
         1     9.54716519E+01   # M1
         2     2.00000000E+02   # M2
         3     1.50000000E+03   # M3
        11     1.50800000E+03   # At
        12     1.50800000E+03   # Ab
        13     1.50800000E+03   # Atau
        23     2.00000000E+02   # MUE
        25     2.50000000E+01   # TB
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
   1000012     4.95838375E+02   # MSf(1,1,1)
   1000011     5.02293635E+02   # MSf(1,2,1)
   2000011     5.01842018E+02   # MSf(2,2,1)
   1000002     1.49902834E+03   # MSf(1,3,1)
   2000002     1.49958985E+03   # MSf(2,3,1)
   1000001     1.50117603E+03   # MSf(1,4,1)
   2000001     1.50020494E+03   # MSf(2,4,1)
   1000014     4.95838375E+02   # MSf(1,1,2)
   1000013     5.02640084E+02   # MSf(1,2,2)
   2000013     5.01495040E+02   # MSf(2,2,2)
   1000004     1.49902888E+03   # MSf(1,3,2)
   2000004     1.49959042E+03   # MSf(2,3,2)
   1000003     1.50119986E+03   # MSf(1,4,2)
   2000003     1.50018109E+03   # MSf(2,4,2)
   1000016     9.97925696E+02   # MSf(1,1,3)
   1000015     9.97930750E+02   # MSf(1,2,3)
   2000015     1.00413388E+03   # MSf(2,2,3)
   1000006     8.76427249E+02   # MSf(1,3,3)
   2000006     1.13478079E+03   # MSf(2,3,3)
   1000005     9.94259418E+02   # MSf(1,4,3)
   2000005     1.00778084E+03   # MSf(2,4,3)
        25     1.24972400E+02   # Mh0
        35     6.99914214E+02   # MHH
        36     7.00000000E+02   # MA0
        37     7.04943652E+02   # MHp
   1000022     8.81217311E+01   # MNeu(1)
   1000023     1.51770777E+02   # MNeu(2)
   1000025    -2.10328954E+02   # MNeu(3)
   1000035     2.65908097E+02   # MNeu(4)
   1000024     1.48224451E+02   # MCha(1)
   1000037     2.66377906E+02   # MCha(2)
   1000021     1.50000000E+03   # MGl
BLOCK DMASS
         0     1.73200000E+02   # Q
        25     7.04221850E-01   # Delta Mh0
        35     4.04338275E-03   # Delta MHH
        36     0.00000000E+00   # Delta MA0
        37     8.60605761E-03   # Delta MHp
BLOCK NMIX
     1   1     9.31988478E-01   # ZNeu(1,1)
     1   2    -1.14406215E-01   # ZNeu(1,2)
     1   3     3.11018111E-01   # ZNeu(1,3)
     1   4    -1.46889171E-01   # ZNeu(1,4)
     2   1    -3.18015719E-01   # ZNeu(2,1)
     2   2    -6.93652772E-01   # ZNeu(2,2)
     2   3     5.10771151E-01   # ZNeu(2,3)
     2   4    -3.96010941E-01   # ZNeu(2,4)
     3   1     9.68557338E-02   # ZNeu(3,1)
     3   2    -1.34875686E-01   # ZNeu(3,2)
     3   3    -6.78023080E-01   # ZNeu(3,3)
     3   4    -7.16039258E-01   # ZNeu(3,4)
     4   1    -1.44507597E-01   # ZNeu(4,1)
     4   2     6.98258977E-01   # ZNeu(4,2)
     4   3     4.27393577E-01   # ZNeu(4,3)
     4   4    -5.55775752E-01   # ZNeu(4,4)
BLOCK UMIX
     1   1    -6.07017863E-01   # UCha(1,1)
     1   2     7.94688187E-01   # UCha(1,2)
     2   1     7.94688187E-01   # UCha(2,1)
     2   2     6.07017863E-01   # UCha(2,2)
BLOCK VMIX
     1   1    -7.94688187E-01   # VCha(1,1)
     1   2     6.07017863E-01   # VCha(1,2)
     2   1     6.07017863E-01   # VCha(2,1)
     2   2     7.94688187E-01   # VCha(2,2)
BLOCK STAUMIX
     1   1     6.94077482E-01   # USf(1,1)
     1   2     7.19900305E-01   # USf(1,2)
     2   1     7.19900305E-01   # USf(2,1)
     2   2    -6.94077482E-01   # USf(2,2)
BLOCK STOPMIX
     1   1     7.08251522E-01   # USf(1,1)
     1   2    -7.05960185E-01   # USf(1,2)
     2   1     7.05960185E-01   # USf(2,1)
     2   2     7.08251522E-01   # USf(2,2)
BLOCK SBOTMIX
     1   1     6.67964822E-01   # USf(1,1)
     1   2     7.44192849E-01   # USf(1,2)
     2   1     7.44192849E-01   # USf(2,1)
     2   2    -6.67964822E-01   # USf(2,2)
BLOCK ALPHA
              -4.22966907E-02   # Alpha
BLOCK DALPHA
               4.26092889E-05   # Delta Alpha
BLOCK HMIX Q= -0.99900000E+03
         1     2.00000000E+02   # MUE
         2     2.50000000E+01   # TB
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
     3   3     1.50800000E+03   # Af(3,3)
BLOCK AU Q=  0.00000000E+00
     1   1     0.00000000E+00   # Af(1,1)
     2   2     0.00000000E+00   # Af(2,2)
     3   3     1.50800000E+03   # Af(3,3)
BLOCK AD Q=  0.00000000E+00
     1   1     0.00000000E+00   # Af(1,1)
     2   2     0.00000000E+00   # Af(2,2)
     3   3     1.50800000E+03   # Af(3,3)
BLOCK YE Q=  0.00000000E+00
     1   1     7.34341041E-05   # Yf(1,1)
     2   2     1.51838424E-02   # Yf(2,2)
     3   3     2.55371598E-01   # Yf(3,3)
BLOCK YU Q=  0.00000000E+00
     1   1     1.72448364E-05   # Yf(1,1)
     2   2     7.39228656E-03   # Yf(2,2)
     3   3     9.95601891E-01   # Yf(3,3)
BLOCK YD Q=  0.00000000E+00
     1   1     8.40743677E-04   # Yf(1,1)
     2   2     1.33110713E-02   # Yf(2,2)
     3   3     5.53780613E-01   # Yf(3,3)
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
     3   3     3.85100369E+02   # Tf(3,3)
BLOCK TU Q=  0.00000000E+00
     1   1     0.00000000E+00   # Tf(1,1)
     2   2     0.00000000E+00   # Tf(2,2)
     3   3     1.50136765E+03   # Tf(3,3)
BLOCK TD Q=  0.00000000E+00
     1   1     0.00000000E+00   # Tf(1,1)
     2   2     0.00000000E+00   # Tf(2,2)
     3   3     8.35101165E+02   # Tf(3,3)
BLOCK SELMIX
     1   1     9.99984128E-01   # UASf(1,1)
     1   4    -5.63422645E-03   # UASf(1,4)
     2   2     8.34980634E-01   # UASf(2,2)
     2   5    -5.50279330E-01   # UASf(2,5)
     3   3     6.94077482E-01   # UASf(3,3)
     3   6     7.19900305E-01   # UASf(3,6)
     4   1     5.63422645E-03   # UASf(4,1)
     4   4     9.99984128E-01   # UASf(4,4)
     5   2     5.50279330E-01   # UASf(5,2)
     5   5     8.34980634E-01   # UASf(5,5)
     6   3     7.19900305E-01   # UASf(6,3)
     6   6    -6.94077482E-01   # UASf(6,6)
BLOCK USQMIX
     1   1     1.00000000E+00   # UASf(1,1)
     1   4     1.42539883E-05   # UASf(1,4)
     2   2     9.99981335E-01   # UASf(2,2)
     2   5     6.10986751E-03   # UASf(2,5)
     3   3     7.08251522E-01   # UASf(3,3)
     3   6    -7.05960185E-01   # UASf(3,6)
     4   1    -1.42539883E-05   # UASf(4,1)
     4   4     1.00000000E+00   # UASf(4,4)
     5   2    -6.10986751E-03   # UASf(5,2)
     5   5     9.99981335E-01   # UASf(5,5)
     6   3     7.05960185E-01   # UASf(6,3)
     6   6     7.08251522E-01   # UASf(6,6)
BLOCK DSQMIX
     1   1     9.99949630E-01   # UASf(1,1)
     1   4    -1.00368449E-02   # UASf(1,4)
     2   2     9.88183453E-01   # UASf(2,2)
     2   5    -1.53275777E-01   # UASf(2,5)
     3   3     6.67964822E-01   # UASf(3,3)
     3   6     7.44192849E-01   # UASf(3,6)
     4   1     1.00368449E-02   # UASf(4,1)
     4   4     9.99949630E-01   # UASf(4,4)
     5   2     1.53275777E-01   # UASf(5,2)
     5   5     9.88183453E-01   # UASf(5,5)
     6   3     7.44192849E-01   # UASf(6,3)
     6   6    -6.67964822E-01   # UASf(6,6)
BLOCK CVHMIX
     1   1     9.99999555E-01   # UH(1,1)
     1   2     9.43905167E-04   # UH(1,2)
     1   3     0.00000000E+00   # UH(1,3)
     2   1    -9.43905167E-04   # UH(2,1)
     2   2     9.99999555E-01   # UH(2,2)
     2   3     0.00000000E+00   # UH(2,3)
     3   1     0.00000000E+00   # UH(3,1)
     3   2     0.00000000E+00   # UH(3,2)
     3   3     1.00000000E+00   # UH(3,3)
DECAY        25     4.09009722E-03   # Gamma(h0)
     2.25330460E-03   2        22        22   # BR(h0 -> photon photon)
     1.43881627E-03   2        22        23   # BR(h0 -> photon Z)
     2.61427262E-02   2        23        23   # BR(h0 -> Z Z)
     2.16076912E-01   2       -24        24   # BR(h0 -> W W)
     6.89510070E-02   2        21        21   # BR(h0 -> gluon gluon)
     5.44530590E-09   2       -11        11   # BR(h0 -> Electron electron)
     2.42216848E-04   2       -13        13   # BR(h0 -> Muon muon)
     6.95790131E-02   2       -15        15   # BR(h0 -> Tau tau)
     1.97039804E-07   2        -2         2   # BR(h0 -> Up up)
     2.72920327E-02   2        -4         4   # BR(h0 -> Charm charm)
     8.90147996E-07   2        -1         1   # BR(h0 -> Down down)
     2.23545412E-04   2        -3         3   # BR(h0 -> Strange strange)
     5.87799333E-01   2        -5         5   # BR(h0 -> Bottom bottom)
DECAY        35     1.23715049E+01   # Gamma(HH)
     7.31569652E-07   2        22        22   # BR(HH -> photon photon)
     2.30659141E-06   2        22        23   # BR(HH -> photon Z)
     2.57314574E-05   2        23        23   # BR(HH -> Z Z)
     4.75181743E-05   2       -24        24   # BR(HH -> W W)
     4.07067462E-05   2        21        21   # BR(HH -> gluon gluon)
     5.67787711E-09   2       -11        11   # BR(HH -> Electron electron)
     2.52693148E-04   2       -13        13   # BR(HH -> Muon muon)
     7.10748462E-02   2       -15        15   # BR(HH -> Tau tau)
     4.19658566E-13   2        -2         2   # BR(HH -> Up up)
     5.80844296E-08   2        -4         4   # BR(HH -> Charm charm)
     2.92909241E-03   2        -6         6   # BR(HH -> Top top)
     6.48321591E-07   2        -1         1   # BR(HH -> Down down)
     1.62811146E-04   2        -3         3   # BR(HH -> Strange strange)
     3.91992854E-01   2        -5         5   # BR(HH -> Bottom bottom)
     1.30498895E-01   2  -1000024   1000024   # BR(HH -> Chargino1 chargino1)
     9.02527537E-02   2  -1000037   1000024   # BR(HH -> Chargino2 chargino1)
     9.02527537E-02   2  -1000024   1000037   # BR(HH -> Chargino1 chargino2)
     1.47422443E-02   2  -1000037   1000037   # BR(HH -> Chargino2 chargino2)
     1.46514347E-02   2   1000022   1000022   # BR(HH -> neutralino1 neutralino1)
     4.08566693E-02   2   1000022   1000023   # BR(HH -> neutralino1 neutralino2)
     2.84592847E-02   2   1000022   1000025   # BR(HH -> neutralino1 neutralino3)
     1.81667902E-05   2   1000022   1000035   # BR(HH -> neutralino1 neutralino4)
     2.26381762E-02   2   1000023   1000023   # BR(HH -> neutralino2 neutralino2)
     1.52009836E-02   2   1000023   1000025   # BR(HH -> neutralino2 neutralino3)
     3.47872653E-03   2   1000023   1000035   # BR(HH -> neutralino2 neutralino4)
     4.13211029E-03   2   1000025   1000025   # BR(HH -> neutralino3 neutralino3)
     6.55603496E-02   2   1000025   1000035   # BR(HH -> neutralino3 neutralino4)
     1.24760695E-02   2   1000035   1000035   # BR(HH -> neutralino4 neutralino4)
     2.51378660E-04   2        25        25   # BR(HH -> h0 h0)
DECAY        36     1.22575441E+01   # Gamma(A0)
    -1.30918148E-06   2        22        22   # BR(A0 -> photon photon)
    -2.90869675E-06   2        22        23   # BR(A0 -> photon Z)
    -7.67271132E-05   2        21        21   # BR(A0 -> gluon gluon)
    -5.64382224E-09   2       -11        11   # BR(A0 -> Electron electron)
     2.51177508E-04   2       -13        13   # BR(A0 -> Muon muon)
    -7.06492963E-02   2       -15        15   # BR(A0 -> Tau tau)
    -3.79567597E-13   2        -2         2   # BR(A0 -> Up up)
    -5.25418075E-08   2        -4         4   # BR(A0 -> Charm charm)
    -3.52190101E-03   2        -6         6   # BR(A0 -> Top top)
    -6.44413903E-07   2        -1         1   # BR(A0 -> Down down)
    -1.61829830E-04   2        -3         3   # BR(A0 -> Strange strange)
    -3.89672549E-01   2        -5         5   # BR(A0 -> Bottom bottom)
    -1.76764583E-01   2  -1000024   1000024   # BR(A0 -> Chargino1 chargino1)
    -5.16553016E-02   2  -1000037   1000024   # BR(A0 -> Chargino2 chargino1)
    -5.16553016E-02   2  -1000024   1000037   # BR(A0 -> Chargino1 chargino2)
    -4.69794725E-02   2  -1000037   1000037   # BR(A0 -> Chargino2 chargino2)
    -1.70718652E-02   2   1000022   1000022   # BR(A0 -> neutralino1 neutralino1)
    -5.17809395E-02   2   1000022   1000023   # BR(A0 -> neutralino1 neutralino2)
    -1.95661051E-02   2   1000022   1000025   # BR(A0 -> neutralino1 neutralino3)
    -1.63246611E-04   2   1000022   1000035   # BR(A0 -> neutralino1 neutralino4)
    -3.19892354E-02   2   1000023   1000023   # BR(A0 -> neutralino2 neutralino2)
    -8.51659723E-03   2   1000023   1000025   # BR(A0 -> neutralino2 neutralino3)
    -5.39293238E-03   2   1000023   1000035   # BR(A0 -> neutralino2 neutralino4)
    -5.48953969E-03   2   1000025   1000025   # BR(A0 -> neutralino3 neutralino3)
    -3.16513995E-02   2   1000025   1000035   # BR(A0 -> neutralino3 neutralino4)
    -3.69427734E-02   2   1000035   1000035   # BR(A0 -> neutralino4 neutralino4)
    -4.23054039E-05   2        23        25   # BR(A0 -> Z h0)
    -1.23267306E-36   2        25        25   # BR(A0 -> h0 h0)
DECAY        37     1.17165474E+01   # Gamma(Hp)
     6.22364730E-09   2       -11        12   # BR(Hp -> Electron nu_e)
     2.66080302E-04   2       -13        14   # BR(Hp -> Muon nu_mu)
     7.52643020E-02   2       -15        16   # BR(Hp -> Tau nu_tau)
     6.48834539E-07   2        -1         2   # BR(Hp -> Down up)
     7.45868288E-06   2        -3         2   # BR(Hp -> Strange up)
     4.29169984E-06   2        -5         2   # BR(Hp -> Bottom up)
     3.24421868E-08   2        -1         4   # BR(Hp -> Down charm)
     1.62424694E-04   2        -3         4   # BR(Hp -> Strange charm)
     6.00987822E-04   2        -5         4   # BR(Hp -> Bottom charm)
     2.64628372E-07   2        -1         6   # BR(Hp -> Down top)
     5.97893766E-06   2        -3         6   # BR(Hp -> Strange top)
     3.71102888E-01   2        -5         6   # BR(Hp -> Bottom top)
     7.14511654E-02   2   1000022   1000024   # BR(Hp -> neutralino1 chargino1)
     2.31598005E-03   2   1000022   1000037   # BR(Hp -> neutralino1 chargino2)
     1.24922428E-02   2   1000023   1000024   # BR(Hp -> neutralino2 chargino1)
     1.94392605E-01   2   1000023   1000037   # BR(Hp -> neutralino2 chargino2)
     7.80330777E-02   2   1000024   1000025   # BR(Hp -> chargino1 neutralino3)
     7.19833015E-02   2   1000025   1000037   # BR(Hp -> neutralino3 chargino2)
     1.20642731E-01   2   1000024   1000035   # BR(Hp -> chargino1 neutralino4)
     1.22765608E-03   2   1000035   1000037   # BR(Hp -> neutralino4 chargino2)
     4.58759526E-05   2        24        25   # BR(Hp -> W h0)
     3.70748141E-10   2        24        35   # BR(Hp -> W HH)
     3.40228042E-10   2        24        36   # BR(Hp -> W A0)
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

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
         3     1.00000000E+01   # TB
BLOCK EXTPAR
         0     0.00000000E+00   # Q
         1     9.54716519E+01   # M1
         2     2.00000000E+02   # M2
         3     1.50000000E+03   # M3
        11     1.52000000E+03   # At
        12     1.52000000E+03   # Ab
        13     1.52000000E+03   # Atau
        23     2.00000000E+02   # MUE
        25     1.00000000E+01   # TB
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
   1000012     4.95907995E+02   # MSf(1,1,1)
   1000011     5.02255496E+02   # MSf(1,2,1)
   2000011     5.01811396E+02   # MSf(2,2,1)
   1000002     1.49904454E+03   # MSf(1,3,1)
   2000002     1.49959668E+03   # MSf(2,3,1)
   1000001     1.50115636E+03   # MSf(1,4,1)
   2000001     1.50020160E+03   # MSf(2,4,1)
   1000014     4.95907995E+02   # MSf(1,1,2)
   1000013     5.02339352E+02   # MSf(1,2,2)
   2000013     5.01727474E+02   # MSf(2,2,2)
   1000004     1.49904496E+03   # MSf(1,3,2)
   2000004     1.49959737E+03   # MSf(2,3,2)
   1000003     1.50116044E+03   # MSf(1,4,2)
   2000003     1.50019752E+03   # MSf(2,4,2)
   1000016     9.97960289E+02   # MSf(1,1,3)
   1000015     1.00057941E+03   # MSf(1,2,3)
   2000015     1.00146014E+03   # MSf(2,2,3)
   1000006     8.76446970E+02   # MSf(1,3,3)
   2000006     1.13479599E+03   # MSf(2,3,3)
   1000005     9.99823082E+02   # MSf(1,4,3)
   2000005     1.00222830E+03   # MSf(2,4,3)
        25     1.23840917E+02   # Mh0
        35     9.00107300E+02   # MHH
        36     9.00000000E+02   # MA0
        37     9.03736897E+02   # MHp
   1000022     8.62939893E+01   # MNeu(1)
   1000023     1.49424226E+02   # MNeu(2)
   1000025    -2.09064388E+02   # MNeu(3)
   1000035     2.68817824E+02   # MNeu(4)
   1000024     1.44153518E+02   # MCha(1)
   1000037     2.68602755E+02   # MCha(2)
   1000021     1.50000000E+03   # MGl
BLOCK DMASS
         0     1.73200000E+02   # Q
        25     7.36445466E-01   # Delta Mh0
        35     1.14367797E-02   # Delta MHH
        36     0.00000000E+00   # Delta MA0
        37     2.58098426E-02   # Delta MHp
BLOCK NMIX
     1   1     9.21138506E-01   # ZNeu(1,1)
     1   2    -1.38923513E-01   # ZNeu(1,2)
     1   3     3.23973955E-01   # ZNeu(1,3)
     1   4    -1.65060554E-01   # ZNeu(1,4)
     2   1    -3.48437302E-01   # ZNeu(2,1)
     2   2    -6.94538675E-01   # ZNeu(2,2)
     2   3     4.94343250E-01   # ZNeu(2,3)
     2   4    -3.89656549E-01   # ZNeu(2,4)
     3   1     9.12601053E-02   # ZNeu(3,1)
     3   2    -1.26949251E-01   # ZNeu(3,2)
     3   3    -6.79246732E-01   # ZNeu(3,3)
     3   4    -7.17063008E-01   # ZNeu(3,4)
     4   1    -1.47536069E-01   # ZNeu(4,1)
     4   2     6.94406346E-01   # ZNeu(4,2)
     4   3     4.35074137E-01   # ZNeu(4,3)
     4   4    -5.53844228E-01   # ZNeu(4,4)
BLOCK UMIX
     1   1    -6.13722981E-01   # UCha(1,1)
     1   2     7.89521439E-01   # UCha(1,2)
     2   1     7.89521439E-01   # UCha(2,1)
     2   2     6.13722981E-01   # UCha(2,2)
BLOCK VMIX
     1   1    -7.89521439E-01   # VCha(1,1)
     1   2     6.13722981E-01   # VCha(1,2)
     2   1     6.13722981E-01   # VCha(2,1)
     2   2     7.89521439E-01   # VCha(2,2)
BLOCK STAUMIX
     1   1     6.11193766E-01   # USf(1,1)
     1   2     7.91481005E-01   # USf(1,2)
     2   1     7.91481005E-01   # USf(2,1)
     2   2    -6.11193766E-01   # USf(2,2)
BLOCK STOPMIX
     1   1     7.08232465E-01   # USf(1,1)
     1   2    -7.05979302E-01   # USf(1,2)
     2   1     7.05979302E-01   # USf(2,1)
     2   2     7.08232465E-01   # USf(2,2)
BLOCK SBOTMIX
     1   1     4.49962257E-01   # USf(1,1)
     1   2     8.93047573E-01   # USf(1,2)
     2   1     8.93047573E-01   # USf(2,1)
     2   2    -4.49962257E-01   # USf(2,2)
BLOCK ALPHA
              -1.02781658E-01   # Alpha
BLOCK DALPHA
               5.91407990E-05   # Delta Alpha
BLOCK HMIX Q= -0.99900000E+03
         1     2.00000000E+02   # MUE
         2     1.00000000E+01   # TB
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
     3   3     1.52000000E+03   # Af(3,3)
BLOCK AU Q=  0.00000000E+00
     1   1     0.00000000E+00   # Af(1,1)
     2   2     0.00000000E+00   # Af(2,2)
     3   3     1.52000000E+03   # Af(3,3)
BLOCK AD Q=  0.00000000E+00
     1   1     0.00000000E+00   # Af(1,1)
     2   2     0.00000000E+00   # Af(2,2)
     3   3     1.52000000E+03   # Af(3,3)
BLOCK YE Q=  0.00000000E+00
     1   1     2.94965567E-05   # Yf(1,1)
     2   2     6.09895188E-03   # Yf(2,2)
     3   3     1.02576084E-01   # Yf(3,3)
BLOCK YU Q=  0.00000000E+00
     1   1     1.73169981E-05   # Yf(1,1)
     2   2     7.42321984E-03   # Yf(2,2)
     3   3     9.99768022E-01   # Yf(3,3)
BLOCK YD Q=  0.00000000E+00
     1   1     3.42833578E-04   # Yf(1,1)
     2   2     5.42808075E-03   # Yf(2,2)
     3   3     2.32696980E-01   # Yf(3,3)
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
     3   3     1.55915647E+02   # Tf(3,3)
BLOCK TU Q=  0.00000000E+00
     1   1     0.00000000E+00   # Tf(1,1)
     2   2     0.00000000E+00   # Tf(2,2)
     3   3     1.51964739E+03   # Tf(3,3)
BLOCK TD Q=  0.00000000E+00
     1   1     0.00000000E+00   # Tf(1,1)
     2   2     0.00000000E+00   # Tf(2,2)
     3   3     3.53699410E+02   # Tf(3,3)
BLOCK SELMIX
     1   1     9.99997373E-01   # UASf(1,1)
     1   4    -2.29196501E-03   # UASf(1,4)
     2   2     9.28921279E-01   # UASf(2,2)
     2   5    -3.70277270E-01   # UASf(2,5)
     3   3     6.11193766E-01   # UASf(3,3)
     3   6     7.91481005E-01   # UASf(3,6)
     4   1     2.29196501E-03   # UASf(4,1)
     4   4     9.99997373E-01   # UASf(4,4)
     5   2     3.70277270E-01   # UASf(5,2)
     5   5     9.28921279E-01   # UASf(5,5)
     6   3     7.91481005E-01   # UASf(6,3)
     6   6    -6.11193766E-01   # UASf(6,6)
BLOCK USQMIX
     1   1     9.99999999E-01   # UASf(1,1)
     1   4     3.62387194E-05   # UASf(1,4)
     2   2     9.99879422E-01   # UASf(2,2)
     2   5     1.55287116E-02   # UASf(2,5)
     3   3     7.08232465E-01   # UASf(3,3)
     3   6    -7.05979302E-01   # UASf(3,6)
     4   1    -3.62387194E-05   # UASf(4,1)
     4   4     9.99999999E-01   # UASf(4,4)
     5   2    -1.55287116E-02   # UASf(5,2)
     5   5     9.99879422E-01   # UASf(5,5)
     6   3     7.05979302E-01   # UASf(6,3)
     6   6     7.08232465E-01   # UASf(6,6)
BLOCK DSQMIX
     1   1     9.99991408E-01   # UASf(1,1)
     1   4    -4.14528410E-03   # UASf(1,4)
     2   2     9.97871270E-01   # UASf(2,2)
     2   5    -6.52144862E-02   # UASf(2,5)
     3   3     4.49962257E-01   # UASf(3,3)
     3   6     8.93047573E-01   # UASf(3,6)
     4   1     4.14528410E-03   # UASf(4,1)
     4   4     9.99991408E-01   # UASf(4,4)
     5   2     6.52144862E-02   # UASf(5,2)
     5   5     9.97871270E-01   # UASf(5,5)
     6   3     8.93047573E-01   # UASf(6,3)
     6   6    -4.49962257E-01   # UASf(6,6)
BLOCK CVHMIX
     1   1     9.99999393E-01   # UH(1,1)
     1   2     1.10143617E-03   # UH(1,2)
     1   3     0.00000000E+00   # UH(1,3)
     2   1    -1.10143617E-03   # UH(2,1)
     2   2     9.99999393E-01   # UH(2,2)
     2   3     0.00000000E+00   # UH(2,3)
     3   1     0.00000000E+00   # UH(3,1)
     3   2     0.00000000E+00   # UH(3,2)
     3   3     1.00000000E+00   # UH(3,3)
DECAY        25     3.83428241E-03   # Gamma(h0)
     2.35445041E-03   2        22        22   # BR(h0 -> photon photon)
     1.40338296E-03   2        22        23   # BR(h0 -> photon Z)
     2.41702906E-02   2        23        23   # BR(h0 -> Z Z)
     2.04348715E-01   2       -24        24   # BR(h0 -> W W)
     7.17558945E-02   2        21        21   # BR(h0 -> gluon gluon)
     5.45909574E-09   2       -11        11   # BR(h0 -> Electron electron)
     2.42829620E-04   2       -13        13   # BR(h0 -> Muon muon)
     6.99136046E-02   2       -15        15   # BR(h0 -> Tau tau)
     2.08509948E-07   2        -2         2   # BR(h0 -> Up up)
     2.88801080E-02   2        -4         4   # BR(h0 -> Charm charm)
     8.98835609E-07   2        -1         1   # BR(h0 -> Down down)
     2.25728560E-04   2        -3         3   # BR(h0 -> Strange strange)
     5.96703883E-01   2        -5         5   # BR(h0 -> Bottom bottom)
DECAY        35     1.15700401E+01   # Gamma(HH)
    -1.11330339E-06   2        22        22   # BR(HH -> photon photon)
    -2.37721925E-06   2        22        23   # BR(HH -> photon Z)
    -1.21663970E-04   2        23        23   # BR(HH -> Z Z)
    -1.82473826E-04   2       -24        24   # BR(HH -> W W)
    -2.64307271E-05   2        21        21   # BR(HH -> gluon gluon)
    -1.20398067E-09   2       -11        11   # BR(HH -> Electron electron)
     5.35871003E-05   2       -13        13   # BR(HH -> Muon muon)
    -1.53304534E-02   2       -15        15   # BR(HH -> Tau tau)
    -3.65248524E-12   2        -2         2   # BR(HH -> Up up)
    -5.05574197E-07   2        -4         4   # BR(HH -> Charm charm)
    -3.07295883E-02   2        -6         6   # BR(HH -> Top top)
    -1.41051912E-07   2        -1         1   # BR(HH -> Down down)
    -3.54239013E-05   2        -3         3   # BR(HH -> Strange strange)
    -8.96029275E-02   2        -5         5   # BR(HH -> Bottom bottom)
    -1.83188229E-01   2  -1000024   1000024   # BR(HH -> Chargino1 chargino1)
    -1.55580461E-01   2  -1000037   1000024   # BR(HH -> Chargino2 chargino1)
    -1.55580461E-01   2  -1000024   1000037   # BR(HH -> Chargino1 chargino2)
    -3.17903646E-02   2  -1000037   1000037   # BR(HH -> Chargino2 chargino2)
    -2.24324035E-02   2   1000022   1000022   # BR(HH -> neutralino1 neutralino1)
    -5.54797402E-02   2   1000022   1000023   # BR(HH -> neutralino1 neutralino2)
    -5.15773448E-02   2   1000022   1000025   # BR(HH -> neutralino1 neutralino3)
    -4.97202825E-07   2   1000022   1000035   # BR(HH -> neutralino1 neutralino4)
    -2.81842050E-02   2   1000023   1000023   # BR(HH -> neutralino2 neutralino2)
    -2.63563726E-02   2   1000023   1000025   # BR(HH -> neutralino2 neutralino3)
    -5.50082222E-03   2   1000023   1000035   # BR(HH -> neutralino2 neutralino4)
    -7.67925682E-03   2   1000025   1000025   # BR(HH -> neutralino3 neutralino3)
    -1.11805125E-01   2   1000025   1000035   # BR(HH -> neutralino3 neutralino4)
    -2.77994485E-02   2   1000035   1000035   # BR(HH -> neutralino4 neutralino4)
    -9.58581608E-04   2        25        25   # BR(HH -> h0 h0)
DECAY        36     1.15610953E+01   # Gamma(A0)
     1.25236770E-06   2        22        22   # BR(A0 -> photon photon)
     2.42625180E-06   2        22        23   # BR(A0 -> photon Z)
     7.62275108E-05   2        21        21   # BR(A0 -> gluon gluon)
     1.18575308E-09   2       -11        11   # BR(A0 -> Electron electron)
     5.27758096E-05   2       -13        13   # BR(A0 -> Muon muon)
     1.50984130E-02   2       -15        15   # BR(A0 -> Tau tau)
     3.41278035E-12   2        -2         2   # BR(A0 -> Up up)
     4.72352822E-07   2        -4         4   # BR(A0 -> Charm charm)
     3.38871965E-02   2        -6         6   # BR(A0 -> Top top)
     1.38917792E-07   2        -1         1   # BR(A0 -> Down down)
     3.48879412E-05   2        -3         3   # BR(A0 -> Strange strange)
     8.82592563E-02   2        -5         5   # BR(A0 -> Bottom bottom)
     2.61406953E-01   2  -1000024   1000024   # BR(A0 -> Chargino1 chargino1)
     8.34730716E-02   2  -1000037   1000024   # BR(A0 -> Chargino2 chargino1)
     8.34730716E-02   2  -1000024   1000037   # BR(A0 -> Chargino1 chargino2)
     9.76495047E-02   2  -1000037   1000037   # BR(A0 -> Chargino2 chargino2)
     2.86913981E-02   2   1000022   1000022   # BR(A0 -> neutralino1 neutralino1)
     7.86768223E-02   2   1000022   1000023   # BR(A0 -> neutralino1 neutralino2)
     2.76881431E-02   2   1000022   1000025   # BR(A0 -> neutralino1 neutralino3)
     5.91721267E-04   2   1000022   1000035   # BR(A0 -> neutralino1 neutralino4)
     4.37358043E-02   2   1000023   1000023   # BR(A0 -> neutralino2 neutralino2)
     1.12313757E-02   2   1000023   1000025   # BR(A0 -> neutralino2 neutralino3)
     7.25665477E-03   2   1000023   1000035   # BR(A0 -> neutralino2 neutralino4)
     6.37898491E-03   2   1000025   1000025   # BR(A0 -> neutralino3 neutralino3)
     5.92816000E-02   2   1000025   1000035   # BR(A0 -> neutralino3 neutralino4)
     7.28686067E-02   2   1000035   1000035   # BR(A0 -> neutralino4 neutralino4)
     1.83240049E-04   2        23        25   # BR(A0 -> Z h0)
     3.10283067E-36   2        25        25   # BR(A0 -> h0 h0)
DECAY        37     1.14424293E+01   # Gamma(Hp)
     1.30717574E-09   2       -11        12   # BR(Hp -> Electron nu_e)
     5.58858345E-05   2       -13        14   # BR(Hp -> Muon nu_mu)
     1.58081194E-02   2       -15        16   # BR(Hp -> Tau nu_tau)
     1.35650454E-07   2        -1         2   # BR(Hp -> Down up)
     1.56510829E-06   2        -3         2   # BR(Hp -> Strange up)
     9.56230963E-07   2        -5         2   # BR(Hp -> Bottom up)
     2.78965990E-08   2        -1         4   # BR(Hp -> Down charm)
     3.44171338E-05   2        -3         4   # BR(Hp -> Strange charm)
     1.33906763E-04   2        -5         4   # BR(Hp -> Bottom charm)
     2.21144592E-06   2        -1         6   # BR(Hp -> Down top)
     4.82628851E-05   2        -3         6   # BR(Hp -> Strange top)
     1.19994827E-01   2        -5         6   # BR(Hp -> Bottom top)
     9.47449702E-02   2   1000022   1000024   # BR(Hp -> neutralino1 chargino1)
     5.41827650E-03   2   1000022   1000037   # BR(Hp -> neutralino1 chargino2)
     2.05091204E-02   2   1000023   1000024   # BR(Hp -> neutralino2 chargino1)
     3.06187997E-01   2   1000023   1000037   # BR(Hp -> neutralino2 chargino2)
     1.18548764E-01   2   1000024   1000025   # BR(Hp -> chargino1 neutralino3)
     1.22271611E-01   2   1000025   1000037   # BR(Hp -> neutralino3 chargino2)
     1.93578774E-01   2   1000024   1000035   # BR(Hp -> chargino1 neutralino4)
     2.47120114E-03   2   1000035   1000037   # BR(Hp -> neutralino4 chargino2)
     1.88968572E-04   2        24        25   # BR(Hp -> W h0)
     7.45805172E-11   2        24        35   # BR(Hp -> W HH)
     8.62673044E-11   2        24        36   # BR(Hp -> W A0)
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

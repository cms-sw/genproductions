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
        26     6.00000000E+02   # MA0
        27     6.05362568E+02   # MHp
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
        25     1.25058671E+02   # Mh0
        35     5.99903887E+02   # MHH
        36     6.00000000E+02   # MA0
        37     6.05904393E+02   # MHp
   1000022     8.83213889E+01   # MNeu(1)
   1000023     1.52049632E+02   # MNeu(2)
   1000025    -2.10470056E+02   # MNeu(3)
   1000035     2.65570688E+02   # MNeu(4)
   1000024     1.48692309E+02   # MCha(1)
   1000037     2.66117031E+02   # MCha(2)
   1000021     1.50000000E+03   # MGl
BLOCK DMASS
         0     1.73200000E+02   # Q
        25     7.07675179E-01   # Delta Mh0
        35     6.35947830E-03   # Delta MHH
        36     0.00000000E+00   # Delta MA0
        37     2.91516715E-02   # Delta MHp
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
              -3.60586516E-02   # Alpha
BLOCK DALPHA
               4.93541505E-05   # Delta Alpha
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
     1   1     9.99999319E-01   # UH(1,1)
     1   2     1.16697866E-03   # UH(1,2)
     1   3     0.00000000E+00   # UH(1,3)
     2   1    -1.16697866E-03   # UH(2,1)
     2   2     9.99999319E-01   # UH(2,2)
     2   3     0.00000000E+00   # UH(2,3)
     3   1     0.00000000E+00   # UH(3,1)
     3   2     0.00000000E+00   # UH(3,2)
     3   3     1.00000000E+00   # UH(3,3)
DECAY        25     4.20797415E-03   # Gamma(h0)
     2.19286316E-03   2        22        22   # BR(h0 -> photon photon)
     1.40737072E-03   2        22        23   # BR(h0 -> photon Z)
     2.56858431E-02   2        23        23   # BR(h0 -> Z Z)
     2.11938223E-01   2       -24        24   # BR(h0 -> W W)
     6.70115672E-02   2        21        21   # BR(h0 -> gluon gluon)
     5.54312306E-09   2       -11        11   # BR(h0 -> Electron electron)
     2.46567947E-04   2       -13        13   # BR(h0 -> Muon muon)
     7.07404889E-02   2       -15        15   # BR(h0 -> Tau tau)
     1.91630664E-07   2        -2         2   # BR(h0 -> Up up)
     2.65428590E-02   2        -4         4   # BR(h0 -> Charm charm)
     9.02597579E-07   2        -1         1   # BR(h0 -> Down down)
     2.26671062E-04   2        -3         3   # BR(h0 -> Strange strange)
     5.94006446E-01   2        -5         5   # BR(h0 -> Bottom bottom)
DECAY        35     1.19247217E+01   # Gamma(HH)
     8.24151614E-07   2        22        22   # BR(HH -> photon photon)
     2.61280410E-06   2        22        23   # BR(HH -> photon Z)
     2.13480533E-05   2        23        23   # BR(HH -> Z Z)
     4.28322133E-05   2       -24        24   # BR(HH -> W W)
     7.00645507E-05   2        21        21   # BR(HH -> gluon gluon)
     7.35935090E-09   2       -11        11   # BR(HH -> Electron electron)
     3.27511589E-04   2       -13        13   # BR(HH -> Muon muon)
     9.15895052E-02   2       -15        15   # BR(HH -> Tau tau)
     2.58289146E-13   2        -2         2   # BR(HH -> Up up)
     3.57478763E-08   2        -4         4   # BR(HH -> Charm charm)
     1.51572481E-03   2        -6         6   # BR(HH -> Top top)
     8.41805867E-07   2        -1         1   # BR(HH -> Down down)
     2.11395924E-04   2        -3         3   # BR(HH -> Strange strange)
     5.00936726E-01   2        -5         5   # BR(HH -> Bottom bottom)
     1.03378809E-01   2  -1000024   1000024   # BR(HH -> Chargino1 chargino1)
     6.99907993E-02   2  -1000037   1000024   # BR(HH -> Chargino2 chargino1)
     6.99907993E-02   2  -1000024   1000037   # BR(HH -> Chargino1 chargino2)
     4.80654948E-03   2  -1000037   1000037   # BR(HH -> Chargino2 chargino2)
     1.24178656E-02   2   1000022   1000022   # BR(HH -> neutralino1 neutralino1)
     3.39276434E-02   2   1000022   1000023   # BR(HH -> neutralino1 neutralino2)
     2.32867286E-02   2   1000022   1000025   # BR(HH -> neutralino1 neutralino3)
     1.57100698E-05   2   1000022   1000035   # BR(HH -> neutralino1 neutralino4)
     1.80547505E-02   2   1000023   1000023   # BR(HH -> neutralino2 neutralino2)
     1.23105298E-02   2   1000023   1000025   # BR(HH -> neutralino2 neutralino3)
     2.24231687E-03   2   1000023   1000035   # BR(HH -> neutralino2 neutralino4)
     2.60292327E-03   2   1000025   1000025   # BR(HH -> neutralino3 neutralino3)
     4.79057045E-02   2   1000025   1000035   # BR(HH -> neutralino3 neutralino4)
     4.11313983E-03   2   1000035   1000035   # BR(HH -> neutralino4 neutralino4)
     2.36300523E-04   2        25        25   # BR(HH -> h0 h0)
DECAY        36     1.17526635E+01   # Gamma(A0)
    -1.75910085E-06   2        22        22   # BR(A0 -> photon photon)
    -3.97819809E-06   2        22        23   # BR(A0 -> photon Z)
    -1.01902968E-04   2        21        21   # BR(A0 -> gluon gluon)
    -7.36777811E-09   2       -11        11   # BR(A0 -> Electron electron)
     3.27886697E-04   2       -13        13   # BR(A0 -> Muon muon)
    -9.16950254E-02   2       -15        15   # BR(A0 -> Tau tau)
    -2.25062657E-13   2        -2         2   # BR(A0 -> Up up)
    -3.11679939E-08   2        -4         4   # BR(A0 -> Charm charm)
    -1.97828637E-03   2        -6         6   # BR(A0 -> Top top)
    -8.42728522E-07   2        -1         1   # BR(A0 -> Down down)
    -2.11627657E-04   2        -3         3   # BR(A0 -> Strange strange)
    -5.01554310E-01   2        -5         5   # BR(A0 -> Bottom bottom)
    -1.50824224E-01   2  -1000024   1000024   # BR(A0 -> Chargino1 chargino1)
    -3.35746006E-02   2  -1000037   1000024   # BR(A0 -> Chargino2 chargino1)
    -3.35746006E-02   2  -1000024   1000037   # BR(A0 -> Chargino1 chargino2)
    -2.91414445E-02   2  -1000037   1000037   # BR(A0 -> Chargino2 chargino2)
    -1.47279685E-02   2   1000022   1000022   # BR(A0 -> neutralino1 neutralino1)
    -4.45155971E-02   2   1000022   1000023   # BR(A0 -> neutralino1 neutralino2)
    -1.54749547E-02   2   1000022   1000025   # BR(A0 -> neutralino1 neutralino3)
    -1.16144898E-04   2   1000022   1000035   # BR(A0 -> neutralino1 neutralino4)
    -2.74655825E-02   2   1000023   1000023   # BR(A0 -> neutralino2 neutralino2)
    -6.29789445E-03   2   1000023   1000025   # BR(A0 -> neutralino2 neutralino3)
    -4.31293221E-03   2   1000023   1000035   # BR(A0 -> neutralino2 neutralino4)
    -4.49602626E-03   2   1000025   1000025   # BR(A0 -> neutralino3 neutralino3)
    -1.63919596E-02   2   1000025   1000035   # BR(A0 -> neutralino3 neutralino4)
    -2.31739009E-02   2   1000035   1000035   # BR(A0 -> neutralino4 neutralino4)
    -3.65117915E-05   2        23        25   # BR(A0 -> Z h0)
    -2.83824378E-39   2        25        25   # BR(A0 -> h0 h0)
DECAY        37     1.08703671E+01   # Gamma(Hp)
     8.30257113E-09   2       -11        12   # BR(Hp -> Electron nu_e)
     3.54960764E-04   2       -13        14   # BR(Hp -> Muon nu_mu)
     1.00404856E-01   2       -15        16   # BR(Hp -> Tau nu_tau)
     8.75817397E-07   2        -1         2   # BR(Hp -> Down up)
     1.00445915E-05   2        -3         2   # BR(Hp -> Strange up)
     5.66933718E-06   2        -5         2   # BR(Hp -> Bottom up)
     4.18091939E-08   2        -1         4   # BR(Hp -> Down charm)
     2.19199967E-04   2        -3         4   # BR(Hp -> Strange charm)
     7.93903221E-04   2        -5         4   # BR(Hp -> Bottom charm)
     1.65812344E-07   2        -1         6   # BR(Hp -> Down top)
     3.88429023E-06   2        -3         6   # BR(Hp -> Strange top)
     4.66650031E-01   2        -5         6   # BR(Hp -> Bottom top)
     6.34134281E-02   2   1000022   1000024   # BR(Hp -> neutralino1 chargino1)
     1.74640755E-03   2   1000022   1000037   # BR(Hp -> neutralino1 chargino2)
     1.05236408E-02   2   1000023   1000024   # BR(Hp -> neutralino2 chargino1)
     1.48533126E-01   2   1000023   1000037   # BR(Hp -> neutralino2 chargino2)
     6.39116506E-02   2   1000024   1000025   # BR(Hp -> chargino1 neutralino3)
     5.02028985E-02   2   1000025   1000037   # BR(Hp -> neutralino3 chargino2)
     9.24774929E-02   2   1000024   1000035   # BR(Hp -> chargino1 neutralino4)
     7.06151178E-04   2   1000035   1000037   # BR(Hp -> neutralino4 chargino2)
     4.15608930E-05   2        24        25   # BR(Hp -> W h0)
     9.62885003E-10   2        24        35   # BR(Hp -> W HH)
     8.88330287E-10   2        24        36   # BR(Hp -> W A0)
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

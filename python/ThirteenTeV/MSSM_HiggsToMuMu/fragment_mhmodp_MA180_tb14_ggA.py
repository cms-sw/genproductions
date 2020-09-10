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
         3     1.40000000E+01   # TB
BLOCK EXTPAR
         0     0.00000000E+00   # Q
         1     9.54716519E+01   # M1
         2     2.00000000E+02   # M2
         3     1.50000000E+03   # M3
        11     1.51428571E+03   # At
        12     1.51428571E+03   # Ab
        13     1.51428571E+03   # Atau
        23     2.00000000E+02   # MUE
        25     1.40000000E+01   # TB
        26     1.80000000E+02   # MA0
        27     1.97139135E+02   # MHp
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
   1000012     4.95867543E+02   # MSf(1,1,1)
   1000011     5.02277652E+02   # MSf(1,2,1)
   2000011     5.01829194E+02   # MSf(2,2,1)
   1000002     1.49903513E+03   # MSf(1,3,1)
   2000002     1.49959271E+03   # MSf(2,3,1)
   1000001     1.50116775E+03   # MSf(1,4,1)
   2000001     1.50020357E+03   # MSf(2,4,1)
   1000014     4.95867543E+02   # MSf(1,1,2)
   1000013     5.02423599E+02   # MSf(1,2,2)
   2000013     5.01683097E+02   # MSf(2,2,2)
   1000004     1.49903561E+03   # MSf(1,3,2)
   2000004     1.49959333E+03   # MSf(2,3,2)
   1000003     1.50117558E+03   # MSf(1,4,2)
   2000003     1.50019575E+03   # MSf(2,4,2)
   1000016     9.97940189E+02   # MSf(1,1,3)
   1000015     9.99882507E+02   # MSf(1,2,3)
   2000015     1.00217597E+03   # MSf(2,2,3)
   1000006     8.76435511E+02   # MSf(1,3,3)
   2000006     1.13478716E+03   # MSf(2,3,3)
   1000005     9.98375125E+02   # MSf(1,4,3)
   2000005     1.00369029E+03   # MSf(2,4,3)
        25     1.23671677E+02   # Mh0
        35     1.81223561E+02   # MHH
        36     1.80000000E+02   # MA0
        37     1.97580952E+02   # MHp
   1000022     8.71700213E+01   # MNeu(1)
   1000023     1.50504867E+02   # MNeu(2)
   1000025    -2.09664642E+02   # MNeu(3)
   1000035     2.67461405E+02   # MNeu(4)
   1000024     1.46059389E+02   # MCha(1)
   1000037     2.67571171E+02   # MCha(2)
   1000021     1.50000000E+03   # MGl
BLOCK DMASS
         0     1.73200000E+02   # Q
        25     7.36825233E-01   # Delta Mh0
        35     2.19038131E-02   # Delta MHH
        36     0.00000000E+00   # Delta MA0
        37     6.78979181E-02   # Delta MHp
BLOCK NMIX
     1   1     9.26359748E-01   # ZNeu(1,1)
     1   2    -1.27356386E-01   # ZNeu(1,2)
     1   3     3.18049606E-01   # ZNeu(1,3)
     1   4    -1.56468580E-01   # ZNeu(1,4)
     2   1    -3.34170358E-01   # ZNeu(2,1)
     2   2    -6.94276511E-01   # ZNeu(2,2)
     2   3     5.02142193E-01   # ZNeu(2,3)
     2   4    -3.92636619E-01   # ZNeu(2,4)
     3   1     9.39740580E-02   # ZNeu(3,1)
     3   2    -1.30790295E-01   # ZNeu(3,2)
     3   3    -6.78628020E-01   # ZNeu(3,3)
     3   4    -7.16607833E-01   # ZNeu(3,4)
     4   1    -1.46139200E-01   # ZNeu(4,1)
     4   2     6.96171225E-01   # ZNeu(4,2)
     4   3     4.31464572E-01   # ZNeu(4,3)
     4   4    -5.54821848E-01   # ZNeu(4,4)
BLOCK UMIX
     1   1    -6.10491734E-01   # UCha(1,1)
     1   2     7.92022628E-01   # UCha(1,2)
     2   1     7.92022628E-01   # UCha(2,1)
     2   2     6.10491734E-01   # UCha(2,2)
BLOCK VMIX
     1   1    -7.92022628E-01   # VCha(1,1)
     1   2     6.10491734E-01   # VCha(1,2)
     2   1     6.10491734E-01   # VCha(2,1)
     2   2     7.92022628E-01   # VCha(2,2)
BLOCK STAUMIX
     1   1     6.71540180E-01   # USf(1,1)
     1   2     7.40968142E-01   # USf(1,2)
     2   1     7.40968142E-01   # USf(2,1)
     2   2    -6.71540180E-01   # USf(2,2)
BLOCK STOPMIX
     1   1     7.08243538E-01   # USf(1,1)
     1   2    -7.05968194E-01   # USf(1,2)
     2   1     7.05968194E-01   # USf(2,1)
     2   2     7.08243538E-01   # USf(2,2)
BLOCK SBOTMIX
     1   1     6.03353498E-01   # USf(1,1)
     1   2     7.97473860E-01   # USf(1,2)
     2   1     7.97473860E-01   # USf(2,1)
     2   2    -6.03353498E-01   # USf(2,2)
BLOCK ALPHA
              -1.76634310E-01   # Alpha
BLOCK DALPHA
               4.54333521E-03   # Delta Alpha
BLOCK HMIX Q= -0.99900000E+03
         1     2.00000000E+02   # MUE
         2     1.40000000E+01   # TB
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
     3   3     1.51428571E+03   # Af(3,3)
BLOCK AU Q=  0.00000000E+00
     1   1     0.00000000E+00   # Af(1,1)
     2   2     0.00000000E+00   # Af(2,2)
     3   3     1.51428571E+03   # Af(3,3)
BLOCK AD Q=  0.00000000E+00
     1   1     0.00000000E+00   # Af(1,1)
     2   2     0.00000000E+00   # Af(2,2)
     3   3     1.51428571E+03   # Af(3,3)
BLOCK YE Q=  0.00000000E+00
     1   1     4.11949279E-05   # Yf(1,1)
     2   2     8.51780382E-03   # Yf(2,2)
     3   3     1.43257887E-01   # Yf(3,3)
BLOCK YU Q=  0.00000000E+00
     1   1     1.72749580E-05   # Yf(1,1)
     2   2     7.40519865E-03   # Yf(2,2)
     3   3     9.97340906E-01   # Yf(3,3)
BLOCK YD Q=  0.00000000E+00
     1   1     4.76870467E-04   # Yf(1,1)
     2   2     7.55022224E-03   # Yf(2,2)
     3   3     3.21042816E-01   # Yf(3,3)
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
     3   3     2.16933371E+02   # Tf(3,3)
BLOCK TU Q=  0.00000000E+00
     1   1     0.00000000E+00   # Tf(1,1)
     2   2     0.00000000E+00   # Tf(2,2)
     3   3     1.51025909E+03   # Tf(3,3)
BLOCK TD Q=  0.00000000E+00
     1   1     0.00000000E+00   # Tf(1,1)
     2   2     0.00000000E+00   # Tf(2,2)
     3   3     4.86150550E+02   # Tf(3,3)
BLOCK SELMIX
     1   1     9.99994952E-01   # UASf(1,1)
     1   4    -3.17744688E-03   # UASf(1,4)
     2   2     8.95991689E-01   # UASf(2,2)
     2   5    -4.44070819E-01   # UASf(2,5)
     3   3     6.71540180E-01   # UASf(3,3)
     3   6     7.40968142E-01   # UASf(3,6)
     4   1     3.17744688E-03   # UASf(4,1)
     4   4     9.99994952E-01   # UASf(4,4)
     5   2     4.44070819E-01   # UASf(5,2)
     5   5     8.95991689E-01   # UASf(5,5)
     6   3     7.40968142E-01   # UASf(6,3)
     6   6    -6.71540180E-01   # UASf(6,6)
BLOCK USQMIX
     1   1     1.00000000E+00   # UASf(1,1)
     1   4     2.56324573E-05   # UASf(1,4)
     2   2     9.99939654E-01   # UASf(2,2)
     2   5     1.09857908E-02   # UASf(2,5)
     3   3     7.08243538E-01   # UASf(3,3)
     3   6    -7.05968194E-01   # UASf(3,6)
     4   1    -2.56324573E-05   # UASf(4,1)
     4   4     1.00000000E+00   # UASf(4,4)
     5   2    -1.09857908E-02   # UASf(5,2)
     5   5     9.99939654E-01   # UASf(5,5)
     6   3     7.05968194E-01   # UASf(6,3)
     6   6     7.08243538E-01   # UASf(6,6)
BLOCK DSQMIX
     1   1     9.99983621E-01   # UASf(1,1)
     1   4    -5.72350775E-03   # UASf(1,4)
     2   2     9.95984067E-01   # UASf(2,2)
     2   5    -8.95306504E-02   # UASf(2,5)
     3   3     6.03353498E-01   # UASf(3,3)
     3   6     7.97473860E-01   # UASf(3,6)
     4   1     5.72350775E-03   # UASf(4,1)
     4   4     9.99983621E-01   # UASf(4,4)
     5   2     8.95306504E-02   # UASf(5,2)
     5   5     9.95984067E-01   # UASf(5,5)
     6   3     7.97473860E-01   # UASf(6,3)
     6   6    -6.03353498E-01   # UASf(6,6)
BLOCK CVHMIX
     1   1     9.98343388E-01   # UH(1,1)
     1   2     5.75367734E-02   # UH(1,2)
     1   3     0.00000000E+00   # UH(1,3)
     2   1    -5.75367734E-02   # UH(2,1)
     2   2     9.98343388E-01   # UH(2,2)
     2   3     0.00000000E+00   # UH(2,3)
     3   1     0.00000000E+00   # UH(3,1)
     3   2     0.00000000E+00   # UH(3,2)
     3   3     1.00000000E+00   # UH(3,3)
DECAY        25     1.51308021E-02   # Gamma(h0)
     6.00479778E-04   2        22        22   # BR(h0 -> photon photon)
     3.45715332E-04   2        22        23   # BR(h0 -> photon Z)
     5.92759027E-03   2        23        23   # BR(h0 -> Z Z)
     5.02864752E-02   2       -24        24   # BR(h0 -> W W)
     1.54897876E-02   2        21        21   # BR(h0 -> gluon gluon)
     7.90702500E-09   2       -11        11   # BR(h0 -> Electron electron)
     3.51717420E-04   2       -13        13   # BR(h0 -> Muon muon)
     1.00499805E-01   2       -15        15   # BR(h0 -> Tau tau)
     5.14818308E-08   2        -2         2   # BR(h0 -> Up up)
     7.13065407E-03   2        -4         4   # BR(h0 -> Charm charm)
     1.26413590E-06   2        -1         1   # BR(h0 -> Down down)
     3.17461800E-04   2        -3         3   # BR(h0 -> Strange strange)
     8.19048990E-01   2        -5         5   # BR(h0 -> Bottom bottom)
DECAY        35     5.92700939E-01   # Gamma(HH)
     8.31548154E-07   2        22        22   # BR(HH -> photon photon)
     2.22626703E-06   2        22        23   # BR(HH -> photon Z)
     1.00575567E-03   2        23        23   # BR(HH -> Z Z)
     1.15131805E-02   2       -24        24   # BR(HH -> W W)
     8.48529418E-04   2        21        21   # BR(HH -> gluon gluon)
     9.34076893E-09   2       -11        11   # BR(HH -> Electron electron)
     4.15540582E-04   2       -13        13   # BR(HH -> Muon muon)
     1.18186353E-01   2       -15        15   # BR(HH -> Tau tau)
     5.42748497E-11   2        -2         2   # BR(HH -> Up up)
     7.51883660E-06   2        -4         4   # BR(HH -> Charm charm)
     1.36745973E-06   2        -1         1   # BR(HH -> Down down)
     3.43407690E-04   2        -3         3   # BR(HH -> Strange strange)
     8.66060913E-01   2        -5         5   # BR(HH -> Bottom bottom)
     1.61436723E-03   2   1000022   1000022   # BR(HH -> neutralino1 neutralino1)
     4.97916128E-12   2        23        36   # BR(HH -> Z A0)
DECAY        36     6.08611201E-01   # Gamma(A0)
    -5.05485706E-07   2        22        22   # BR(A0 -> photon photon)
    -8.48823632E-07   2        22        23   # BR(A0 -> photon Z)
    -5.11152458E-04   2        21        21   # BR(A0 -> gluon gluon)
    -9.21641273E-09   2       -11        11   # BR(A0 -> Electron electron)
     4.10008116E-04   2       -13        13   # BR(A0 -> Muon muon)
    -1.16652275E-01   2       -15        15   # BR(A0 -> Tau tau)
    -7.92606511E-12   2        -2         2   # BR(A0 -> Up up)
    -1.10958512E-06   2        -4         4   # BR(A0 -> Charm charm)
    -1.35105767E-06   2        -1         1   # BR(A0 -> Down down)
    -3.39288848E-04   2        -3         3   # BR(A0 -> Strange strange)
    -8.56828754E-01   2        -5         5   # BR(A0 -> Bottom bottom)
    -2.52460471E-02   2   1000022   1000022   # BR(A0 -> neutralino1 neutralino1)
    -8.64993520E-06   2        23        25   # BR(A0 -> Z h0)
DECAY        37     1.08272311E-01   # Gamma(Hp)
     5.91962154E-08   2       -11        12   # BR(Hp -> Electron nu_e)
     2.53082121E-03   2       -13        14   # BR(Hp -> Muon nu_mu)
     7.15769624E-01   2       -15        16   # BR(Hp -> Tau nu_tau)
     7.67906066E-06   2        -1         2   # BR(Hp -> Down up)
     8.63725241E-05   2        -3         2   # BR(Hp -> Strange up)
     5.18734420E-05   2        -5         2   # BR(Hp -> Bottom up)
     6.57776232E-07   2        -1         4   # BR(Hp -> Down charm)
     1.92853108E-03   2        -3         4   # BR(Hp -> Strange charm)
     7.26353006E-03   2        -5         4   # BR(Hp -> Bottom charm)
     1.85087291E-06   2        -1         6   # BR(Hp -> Down top)
     4.04843227E-05   2        -3         6   # BR(Hp -> Strange top)
     2.71921963E-01   2        -5         6   # BR(Hp -> Bottom top)
     3.64500415E-04   2        24        25   # BR(Hp -> W h0)
     1.31279079E-05   2        24        35   # BR(Hp -> W HH)
     1.89251890E-05   2        24        36   # BR(Hp -> W A0)
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

COM_ENERGY = 13000.0 # GeV
CROSS_SECTION = 1 # pb
PROCESS = 'HiggsBSM:gg2A3bbbar = on'
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
         3     1.10000000E+01   # TB
BLOCK EXTPAR
         0     0.00000000E+00   # Q
         1     9.54716519E+01   # M1
         2     2.00000000E+02   # M2
         3     1.50000000E+03   # M3
        11     1.51818182E+03   # At
        12     1.51818182E+03   # Ab
        13     1.51818182E+03   # Atau
        23     2.00000000E+02   # MUE
        25     1.10000000E+01   # TB
        26     1.30000000E+02   # MA0
        27     1.52852342E+02   # MHp
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
   1000012     4.95893706E+02   # MSf(1,1,1)
   1000011     5.02263322E+02   # MSf(1,2,1)
   2000011     5.01817683E+02   # MSf(2,2,1)
   1000002     1.49904121E+03   # MSf(1,3,1)
   2000002     1.49959528E+03   # MSf(2,3,1)
   1000001     1.50116038E+03   # MSf(1,4,1)
   2000001     1.50020230E+03   # MSf(2,4,1)
   1000014     4.95893706E+02   # MSf(1,1,2)
   1000013     5.02361772E+02   # MSf(1,2,2)
   2000013     5.01719149E+02   # MSf(2,2,2)
   1000004     1.49904166E+03   # MSf(1,3,2)
   2000004     1.49959594E+03   # MSf(2,3,2)
   1000003     1.50116529E+03   # MSf(1,4,2)
   2000003     1.50019740E+03   # MSf(2,4,2)
   1000016     9.97953189E+02   # MSf(1,1,3)
   1000015     1.00040781E+03   # MSf(1,2,3)
   2000015     1.00163864E+03   # MSf(2,2,3)
   1000006     8.76442922E+02   # MSf(1,3,3)
   2000006     1.13479287E+03   # MSf(2,3,3)
   1000005     9.99483078E+02   # MSf(1,4,3)
   2000005     1.00257434E+03   # MSf(2,4,3)
        25     1.17711132E+02   # Mh0
        35     1.37076818E+02   # MHH
        36     1.30000000E+02   # MA0
        37     1.52974884E+02   # MHp
   1000022     8.65735115E+01   # MNeu(1)
   1000023     1.49760802E+02   # MNeu(2)
   1000025    -2.09254812E+02   # MNeu(3)
   1000035     2.68392150E+02   # MNeu(4)
   1000024     1.44753165E+02   # MCha(1)
   1000037     2.68280074E+02   # MCha(2)
   1000021     1.50000000E+03   # MGl
BLOCK DMASS
         0     1.73200000E+02   # Q
        25     6.03963711E-01   # Delta Mh0
        35     1.24262329E-01   # Delta MHH
        36     0.00000000E+00   # Delta MA0
        37     1.25807662E-01   # Delta MHp
BLOCK NMIX
     1   1     9.22807713E-01   # ZNeu(1,1)
     1   2    -1.35268955E-01   # ZNeu(1,2)
     1   3     3.22139313E-01   # ZNeu(1,3)
     1   4    -1.62340685E-01   # ZNeu(1,4)
     2   1    -3.43946052E-01   # ZNeu(2,1)
     2   2    -6.94484935E-01   # ZNeu(2,2)
     2   3     4.96817247E-01   # ZNeu(2,3)
     2   4    -3.90594945E-01   # ZNeu(2,4)
     3   1     9.21330183E-02   # ZNeu(3,1)
     3   2    -1.28184003E-01   # ZNeu(3,2)
     3   3    -6.79042939E-01   # ZNeu(3,3)
     3   4    -7.16924721E-01   # ZNeu(3,4)
     4   1    -1.47100462E-01   # ZNeu(4,1)
     4   2     6.94954564E-01   # ZNeu(4,2)
     4   3     4.33934987E-01   # ZNeu(4,3)
     4   4    -5.54166072E-01   # ZNeu(4,4)
BLOCK UMIX
     1   1    -6.12687875E-01   # UCha(1,1)
     1   2     7.90324976E-01   # UCha(1,2)
     2   1     7.90324976E-01   # UCha(2,1)
     2   2     6.12687875E-01   # UCha(2,2)
BLOCK VMIX
     1   1    -7.90324976E-01   # VCha(1,1)
     1   2     6.12687875E-01   # VCha(1,2)
     2   1     6.12687875E-01   # VCha(2,1)
     2   2     7.90324976E-01   # VCha(2,2)
BLOCK STAUMIX
     1   1     6.39694531E-01   # USf(1,1)
     1   2     7.68629239E-01   # USf(1,2)
     2   1     7.68629239E-01   # USf(2,1)
     2   2    -6.39694531E-01   # USf(2,2)
BLOCK STOPMIX
     1   1     7.08236377E-01   # USf(1,1)
     1   2    -7.05975378E-01   # USf(1,2)
     2   1     7.05975378E-01   # USf(2,1)
     2   2     7.08236377E-01   # USf(2,2)
BLOCK SBOTMIX
     1   1     5.17391046E-01   # USf(1,1)
     1   2     8.55749090E-01   # USf(1,2)
     2   1     8.55749090E-01   # USf(2,1)
     2   2    -5.17391046E-01   # USf(2,2)
BLOCK ALPHA
              -6.74657141E-01   # Alpha
BLOCK DALPHA
               4.94975811E-02   # Delta Alpha
BLOCK HMIX Q= -0.99900000E+03
         1     2.00000000E+02   # MUE
         2     1.10000000E+01   # TB
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
     3   3     1.51818182E+03   # Af(3,3)
BLOCK AU Q=  0.00000000E+00
     1   1     0.00000000E+00   # Af(1,1)
     2   2     0.00000000E+00   # Af(2,2)
     3   3     1.51818182E+03   # Af(3,3)
BLOCK AD Q=  0.00000000E+00
     1   1     0.00000000E+00   # Af(1,1)
     2   2     0.00000000E+00   # Af(2,2)
     3   3     1.51818182E+03   # Af(3,3)
BLOCK YE Q=  0.00000000E+00
     1   1     3.24183233E-05   # Yf(1,1)
     2   2     6.70308051E-03   # Yf(2,2)
     3   3     1.12736706E-01   # Yf(3,3)
BLOCK YU Q=  0.00000000E+00
     1   1     1.73021133E-05   # Yf(1,1)
     2   2     7.41683924E-03   # Yf(2,2)
     3   3     9.98908675E-01   # Yf(3,3)
BLOCK YD Q=  0.00000000E+00
     1   1     3.76411664E-04   # Yf(1,1)
     2   2     5.95970976E-03   # Yf(2,2)
     3   3     2.54964717E-01   # Yf(3,3)
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
     3   3     1.71154818E+02   # Tf(3,3)
BLOCK TU Q=  0.00000000E+00
     1   1     0.00000000E+00   # Tf(1,1)
     2   2     0.00000000E+00   # Tf(2,2)
     3   3     1.51652499E+03   # Tf(3,3)
BLOCK TD Q=  0.00000000E+00
     1   1     0.00000000E+00   # Tf(1,1)
     2   2     0.00000000E+00   # Tf(2,2)
     3   3     3.87082797E+02   # Tf(3,3)
BLOCK SELMIX
     1   1     9.99996844E-01   # UASf(1,1)
     1   4    -2.51242082E-03   # UASf(1,4)
     2   2     9.20179163E-01   # UASf(2,2)
     2   5    -3.91497521E-01   # UASf(2,5)
     3   3     6.39694531E-01   # UASf(3,3)
     3   6     7.68629239E-01   # UASf(3,6)
     4   1     2.51242082E-03   # UASf(4,1)
     4   4     9.99996844E-01   # UASf(4,4)
     5   2     3.91497521E-01   # UASf(5,2)
     5   5     9.20179163E-01   # UASf(5,5)
     6   3     7.68629239E-01   # UASf(6,3)
     6   6    -6.39694531E-01   # UASf(6,6)
BLOCK USQMIX
     1   1     9.99999999E-01   # UASf(1,1)
     1   4     3.28301270E-05   # UASf(1,4)
     2   2     9.99901027E-01   # UASf(2,2)
     2   5     1.40690024E-02   # UASf(2,5)
     3   3     7.08236377E-01   # UASf(3,3)
     3   6    -7.05975378E-01   # UASf(3,6)
     4   1    -3.28301270E-05   # UASf(4,1)
     4   4     9.99999999E-01   # UASf(4,4)
     5   2    -1.40690024E-02   # UASf(5,2)
     5   5     9.99901027E-01   # UASf(5,5)
     6   3     7.05975378E-01   # UASf(6,3)
     6   6     7.08236377E-01   # UASf(6,6)
BLOCK DSQMIX
     1   1     9.99989697E-01   # UASf(1,1)
     1   4    -4.53939165E-03   # UASf(1,4)
     2   2     9.97453158E-01   # UASf(2,2)
     2   5    -7.13245911E-02   # UASf(2,5)
     3   3     5.17391046E-01   # UASf(3,3)
     3   6     8.55749090E-01   # UASf(3,6)
     4   1     4.53939165E-03   # UASf(4,1)
     4   4     9.99989697E-01   # UASf(4,4)
     5   2     7.13245911E-02   # UASf(5,2)
     5   5     9.97453158E-01   # UASf(5,5)
     6   3     8.55749090E-01   # UASf(6,3)
     6   6    -5.17391046E-01   # UASf(6,6)
BLOCK CVHMIX
     1   1     9.09931257E-01   # UH(1,1)
     1   2     4.14759097E-01   # UH(1,2)
     1   3     0.00000000E+00   # UH(1,3)
     2   1    -4.14759097E-01   # UH(2,1)
     2   2     9.09931257E-01   # UH(2,2)
     2   3     0.00000000E+00   # UH(2,3)
     3   1     0.00000000E+00   # UH(3,1)
     3   2     0.00000000E+00   # UH(3,2)
     3   3     1.00000000E+00   # UH(3,3)
DECAY        25     1.06085791E-01   # Gamma(h0)
     5.93581626E-05   2        22        22   # BR(h0 -> photon photon)
     1.58731524E-05   2        22        23   # BR(h0 -> photon Z)
     2.65093598E-04   2        23        23   # BR(h0 -> Z Z)
     2.52915999E-03   2       -24        24   # BR(h0 -> W W)
     1.07892776E-03   2        21        21   # BR(h0 -> gluon gluon)
     8.55673310E-09   2       -11        11   # BR(h0 -> Electron electron)
     3.80611816E-04   2       -13        13   # BR(h0 -> Muon muon)
     1.08640918E-01   2       -15        15   # BR(h0 -> Tau tau)
     4.43024836E-09   2        -2         2   # BR(h0 -> Up up)
     6.13603815E-04   2        -4         4   # BR(h0 -> Charm charm)
     1.37411806E-06   2        -1         1   # BR(h0 -> Down down)
     3.45079687E-04   2        -3         3   # BR(h0 -> Strange strange)
     8.86069987E-01   2        -5         5   # BR(h0 -> Bottom bottom)
DECAY        35     1.82391477E-01   # Gamma(HH)
     1.35943446E-05   2        22        22   # BR(HH -> photon photon)
     3.19877264E-05   2        22        23   # BR(HH -> photon Z)
     6.99111237E-04   2        23        23   # BR(HH -> Z Z)
     4.92047320E-03   2       -24        24   # BR(HH -> W W)
     2.80970062E-03   2        21        21   # BR(HH -> gluon gluon)
     8.85358900E-09   2       -11        11   # BR(HH -> Electron electron)
     3.93834434E-04   2       -13        13   # BR(HH -> Muon muon)
     1.12247365E-01   2       -15        15   # BR(HH -> Tau tau)
     1.88125650E-09   2        -2         2   # BR(HH -> Up up)
     2.60591571E-04   2        -4         4   # BR(HH -> Charm charm)
     1.37333114E-06   2        -1         1   # BR(HH -> Down down)
     3.44881516E-04   2        -3         3   # BR(HH -> Strange strange)
     8.78277007E-01   2        -5         5   # BR(HH -> Bottom bottom)
     6.90463371E-08   2        23        36   # BR(HH -> Z A0)
DECAY        36     2.83940471E-01   # Gamma(A0)
    -3.99395058E-07   2        22        22   # BR(A0 -> photon photon)
    -1.77716768E-07   2        22        23   # BR(A0 -> photon Z)
    -8.41552959E-04   2        21        21   # BR(A0 -> gluon gluon)
    -8.79181030E-09   2       -11        11   # BR(A0 -> Electron electron)
     3.91081097E-04   2       -13        13   # BR(A0 -> Muon muon)
    -1.11594340E-01   2       -15        15   # BR(A0 -> Tau tau)
    -2.25072981E-11   2        -2         2   # BR(A0 -> Up up)
    -3.18589245E-06   2        -4         4   # BR(A0 -> Charm charm)
    -1.38005781E-06   2        -1         1   # BR(A0 -> Down down)
    -3.46571256E-04   2        -3         3   # BR(A0 -> Strange strange)
    -8.86821015E-01   2        -5         5   # BR(A0 -> Bottom bottom)
    -2.87907195E-07   2        23        25   # BR(A0 -> Z h0)
DECAY        37     3.77171566E-02   # Gamma(Hp)
     8.12225766E-08   2       -11        12   # BR(Hp -> Electron nu_e)
     3.47251492E-03   2       -13        14   # BR(Hp -> Muon nu_mu)
     9.81994669E-01   2       -15        16   # BR(Hp -> Tau nu_tau)
     1.10868568E-05   2        -1         2   # BR(Hp -> Down up)
     1.23952944E-04   2        -3         2   # BR(Hp -> Strange up)
     7.53134394E-05   2        -5         2   # BR(Hp -> Bottom up)
     1.66770936E-06   2        -1         4   # BR(Hp -> Down charm)
     2.80043434E-03   2        -3         4   # BR(Hp -> Strange charm)
     1.05451074E-02   2        -5         4   # BR(Hp -> Bottom charm)
     2.98624036E-04   2        -5         6   # BR(Hp -> Bottom top)
     4.63072215E-04   2        24        25   # BR(Hp -> W h0)
     2.22082923E-05   2        24        35   # BR(Hp -> W HH)
     1.91267541E-04   2        24        36   # BR(Hp -> W A0)
DECAY         6     1.37460522E+00   # Gamma(top)
     9.97577567E-01   2         5        24   # BR(top -> bottom W)
     2.42243323E-03   2         5        37   # BR(top -> bottom Hp)
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

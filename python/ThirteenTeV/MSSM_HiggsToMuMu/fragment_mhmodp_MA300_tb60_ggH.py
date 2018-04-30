COM_ENERGY = 13000.0 # GeV
CROSS_SECTION = 1 # pb
PROCESS = 'HiggsBSM:gg2H2 = on'
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
         3     6.00000000E+01   # TB
BLOCK EXTPAR
         0     0.00000000E+00   # Q
         1     9.54716519E+01   # M1
         2     2.00000000E+02   # M2
         3     1.50000000E+03   # M3
        11     1.50333333E+03   # At
        12     1.50333333E+03   # Ab
        13     1.50333333E+03   # Atau
        23     2.00000000E+02   # MUE
        25     6.00000000E+01   # TB
        26     3.00000000E+02   # MA0
        27     3.10586282E+02   # MHp
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
   1000012     4.95827309E+02   # MSf(1,1,1)
   1000011     5.02299762E+02   # MSf(1,2,1)
   2000011     5.01846818E+02   # MSf(2,2,1)
   1000002     1.49902577E+03   # MSf(1,3,1)
   2000002     1.49958876E+03   # MSf(2,3,1)
   1000001     1.50117956E+03   # MSf(1,4,1)
   2000001     1.50020505E+03   # MSf(2,4,1)
   1000014     4.95827309E+02   # MSf(1,1,2)
   1000013     5.03354517E+02   # MSf(1,2,2)
   2000013     5.00788910E+02   # MSf(2,2,2)
   1000004     1.49902632E+03   # MSf(1,3,2)
   2000004     1.49958932E+03   # MSf(2,3,2)
   1000003     1.50129637E+03   # MSf(1,4,2)
   2000003     1.50008816E+03   # MSf(2,4,2)
   1000016     9.97920197E+02   # MSf(1,1,3)
   1000015     9.91678638E+02   # MSf(1,2,3)
   2000015     1.01031434E+03   # MSf(2,2,3)
   1000006     8.76424115E+02   # MSf(1,3,3)
   2000006     1.13477838E+03   # MSf(2,3,3)
   1000005     9.82542174E+02   # MSf(1,4,3)
   2000005     1.01921076E+03   # MSf(2,4,3)
        25     1.25463884E+02   # Mh0
        35     3.00082518E+02   # MHH
        36     3.00000000E+02   # MA0
        37     3.15637301E+02   # MHp
   1000022     8.88163969E+01   # MNeu(1)
   1000023     1.52762413E+02   # MNeu(2)
   1000025    -2.10822655E+02   # MNeu(3)
   1000035     2.64715497E+02   # MNeu(4)
   1000024     1.49874270E+02   # MCha(1)
   1000037     2.65453160E+02   # MCha(2)
   1000021     1.50000000E+03   # MGl
BLOCK DMASS
         0     1.73200000E+02   # Q
        25     8.04623915E-01   # Delta Mh0
        35     5.36489035E-02   # Delta MHH
        36     0.00000000E+00   # Delta MA0
        37     6.07301581E-01   # Delta MHp
BLOCK NMIX
     1   1     9.36057036E-01   # ZNeu(1,1)
     1   2    -1.04695599E-01   # ZNeu(1,2)
     1   3     3.05476791E-01   # ZNeu(1,3)
     1   4    -1.39713945E-01   # ZNeu(1,4)
     2   1    -3.05772271E-01   # ZNeu(2,1)
     2   2    -6.92942408E-01   # ZNeu(2,2)
     2   3     5.17178606E-01   # ZNeu(2,3)
     2   4    -3.98572988E-01   # ZNeu(2,4)
     3   1     9.89211014E-02   # ZNeu(3,1)
     3   2    -1.37808383E-01   # ZNeu(3,2)
     3   3    -6.77626927E-01   # ZNeu(3,3)
     3   4    -7.15573346E-01   # ZNeu(3,4)
     4   1    -1.43231138E-01   # ZNeu(4,1)
     4   2     6.99913209E-01   # ZNeu(4,2)
     4   3     4.24301741E-01   # ZNeu(4,3)
     4   4    -5.56394081E-01   # ZNeu(4,4)
BLOCK UMIX
     1   1    -6.04499906E-01   # UCha(1,1)
     1   2     7.96605212E-01   # UCha(1,2)
     2   1     7.96605212E-01   # UCha(2,1)
     2   2     6.04499906E-01   # UCha(2,2)
BLOCK VMIX
     1   1    -7.96605212E-01   # VCha(1,1)
     1   2     6.04499906E-01   # VCha(1,2)
     2   1     6.04499906E-01   # VCha(2,1)
     2   2     7.96605212E-01   # VCha(2,2)
BLOCK STAUMIX
     1   1     7.02785035E-01   # USf(1,1)
     1   2     7.11402273E-01   # USf(1,2)
     2   1     7.11402273E-01   # USf(2,1)
     2   2    -7.02785035E-01   # USf(2,2)
BLOCK STOPMIX
     1   1     7.08254550E-01   # USf(1,1)
     1   2    -7.05957146E-01   # USf(1,2)
     2   1     7.05957146E-01   # USf(2,1)
     2   2     7.08254550E-01   # USf(2,2)
BLOCK SBOTMIX
     1   1     6.92890711E-01   # USf(1,1)
     1   2     7.21042622E-01   # USf(1,2)
     2   1     7.21042622E-01   # USf(2,1)
     2   2    -6.92890711E-01   # USf(2,2)
BLOCK ALPHA
              -2.40245228E-02   # Alpha
BLOCK DALPHA
               9.81577428E-05   # Delta Alpha
BLOCK HMIX Q= -0.99900000E+03
         1     2.00000000E+02   # MUE
         2     6.00000000E+01   # TB
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
     3   3     1.50333333E+03   # Af(3,3)
BLOCK AU Q=  0.00000000E+00
     1   1     0.00000000E+00   # Af(1,1)
     2   2     0.00000000E+00   # Af(2,2)
     3   3     1.50333333E+03   # Af(3,3)
BLOCK AD Q=  0.00000000E+00
     1   1     0.00000000E+00   # Af(1,1)
     2   2     0.00000000E+00   # Af(2,2)
     3   3     1.50333333E+03   # Af(3,3)
BLOCK YE Q=  0.00000000E+00
     1   1     1.76125482E-04   # Yf(1,1)
     2   2     3.64171606E-02   # Yf(2,2)
     3   3     6.12487158E-01   # Yf(3,3)
BLOCK YU Q=  0.00000000E+00
     1   1     1.72334501E-05   # Yf(1,1)
     2   2     7.38740563E-03   # Yf(2,2)
     3   3     9.94944522E-01   # Yf(3,3)
BLOCK YD Q=  0.00000000E+00
     1   1     1.94843658E-03   # Yf(1,1)
     2   2     3.08464352E-02   # Yf(2,2)
     3   3     1.20415141E+00   # Yf(3,3)
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
     3   3     9.20772361E+02   # Tf(3,3)
BLOCK TU Q=  0.00000000E+00
     1   1     0.00000000E+00   # Tf(1,1)
     2   2     0.00000000E+00   # Tf(2,2)
     3   3     1.49573326E+03   # Tf(3,3)
BLOCK TD Q=  0.00000000E+00
     1   1     0.00000000E+00   # Tf(1,1)
     2   2     0.00000000E+00   # Tf(2,2)
     3   3     1.81024095E+03   # Tf(3,3)
BLOCK SELMIX
     1   1     9.99909095E-01   # UASf(1,1)
     1   4    -1.34833961E-02   # UASf(1,4)
     2   2     7.66968342E-01   # UASf(2,2)
     2   5    -6.41684940E-01   # UASf(2,5)
     3   3     7.02785035E-01   # UASf(3,3)
     3   6     7.11402273E-01   # UASf(3,6)
     4   1     1.34833961E-02   # UASf(4,1)
     4   4     9.99909095E-01   # UASf(4,4)
     5   2     6.41684940E-01   # UASf(5,2)
     5   5     7.66968342E-01   # UASf(5,5)
     6   3     7.11402273E-01   # UASf(6,3)
     6   6    -7.02785035E-01   # UASf(6,6)
BLOCK USQMIX
     1   1     1.00000000E+00   # UASf(1,1)
     1   4     5.92347675E-06   # UASf(1,4)
     2   2     9.99996776E-01   # UASf(2,2)
     2   5     2.53917248E-03   # UASf(2,5)
     3   3     7.08254550E-01   # UASf(3,3)
     3   6    -7.05957146E-01   # UASf(3,6)
     4   1    -5.92347675E-06   # UASf(4,1)
     4   4     1.00000000E+00   # UASf(4,4)
     5   2    -2.53917248E-03   # UASf(5,2)
     5   5     9.99996776E-01   # UASf(5,5)
     6   3     7.05957146E-01   # UASf(6,3)
     6   6     7.08254550E-01   # UASf(6,6)
BLOCK DSQMIX
     1   1     9.99730862E-01   # UASf(1,1)
     1   4    -2.31992028E-02   # UASf(1,4)
     2   2     9.50186461E-01   # UASf(2,2)
     2   5    -3.11682034E-01   # UASf(2,5)
     3   3     6.92890711E-01   # UASf(3,3)
     3   6     7.21042622E-01   # UASf(3,6)
     4   1     2.31992028E-02   # UASf(4,1)
     4   4     9.99730862E-01   # UASf(4,4)
     5   2     3.11682034E-01   # UASf(5,2)
     5   5     9.50186461E-01   # UASf(5,5)
     6   3     7.21042622E-01   # UASf(6,3)
     6   6    -6.92890711E-01   # UASf(6,6)
BLOCK CVHMIX
     1   1     9.99992120E-01   # UH(1,1)
     1   2     3.96983720E-03   # UH(1,2)
     1   3     0.00000000E+00   # UH(1,3)
     2   1    -3.96983720E-03   # UH(2,1)
     2   2     9.99992120E-01   # UH(2,2)
     2   3     0.00000000E+00   # UH(2,3)
     3   1     0.00000000E+00   # UH(3,1)
     3   2     0.00000000E+00   # UH(3,2)
     3   3     1.00000000E+00   # UH(3,3)
DECAY        25     5.95543545E-03   # Gamma(h0)
     1.56877689E-03   2        22        22   # BR(h0 -> photon photon)
     1.02772807E-03   2        22        23   # BR(h0 -> photon Z)
     1.90867559E-02   2        23        23   # BR(h0 -> Z Z)
     1.56235882E-01   2       -24        24   # BR(h0 -> W W)
     4.64235003E-02   2        21        21   # BR(h0 -> gluon gluon)
     7.00386316E-09   2       -11        11   # BR(h0 -> Electron electron)
     3.11544075E-04   2       -13        13   # BR(h0 -> Muon muon)
     8.79028126E-02   2       -15        15   # BR(h0 -> Tau tau)
     1.35750117E-07   2        -2         2   # BR(h0 -> Up up)
     1.88029859E-02   2        -4         4   # BR(h0 -> Charm charm)
     1.07369839E-06   2        -1         1   # BR(h0 -> Down down)
     2.69622991E-04   2        -3         3   # BR(h0 -> Strange strange)
     6.68369175E-01   2        -5         5   # BR(h0 -> Bottom bottom)
DECAY        35     1.36304773E+01   # Gamma(HH)
    -5.79822118E-07   2        22        22   # BR(HH -> photon photon)
    -5.36897763E-07   2        22        23   # BR(HH -> photon Z)
    -1.03303891E-05   2        23        23   # BR(HH -> Z Z)
    -2.31407982E-05   2       -24        24   # BR(HH -> W W)
    -3.12772507E-04   2        21        21   # BR(HH -> gluon gluon)
    -1.35502363E-08   2       -11        11   # BR(HH -> Electron electron)
     6.02898277E-04   2       -13        13   # BR(HH -> Muon muon)
    -1.64065183E-01   2       -15        15   # BR(HH -> Tau tau)
    -4.67491907E-14   2        -2         2   # BR(HH -> Up up)
    -6.46626682E-09   2        -4         4   # BR(HH -> Charm charm)
    -1.53879879E-06   2        -1         1   # BR(HH -> Down down)
    -3.86374605E-04   2        -3         3   # BR(HH -> Strange strange)
    -8.26795679E-01   2        -5         5   # BR(HH -> Bottom bottom)
    -7.40862986E-06   2  -1000024   1000024   # BR(HH -> Chargino1 chargino1)
    -3.16040339E-03   2   1000022   1000022   # BR(HH -> neutralino1 neutralino1)
    -4.01570822E-03   2   1000022   1000023   # BR(HH -> neutralino1 neutralino2)
    -4.92796890E-04   2   1000022   1000025   # BR(HH -> neutralino1 neutralino3)
    -1.24629034E-04   2        25        25   # BR(HH -> h0 h0)
DECAY        36     1.38068535E+01   # Gamma(A0)
     4.67297008E-07   2        22        22   # BR(A0 -> photon photon)
     1.89952210E-06   2        22        23   # BR(A0 -> photon Z)
     3.07135320E-04   2        21        21   # BR(A0 -> gluon gluon)
     1.33946440E-08   2       -11        11   # BR(A0 -> Electron electron)
     5.95975698E-04   2       -13        13   # BR(A0 -> Muon muon)
     1.62133621E-01   2       -15        15   # BR(A0 -> Tau tau)
     1.43909203E-14   2        -2         2   # BR(A0 -> Up up)
     2.04874833E-09   2        -4         4   # BR(A0 -> Charm charm)
     1.52044587E-06   2        -1         1   # BR(A0 -> Down down)
     3.81766457E-04   2        -3         3   # BR(A0 -> Strange strange)
     8.17242983E-01   2        -5         5   # BR(A0 -> Bottom bottom)
     2.99063425E-03   2  -1000024   1000024   # BR(A0 -> Chargino1 chargino1)
     4.98487240E-03   2   1000022   1000022   # BR(A0 -> neutralino1 neutralino1)
     1.13466929E-02   2   1000022   1000023   # BR(A0 -> neutralino1 neutralino2)
     1.13775320E-06   2   1000022   1000025   # BR(A0 -> neutralino1 neutralino3)
     1.12783876E-05   2        23        25   # BR(A0 -> Z h0)
     2.66697685E-36   2        25        25   # BR(A0 -> h0 h0)
DECAY        37     8.19381024E+00   # Gamma(Hp)
     2.29517141E-08   2       -11        12   # BR(Hp -> Electron nu_e)
     9.81257072E-04   2       -13        14   # BR(Hp -> Muon nu_mu)
     2.77547410E-01   2       -15        16   # BR(Hp -> Tau nu_tau)
     2.51948146E-06   2        -1         2   # BR(Hp -> Down up)
     2.85820653E-05   2        -3         2   # BR(Hp -> Strange up)
     1.44758136E-05   2        -5         2   # BR(Hp -> Bottom up)
     1.14233015E-07   2        -1         4   # BR(Hp -> Down charm)
     6.30380750E-04   2        -3         4   # BR(Hp -> Strange charm)
     2.02706520E-03   2        -5         4   # BR(Hp -> Bottom charm)
     1.81919166E-08   2        -1         6   # BR(Hp -> Down top)
     8.40000780E-07   2        -3         6   # BR(Hp -> Strange top)
     6.93686826E-01   2        -5         6   # BR(Hp -> Bottom top)
     2.36427135E-02   2   1000022   1000024   # BR(Hp -> neutralino1 chargino1)
     1.40867746E-03   2   1000023   1000024   # BR(Hp -> neutralino2 chargino1)
     2.88066936E-05   2        24        25   # BR(Hp -> W h0)
     1.43407712E-07   2        24        35   # BR(Hp -> W HH)
     1.47234977E-07   2        24        36   # BR(Hp -> W A0)
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

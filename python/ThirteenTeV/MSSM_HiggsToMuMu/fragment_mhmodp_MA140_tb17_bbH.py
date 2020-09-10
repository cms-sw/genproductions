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
         3     1.70000000E+01   # TB
BLOCK EXTPAR
         0     0.00000000E+00   # Q
         1     9.54716519E+01   # M1
         2     2.00000000E+02   # M2
         3     1.50000000E+03   # M3
        11     1.51176471E+03   # At
        12     1.51176471E+03   # Ab
        13     1.51176471E+03   # Atau
        23     2.00000000E+02   # MUE
        25     1.70000000E+01   # TB
        26     1.40000000E+02   # MA0
        27     1.61442988E+02   # MHp
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
   1000012     4.95853894E+02   # MSf(1,1,1)
   1000011     5.02285129E+02   # MSf(1,2,1)
   2000011     5.01835197E+02   # MSf(2,2,1)
   1000002     1.49903195E+03   # MSf(1,3,1)
   2000002     1.49959137E+03   # MSf(2,3,1)
   1000001     1.50117161E+03   # MSf(1,4,1)
   2000001     1.50020423E+03   # MSf(2,4,1)
   1000014     4.95853894E+02   # MSf(1,1,2)
   1000013     5.02482660E+02   # MSf(1,2,2)
   2000013     5.01637434E+02   # MSf(2,2,2)
   1000004     1.49903246E+03   # MSf(1,3,2)
   2000004     1.49959197E+03   # MSf(2,3,2)
   1000003     1.50118299E+03   # MSf(1,4,2)
   2000003     1.50019284E+03   # MSf(2,4,2)
   1000016     9.97933407E+02   # MSf(1,1,3)
   1000015     9.99352075E+02   # MSf(1,2,3)
   2000015     1.00271165E+03   # MSf(2,2,3)
   1000006     8.76431645E+02   # MSf(1,3,3)
   2000006     1.13478418E+03   # MSf(2,3,3)
   1000005     9.97240312E+02   # MSf(1,4,3)
   2000005     1.00482428E+03   # MSf(2,4,3)
        25     1.22660618E+02   # Mh0
        35     1.42369985E+02   # MHH
        36     1.40000000E+02   # MA0
        37     1.62293951E+02   # MHp
   1000022     8.75535822E+01   # MNeu(1)
   1000023     1.51002900E+02   # MNeu(2)
   1000025    -2.09930774E+02   # MNeu(3)
   1000035     2.66845944E+02   # MNeu(4)
   1000024     1.46919458E+02   # MCha(1)
   1000037     2.67099887E+02   # MCha(2)
   1000021     1.50000000E+03   # MGl
BLOCK DMASS
         0     1.73200000E+02   # Q
        25     7.00093505E-01   # Delta Mh0
        35     4.08417355E-02   # Delta MHH
        36     0.00000000E+00   # Delta MA0
        37     5.67457302E-02   # Delta MHp
BLOCK NMIX
     1   1     9.28634751E-01   # ZNeu(1,1)
     1   2    -1.22185812E-01   # ZNeu(1,2)
     1   3     3.15291889E-01   # ZNeu(1,3)
     1   4    -1.52640597E-01   # ZNeu(1,4)
     2   1    -3.27743702E-01   # ZNeu(2,1)
     2   2    -6.94070464E-01   # ZNeu(2,2)
     2   3     5.05599719E-01   # ZNeu(2,3)
     2   4    -3.93978655E-01   # ZNeu(2,4)
     3   1     9.51432239E-02   # ZNeu(3,1)
     3   2    -1.32446955E-01   # ZNeu(3,2)
     3   3    -6.78375703E-01   # ZNeu(3,3)
     3   4    -7.16388146E-01   # ZNeu(3,4)
     4   1    -1.45496848E-01   # ZNeu(4,1)
     4   2     6.96989686E-01   # ZNeu(4,2)
     4   3     4.29844569E-01   # ZNeu(4,3)
     4   4    -5.55220399E-01   # ZNeu(4,4)
BLOCK UMIX
     1   1    -6.09087723E-01   # UCha(1,1)
     1   2     7.93102859E-01   # UCha(1,2)
     2   1     7.93102859E-01   # UCha(2,1)
     2   2     6.09087723E-01   # UCha(2,2)
BLOCK VMIX
     1   1    -7.93102859E-01   # VCha(1,1)
     1   2     6.09087723E-01   # VCha(1,2)
     2   1     6.09087723E-01   # VCha(2,1)
     2   2     7.93102859E-01   # VCha(2,2)
BLOCK STAUMIX
     1   1     6.82946892E-01   # USf(1,1)
     1   2     7.30468030E-01   # USf(1,2)
     2   1     7.30468030E-01   # USf(2,1)
     2   2    -6.82946892E-01   # USf(2,2)
BLOCK STOPMIX
     1   1     7.08247274E-01   # USf(1,1)
     1   2    -7.05964446E-01   # USf(1,2)
     2   1     7.05964446E-01   # USf(2,1)
     2   2     7.08247274E-01   # USf(2,2)
BLOCK SBOTMIX
     1   1     6.35921930E-01   # USf(1,1)
     1   2     7.71753392E-01   # USf(1,2)
     2   1     7.71753392E-01   # USf(2,1)
     2   2    -6.35921930E-01   # USf(2,2)
BLOCK ALPHA
              -3.70061451E-01   # Alpha
BLOCK DALPHA
               3.17837849E-02   # Delta Alpha
BLOCK HMIX Q= -0.99900000E+03
         1     2.00000000E+02   # MUE
         2     1.70000000E+01   # TB
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
     3   3     1.51176471E+03   # Af(3,3)
BLOCK AU Q=  0.00000000E+00
     1   1     0.00000000E+00   # Af(1,1)
     2   2     0.00000000E+00   # Af(2,2)
     3   3     1.51176471E+03   # Af(3,3)
BLOCK AD Q=  0.00000000E+00
     1   1     0.00000000E+00   # Af(1,1)
     2   2     0.00000000E+00   # Af(2,2)
     3   3     1.51176471E+03   # Af(3,3)
BLOCK YE Q=  0.00000000E+00
     1   1     4.99815400E-05   # Yf(1,1)
     2   2     1.03345964E-02   # Yf(2,2)
     3   3     1.73813869E-01   # Yf(3,3)
BLOCK YU Q=  0.00000000E+00
     1   1     1.72608429E-05   # Yf(1,1)
     2   2     7.39914798E-03   # Yf(2,2)
     3   3     9.96525996E-01   # Yf(3,3)
BLOCK YD Q=  0.00000000E+00
     1   1     5.76838791E-04   # Yf(1,1)
     2   2     9.13294928E-03   # Yf(2,2)
     3   3     3.86002572E-01   # Yf(3,3)
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
     3   3     2.62765672E+02   # Tf(3,3)
BLOCK TU Q=  0.00000000E+00
     1   1     0.00000000E+00   # Tf(1,1)
     2   2     0.00000000E+00   # Tf(2,2)
     3   3     1.50651283E+03   # Tf(3,3)
BLOCK TD Q=  0.00000000E+00
     1   1     0.00000000E+00   # Tf(1,1)
     2   2     0.00000000E+00   # Tf(2,2)
     3   3     5.83545064E+02   # Tf(3,3)
BLOCK SELMIX
     1   1     9.99992605E-01   # UASf(1,1)
     1   4    -3.84565237E-03   # UASf(1,4)
     2   2     8.75301349E-01   # UASf(2,2)
     2   5    -4.83577863E-01   # UASf(2,5)
     3   3     6.82946892E-01   # UASf(3,3)
     3   6     7.30468030E-01   # UASf(3,6)
     4   1     3.84565237E-03   # UASf(4,1)
     4   4     9.99992605E-01   # UASf(4,4)
     5   2     4.83577863E-01   # UASf(5,2)
     5   5     8.75301349E-01   # UASf(5,5)
     6   3     7.30468030E-01   # UASf(6,3)
     6   6    -6.82946892E-01   # UASf(6,6)
BLOCK USQMIX
     1   1     1.00000000E+00   # UASf(1,1)
     1   4     2.10398797E-05   # UASf(1,4)
     2   2     9.99959337E-01   # UASf(2,2)
     2   5     9.01799486E-03   # UASf(2,5)
     3   3     7.08247274E-01   # UASf(3,3)
     3   6    -7.05964446E-01   # UASf(3,6)
     4   1    -2.10398797E-05   # UASf(4,1)
     4   4     1.00000000E+00   # UASf(4,4)
     5   2    -9.01799486E-03   # UASf(5,2)
     5   5     9.99959337E-01   # UASf(5,5)
     6   3     7.05964446E-01   # UASf(6,3)
     6   6     7.08247274E-01   # UASf(6,6)
BLOCK DSQMIX
     1   1     9.99976152E-01   # UASf(1,1)
     1   4    -6.90614146E-03   # UASf(1,4)
     2   2     9.94210684E-01   # UASf(2,2)
     2   5    -1.07448203E-01   # UASf(2,5)
     3   3     6.35921930E-01   # UASf(3,3)
     3   6     7.71753392E-01   # UASf(3,6)
     4   1     6.90614146E-03   # UASf(4,1)
     4   4     9.99976152E-01   # UASf(4,4)
     5   2     1.07448203E-01   # UASf(5,2)
     5   5     9.94210684E-01   # UASf(5,5)
     6   3     7.71753392E-01   # UASf(6,3)
     6   6    -6.35921930E-01   # UASf(6,6)
BLOCK CVHMIX
     1   1     9.74121410E-01   # UH(1,1)
     1   2     2.26025393E-01   # UH(1,2)
     1   3     0.00000000E+00   # UH(1,3)
     2   1    -2.26025393E-01   # UH(2,1)
     2   2     9.74121410E-01   # UH(2,2)
     2   3     0.00000000E+00   # UH(2,3)
     3   1     0.00000000E+00   # UH(3,1)
     3   2     0.00000000E+00   # UH(3,2)
     3   3     1.00000000E+00   # UH(3,3)
DECAY        25     8.57421056E-02   # Gamma(h0)
     1.02052087E-04   2        22        22   # BR(h0 -> photon photon)
     5.00031273E-05   2        22        23   # BR(h0 -> photon Z)
     8.40987124E-04   2        23        23   # BR(h0 -> Z Z)
     7.28206515E-03   2       -24        24   # BR(h0 -> W W)
     1.95001668E-03   2        21        21   # BR(h0 -> gluon gluon)
     8.85520247E-09   2       -11        11   # BR(h0 -> Electron electron)
     3.93892941E-04   2       -13        13   # BR(h0 -> Muon muon)
     1.11913520E-01   2       -15        15   # BR(h0 -> Tau tau)
     8.07477798E-09   2        -2         2   # BR(h0 -> Up up)
     1.11841978E-03   2        -4         4   # BR(h0 -> Charm charm)
     1.38619378E-06   2        -1         1   # BR(h0 -> Down down)
     3.48105317E-04   2        -3         3   # BR(h0 -> Strange strange)
     8.75999534E-01   2        -5         5   # BR(h0 -> Bottom bottom)
DECAY        35     6.18594388E-01   # Gamma(HH)
    -7.74541674E-07   2        22        22   # BR(HH -> photon photon)
    -3.29424575E-06   2        22        23   # BR(HH -> photon Z)
    -1.07371914E-04   2        23        23   # BR(HH -> Z Z)
    -7.74781980E-04   2       -24        24   # BR(HH -> W W)
    -1.26346328E-03   2        21        21   # BR(HH -> gluon gluon)
    -9.33590073E-09   2       -11        11   # BR(HH -> Electron electron)
     4.15293869E-04   2       -13        13   # BR(HH -> Muon muon)
    -1.17720172E-01   2       -15        15   # BR(HH -> Tau tau)
    -1.89151886E-10   2        -2         2   # BR(HH -> Up up)
    -2.62032596E-05   2        -4         4   # BR(HH -> Charm charm)
    -1.40791017E-06   2        -1         1   # BR(HH -> Down down)
    -3.53557035E-04   2        -3         3   # BR(HH -> Strange strange)
    -8.79333670E-01   2        -5         5   # BR(HH -> Bottom bottom)
    -1.17450638E-10   2        23        36   # BR(HH -> Z A0)
DECAY        36     6.97947764E-01   # Gamma(A0)
     2.22035000E-07   2        22        22   # BR(A0 -> photon photon)
     1.03837322E-07   2        22        23   # BR(A0 -> photon Z)
     8.44834409E-04   2        21        21   # BR(A0 -> gluon gluon)
     9.29063273E-09   2       -11        11   # BR(A0 -> Electron electron)
     4.13279042E-04   2       -13        13   # BR(A0 -> Muon muon)
     1.17243042E-01   2       -15        15   # BR(A0 -> Tau tau)
     3.89948616E-12   2        -2         2   # BR(A0 -> Up up)
     5.53059770E-07   2        -4         4   # BR(A0 -> Charm charm)
     1.40675839E-06   2        -1         1   # BR(A0 -> Down down)
     3.53268207E-04   2        -3         3   # BR(A0 -> Strange strange)
     8.81143086E-01   2        -5         5   # BR(A0 -> Bottom bottom)
     1.94840997E-07   2        23        25   # BR(A0 -> Z h0)
DECAY        37     9.54584361E-02   # Gamma(Hp)
     8.13197557E-08   2       -11        12   # BR(Hp -> Electron nu_e)
     3.47667000E-03   2       -13        14   # BR(Hp -> Muon nu_mu)
     9.83199183E-01   2       -15        16   # BR(Hp -> Tau nu_tau)
     1.08515015E-05   2        -1         2   # BR(Hp -> Down up)
     1.21486778E-04   2        -3         2   # BR(Hp -> Strange up)
     7.20550943E-05   2        -5         2   # BR(Hp -> Bottom up)
     6.88135979E-07   2        -1         4   # BR(Hp -> Down charm)
     2.71982175E-03   2        -3         4   # BR(Hp -> Strange charm)
     1.00890261E-02   2        -5         4   # BR(Hp -> Bottom charm)
     1.06895750E-04   2        -5         6   # BR(Hp -> Bottom top)
     1.02305784E-04   2        24        25   # BR(Hp -> W h0)
     3.47137332E-05   2        24        35   # BR(Hp -> W HH)
     6.62210131E-05   2        24        36   # BR(Hp -> W A0)
DECAY         6     1.37314582E+00   # Gamma(top)
     9.98637810E-01   2         5        24   # BR(top -> bottom W)
     1.36218964E-03   2         5        37   # BR(top -> bottom Hp)
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

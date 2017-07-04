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
        26     1.90000000E+02   # MA0
        27     2.06310054E+02   # MHp
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
        25     1.24225197E+02   # Mh0
        35     1.90782906E+02   # MHH
        36     1.90000000E+02   # MA0
        37     2.07029779E+02   # MHp
   1000022     8.75535822E+01   # MNeu(1)
   1000023     1.51002900E+02   # MNeu(2)
   1000025    -2.09930774E+02   # MNeu(3)
   1000035     2.66845944E+02   # MNeu(4)
   1000024     1.46919458E+02   # MCha(1)
   1000037     2.67099887E+02   # MCha(2)
   1000021     1.50000000E+03   # MGl
BLOCK DMASS
         0     1.73200000E+02   # Q
        25     7.26990449E-01   # Delta Mh0
        35     1.60255882E-02   # Delta MHH
        36     0.00000000E+00   # Delta MA0
        37     3.79935297E-02   # Delta MHp
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
              -1.32587408E-01   # Alpha
BLOCK DALPHA
               2.74727866E-03   # Delta Alpha
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
     1   1     9.99226837E-01   # UH(1,1)
     1   2     3.93157609E-02   # UH(1,2)
     1   3     0.00000000E+00   # UH(1,3)
     2   1    -3.93157609E-02   # UH(2,1)
     2   2     9.99226837E-01   # UH(2,2)
     2   3     0.00000000E+00   # UH(2,3)
     3   1     0.00000000E+00   # UH(3,1)
     3   2     0.00000000E+00   # UH(3,2)
     3   3     1.00000000E+00   # UH(3,3)
DECAY        25     1.28208920E-02   # Gamma(h0)
     7.17371566E-04   2        22        22   # BR(h0 -> photon photon)
     4.30589492E-04   2        22        23   # BR(h0 -> photon Z)
     7.54960672E-03   2        23        23   # BR(h0 -> Z Z)
     6.33369273E-02   2       -24        24   # BR(h0 -> W W)
     1.91365355E-02   2        21        21   # BR(h0 -> gluon gluon)
     7.80756590E-09   2       -11        11   # BR(h0 -> Electron electron)
     3.47293777E-04   2       -13        13   # BR(h0 -> Muon muon)
     9.91128823E-02   2       -15        15   # BR(h0 -> Tau tau)
     6.17078866E-08   2        -2         2   # BR(h0 -> Up up)
     8.54710093E-03   2        -4         4   # BR(h0 -> Charm charm)
     1.24174626E-06   2        -1         1   # BR(h0 -> Down down)
     3.11837334E-04   2        -3         3   # BR(h0 -> Strange strange)
     8.00508544E-01   2        -5         5   # BR(h0 -> Bottom bottom)
DECAY        35     9.04879089E-01   # Gamma(HH)
    -7.25085980E-07   2        22        22   # BR(HH -> photon photon)
    -7.26677755E-07   2        22        23   # BR(HH -> photon Z)
    -1.41084273E-03   2        23        23   # BR(HH -> Z Z)
    -5.00373164E-03   2       -24        24   # BR(HH -> W W)
    -7.10765468E-04   2        21        21   # BR(HH -> gluon gluon)
    -9.65193527E-09   2       -11        11   # BR(HH -> Electron electron)
     4.29390006E-04   2       -13        13   # BR(HH -> Muon muon)
    -1.21774324E-01   2       -15        15   # BR(HH -> Tau tau)
    -2.06555228E-11   2        -2         2   # BR(HH -> Up up)
    -2.86054923E-06   2        -4         4   # BR(HH -> Charm charm)
    -1.38527399E-06   2        -1         1   # BR(HH -> Down down)
    -3.47877623E-04   2        -3         3   # BR(HH -> Strange strange)
    -8.66776550E-01   2        -5         5   # BR(HH -> Bottom bottom)
    -3.54081123E-03   2   1000022   1000022   # BR(HH -> neutralino1 neutralino1)
DECAY        36     9.25252296E-01   # Gamma(A0)
     3.54451557E-07   2        22        22   # BR(A0 -> photon photon)
     7.10234789E-07   2        22        23   # BR(A0 -> photon Z)
     5.02943795E-04   2        21        21   # BR(A0 -> gluon gluon)
     9.47919880E-09   2       -11        11   # BR(A0 -> Electron electron)
     4.21705406E-04   2       -13        13   # BR(A0 -> Muon muon)
     1.19626990E-01   2       -15        15   # BR(A0 -> Tau tau)
     3.60049459E-12   2        -2         2   # BR(A0 -> Up up)
     5.04229676E-07   2        -4         4   # BR(A0 -> Charm charm)
     1.36153145E-06   2        -1         1   # BR(A0 -> Down down)
     3.41915414E-04   2        -3         3   # BR(A0 -> Strange strange)
     8.52863032E-01   2        -5         5   # BR(A0 -> Bottom bottom)
     2.62340290E-02   2   1000022   1000022   # BR(A0 -> neutralino1 neutralino1)
     6.44384733E-06   2        23        25   # BR(A0 -> Z h0)
DECAY        37     1.96559518E-01   # Gamma(Hp)
     5.03786804E-08   2       -11        12   # BR(Hp -> Electron nu_e)
     2.15384444E-03   2       -13        14   # BR(Hp -> Muon nu_mu)
     6.09161401E-01   2       -15        16   # BR(Hp -> Tau nu_tau)
     6.44514717E-06   2        -1         2   # BR(Hp -> Down up)
     7.25592530E-05   2        -3         2   # BR(Hp -> Strange up)
     4.30574661E-05   2        -5         2   # BR(Hp -> Bottom up)
     4.11016309E-07   2        -1         4   # BR(Hp -> Down charm)
     1.61549163E-03   2        -3         4   # BR(Hp -> Strange charm)
     6.02912384E-03   2        -5         4   # BR(Hp -> Bottom charm)
     1.20850980E-06   2        -1         6   # BR(Hp -> Down top)
     2.65451682E-05   2        -3         6   # BR(Hp -> Strange top)
     3.80609386E-01   2        -5         6   # BR(Hp -> Bottom top)
     2.64430540E-04   2        24        25   # BR(Hp -> W h0)
     7.07581560E-06   2        24        35   # BR(Hp -> W HH)
     8.96966159E-06   2        24        36   # BR(Hp -> W A0)
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

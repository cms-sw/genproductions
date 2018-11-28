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
         3     2.00000000E+01   # TB
BLOCK EXTPAR
         0     0.00000000E+00   # Q
         1     9.54716519E+01   # M1
         2     2.00000000E+02   # M2
         3     1.50000000E+03   # M3
        11     1.51000000E+03   # At
        12     1.51000000E+03   # Ab
        13     1.51000000E+03   # Atau
        23     2.00000000E+02   # MUE
        25     2.00000000E+01   # TB
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
   1000012     4.95845890E+02   # MSf(1,1,1)
   1000011     5.02289514E+02   # MSf(1,2,1)
   2000011     5.01838716E+02   # MSf(2,2,1)
   1000002     1.49903009E+03   # MSf(1,3,1)
   2000002     1.49959059E+03   # MSf(2,3,1)
   1000001     1.50117388E+03   # MSf(1,4,1)
   2000001     1.50020460E+03   # MSf(2,4,1)
   1000014     4.95845890E+02   # MSf(1,1,2)
   1000013     5.02541395E+02   # MSf(1,2,2)
   2000013     5.01586505E+02   # MSf(2,2,2)
   1000004     1.49903061E+03   # MSf(1,3,2)
   2000004     1.49959117E+03   # MSf(2,3,2)
   1000003     1.50118944E+03   # MSf(1,4,2)
   2000003     1.50018903E+03   # MSf(2,4,2)
   1000016     9.97929430E+02   # MSf(1,1,3)
   1000015     9.98819801E+02   # MSf(1,2,3)
   2000015     1.00324582E+03   # MSf(2,2,3)
   1000006     8.76429378E+02   # MSf(1,3,3)
   2000006     1.13478243E+03   # MSf(2,3,3)
   1000005     9.96111023E+02   # MSf(1,4,3)
   2000005     1.00594745E+03   # MSf(2,4,3)
        25     1.24408699E+02   # Mh0
        35     1.80681169E+02   # MHH
        36     1.80000000E+02   # MA0
        37     1.98233628E+02   # MHp
   1000022     8.78206612E+01   # MNeu(1)
   1000023     1.51359250E+02   # MNeu(2)
   1000025    -2.10117348E+02   # MNeu(3)
   1000035     2.66409088E+02   # MNeu(4)
   1000024     1.47528170E+02   # MCha(1)
   1000037     2.66764158E+02   # MCha(2)
   1000021     1.50000000E+03   # MGl
BLOCK DMASS
         0     1.73200000E+02   # Q
        25     7.24604656E-01   # Delta Mh0
        35     1.37963419E-02   # Delta MHH
        36     0.00000000E+00   # Delta MA0
        37     2.34727073E-02   # Delta MHp
BLOCK NMIX
     1   1     9.30213894E-01   # ZNeu(1,1)
     1   2    -1.18546783E-01   # ZNeu(1,2)
     1   3     3.13311260E-01   # ZNeu(1,3)
     1   4    -1.49949409E-01   # ZNeu(1,4)
     2   1    -3.23202104E-01   # ZNeu(2,1)
     2   2    -6.93891430E-01   # ZNeu(2,2)
     2   3     5.08023191E-01   # ZNeu(2,3)
     2   4    -3.94927234E-01   # ZNeu(2,4)
     3   1     9.59510181E-02   # ZNeu(3,1)
     3   2    -1.33592266E-01   # ZNeu(3,2)
     3   3    -6.78206777E-01   # ZNeu(3,3)
     3   4    -7.16227671E-01   # ZNeu(3,4)
     4   1    -1.45037624E-01   # ZNeu(4,1)
     4   2     6.97577558E-01   # ZNeu(4,2)
     4   3     4.28700431E-01   # ZNeu(4,3)
     4   4    -5.55486794E-01   # ZNeu(4,4)
BLOCK UMIX
     1   1    -6.08113363E-01   # UCha(1,1)
     1   2     7.93850198E-01   # UCha(1,2)
     2   1     7.93850198E-01   # UCha(2,1)
     2   2     6.08113363E-01   # UCha(2,2)
BLOCK VMIX
     1   1    -7.93850198E-01   # VCha(1,1)
     1   2     6.08113363E-01   # VCha(1,2)
     2   1     6.08113363E-01   # VCha(2,1)
     2   2     7.93850198E-01   # VCha(2,2)
BLOCK STAUMIX
     1   1     6.88810103E-01   # USf(1,1)
     1   2     7.24941820E-01   # USf(1,2)
     2   1     7.24941820E-01   # USf(2,1)
     2   2    -6.88810103E-01   # USf(2,2)
BLOCK STOPMIX
     1   1     7.08249465E-01   # USf(1,1)
     1   2    -7.05962248E-01   # USf(1,2)
     2   1     7.05962248E-01   # USf(2,1)
     2   2     7.08249465E-01   # USf(2,2)
BLOCK SBOTMIX
     1   1     6.52799510E-01   # USf(1,1)
     1   2     7.57530726E-01   # USf(1,2)
     2   1     7.57530726E-01   # USf(2,1)
     2   2    -6.52799510E-01   # USf(2,2)
BLOCK ALPHA
              -1.27963376E-01   # Alpha
BLOCK DALPHA
               3.19728716E-03   # Delta Alpha
BLOCK HMIX Q= -0.99900000E+03
         1     2.00000000E+02   # MUE
         2     2.00000000E+01   # TB
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
     3   3     1.51000000E+03   # Af(3,3)
BLOCK AU Q=  0.00000000E+00
     1   1     0.00000000E+00   # Af(1,1)
     2   2     0.00000000E+00   # Af(2,2)
     3   3     1.51000000E+03   # Af(3,3)
BLOCK AD Q=  0.00000000E+00
     1   1     0.00000000E+00   # Af(1,1)
     2   2     0.00000000E+00   # Af(2,2)
     3   3     1.51000000E+03   # Af(3,3)
BLOCK YE Q=  0.00000000E+00
     1   1     5.87736714E-05   # Yf(1,1)
     2   2     1.21525301E-02   # Yf(2,2)
     3   3     2.04389044E-01   # Yf(3,3)
BLOCK YU Q=  0.00000000E+00
     1   1     1.72525825E-05   # Yf(1,1)
     2   2     7.39560703E-03   # Yf(2,2)
     3   3     9.96049095E-01   # Yf(3,3)
BLOCK YD Q=  0.00000000E+00
     1   1     6.76269414E-04   # Yf(1,1)
     2   2     1.07071436E-02   # Yf(2,2)
     3   3     4.49839737E-01   # Yf(3,3)
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
     3   3     3.08627457E+02   # Tf(3,3)
BLOCK TU Q=  0.00000000E+00
     1   1     0.00000000E+00   # Tf(1,1)
     2   2     0.00000000E+00   # Tf(2,2)
     3   3     1.50403413E+03   # Tf(3,3)
BLOCK TD Q=  0.00000000E+00
     1   1     0.00000000E+00   # Tf(1,1)
     2   2     0.00000000E+00   # Tf(2,2)
     3   3     6.79258003E+02   # Tf(3,3)
BLOCK SELMIX
     1   1     9.99989805E-01   # UASf(1,1)
     1   4    -4.51557869E-03   # UASf(1,4)
     2   2     8.57926156E-01   # UASf(2,2)
     2   5    -5.13773015E-01   # UASf(2,5)
     3   3     6.88810103E-01   # UASf(3,3)
     3   6     7.24941820E-01   # UASf(3,6)
     4   1     4.51557869E-03   # UASf(4,1)
     4   4     9.99989805E-01   # UASf(4,4)
     5   2     5.13773015E-01   # UASf(5,2)
     5   5     8.57926156E-01   # UASf(5,5)
     6   3     7.24941820E-01   # UASf(6,3)
     6   6    -6.88810103E-01   # UASf(6,6)
BLOCK USQMIX
     1   1     1.00000000E+00   # UASf(1,1)
     1   4     1.78495859E-05   # UASf(1,4)
     2   2     9.99970732E-01   # UASf(2,2)
     2   5     7.65085064E-03   # UASf(2,5)
     3   3     7.08249465E-01   # UASf(3,3)
     3   6    -7.05962248E-01   # UASf(3,6)
     4   1    -1.78495859E-05   # UASf(4,1)
     4   4     1.00000000E+00   # UASf(4,4)
     5   2    -7.65085064E-03   # UASf(5,2)
     5   5     9.99970732E-01   # UASf(5,5)
     6   3     7.05962248E-01   # UASf(6,3)
     6   6     7.08249465E-01   # UASf(6,6)
BLOCK DSQMIX
     1   1     9.99967318E-01   # UASf(1,1)
     1   4    -8.08468495E-03   # UASf(1,4)
     2   2     9.92157399E-01   # UASf(2,2)
     2   5    -1.24994779E-01   # UASf(2,5)
     3   3     6.52799510E-01   # UASf(3,3)
     3   6     7.57530726E-01   # UASf(3,6)
     4   1     8.08468495E-03   # UASf(4,1)
     4   4     9.99967318E-01   # UASf(4,4)
     5   2     1.24994779E-01   # UASf(5,2)
     5   5     9.92157399E-01   # UASf(5,5)
     6   3     7.57530726E-01   # UASf(6,3)
     6   6    -6.52799510E-01   # UASf(6,6)
BLOCK CVHMIX
     1   1     9.99031078E-01   # UH(1,1)
     1   2     4.40102961E-02   # UH(1,2)
     1   3     0.00000000E+00   # UH(1,3)
     2   1    -4.40102961E-02   # UH(2,1)
     2   2     9.99031078E-01   # UH(2,2)
     2   3     0.00000000E+00   # UH(2,3)
     3   1     0.00000000E+00   # UH(3,1)
     3   2     0.00000000E+00   # UH(3,2)
     3   3     1.00000000E+00   # UH(3,3)
DECAY        25     1.59364808E-02   # Gamma(h0)
     5.81161313E-04   2        22        22   # BR(h0 -> photon photon)
     3.51534362E-04   2        22        23   # BR(h0 -> photon Z)
     6.21247618E-03   2        23        23   # BR(h0 -> Z Z)
     5.19278084E-02   2       -24        24   # BR(h0 -> W W)
     1.50999910E-02   2        21        21   # BR(h0 -> gluon gluon)
     8.12747120E-09   2       -11        11   # BR(h0 -> Electron electron)
     3.61523866E-04   2       -13        13   # BR(h0 -> Muon muon)
     1.02896341E-01   2       -15        15   # BR(h0 -> Tau tau)
     4.97176718E-08   2        -2         2   # BR(h0 -> Up up)
     6.88636927E-03   2        -4         4   # BR(h0 -> Charm charm)
     1.27899560E-06   2        -1         1   # BR(h0 -> Down down)
     3.21187940E-04   2        -3         3   # BR(h0 -> Strange strange)
     8.15360270E-01   2        -5         5   # BR(h0 -> Bottom bottom)
DECAY        35     1.16936527E+00   # Gamma(HH)
    -5.40419451E-07   2        22        22   # BR(HH -> photon photon)
    -4.88947201E-07   2        22        23   # BR(HH -> photon Z)
    -2.46933430E-04   2        23        23   # BR(HH -> Z Z)
    -3.14467967E-03   2       -24        24   # BR(HH -> W W)
    -7.14166332E-04   2        21        21   # BR(HH -> gluon gluon)
    -9.84014263E-09   2       -11        11   # BR(HH -> Electron electron)
     4.37755722E-04   2       -13        13   # BR(HH -> Muon muon)
    -1.23782243E-01   2       -15        15   # BR(HH -> Tau tau)
    -1.41777018E-11   2        -2         2   # BR(HH -> Up up)
    -1.96411112E-06   2        -4         4   # BR(HH -> Charm charm)
    -1.40883632E-06   2        -1         1   # BR(HH -> Down down)
    -3.53789509E-04   2        -3         3   # BR(HH -> Strange strange)
    -8.70788287E-01   2        -5         5   # BR(HH -> Bottom bottom)
    -5.27732609E-04   2   1000022   1000022   # BR(HH -> neutralino1 neutralino1)
DECAY        36     1.18555789E+00   # Gamma(A0)
     2.11800078E-07   2        22        22   # BR(A0 -> photon photon)
     3.52484410E-07   2        22        23   # BR(A0 -> photon Z)
     5.79964429E-04   2        21        21   # BR(A0 -> gluon gluon)
     9.75822268E-09   2       -11        11   # BR(A0 -> Electron electron)
     4.34111489E-04   2       -13        13   # BR(A0 -> Muon muon)
     1.22790648E-01   2       -15        15   # BR(A0 -> Tau tau)
     1.91543780E-12   2        -2         2   # BR(A0 -> Up up)
     2.69157443E-07   2        -4         4   # BR(A0 -> Charm charm)
     1.39814680E-06   2        -1         1   # BR(A0 -> Down down)
     3.51105309E-04   2        -3         3   # BR(A0 -> Strange strange)
     8.65226323E-01   2        -5         5   # BR(A0 -> Bottom bottom)
     1.06133224E-02   2   1000022   1000022   # BR(A0 -> neutralino1 neutralino1)
     2.28371820E-06   2        23        25   # BR(A0 -> Z h0)
DECAY        37     2.20379004E-01   # Gamma(Hp)
     5.95493890E-08   2       -11        12   # BR(Hp -> Electron nu_e)
     2.54592048E-03   2       -13        14   # BR(Hp -> Muon nu_mu)
     7.20040780E-01   2       -15        16   # BR(Hp -> Tau nu_tau)
     7.62796645E-06   2        -1         2   # BR(Hp -> Down up)
     8.58016703E-05   2        -3         2   # BR(Hp -> Strange up)
     5.03071455E-05   2        -5         2   # BR(Hp -> Bottom up)
     4.17561856E-07   2        -1         4   # BR(Hp -> Down charm)
     1.91040871E-03   2        -3         4   # BR(Hp -> Strange charm)
     7.04420954E-03   2        -5         4   # BR(Hp -> Bottom charm)
     4.66555326E-07   2        -1         6   # BR(Hp -> Down top)
     1.03162579E-05   2        -3         6   # BR(Hp -> Strange top)
     2.68185608E-01   2        -5         6   # BR(Hp -> Bottom top)
     9.77773563E-05   2        24        25   # BR(Hp -> W h0)
     9.17238085E-06   2        24        35   # BR(Hp -> W HH)
     1.11274727E-05   2        24        36   # BR(Hp -> W A0)
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

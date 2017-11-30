# This file was automatically created by FeynRules 1.7.178
# Mathematica version: 9.0 for Mac OS X x86 (64-bit) (November 20, 2012)
# Date: Sun 26 Jan 2014 12:11:59


from object_library import all_vertices, Vertex
import particles as P
import couplings as C
import lorentz as L


V_1 = Vertex(name = 'V_1',
             particles = [ P.h, P.h, P.h, P.h ],
             color = [ '1' ],
             lorentz = [ L.SSSS1 ],
             couplings = {(0,0):C.GC_122})

V_2 = Vertex(name = 'V_2',
             particles = [ P.h, P.h, P.h ],
             color = [ '1' ],
             lorentz = [ L.SSS1 ],
             couplings = {(0,0):C.GC_123})

V_3 = Vertex(name = 'V_3',
             particles = [ P.ghG, P.ghG__tilde__, P.G ],
             color = [ 'f(1,2,3)' ],
             lorentz = [ L.UUV1 ],
             couplings = {(0,0):C.GC_4})

V_4 = Vertex(name = 'V_4',
             particles = [ P.G, P.G, P.G ],
             color = [ 'f(1,2,3)' ],
             lorentz = [ L.VVV7 ],
             couplings = {(0,0):C.GC_4})

V_5 = Vertex(name = 'V_5',
             particles = [ P.G, P.G, P.G, P.G ],
             color = [ 'f(-1,1,2)*f(3,4,-1)', 'f(-1,1,3)*f(2,4,-1)', 'f(-1,1,4)*f(2,3,-1)' ],
             lorentz = [ L.VVVV1, L.VVVV3, L.VVVV4 ],
             couplings = {(1,1):C.GC_6,(0,0):C.GC_6,(2,2):C.GC_6})

V_6 = Vertex(name = 'V_6',
             particles = [ P.b__tilde__, P.b, P.G ],
             color = [ 'T(3,2,1)' ],
             lorentz = [ L.FFV1 ],
             couplings = {(0,0):C.GC_5})

V_7 = Vertex(name = 'V_7',
             particles = [ P.d__tilde__, P.d, P.G ],
             color = [ 'T(3,2,1)' ],
             lorentz = [ L.FFV1 ],
             couplings = {(0,0):C.GC_5})

V_8 = Vertex(name = 'V_8',
             particles = [ P.s__tilde__, P.s, P.G ],
             color = [ 'T(3,2,1)' ],
             lorentz = [ L.FFV1 ],
             couplings = {(0,0):C.GC_5})

V_9 = Vertex(name = 'V_9',
             particles = [ P.c__tilde__, P.c, P.G ],
             color = [ 'T(3,2,1)' ],
             lorentz = [ L.FFV1 ],
             couplings = {(0,0):C.GC_5})

V_10 = Vertex(name = 'V_10',
              particles = [ P.t__tilde__, P.t, P.G ],
              color = [ 'T(3,2,1)' ],
              lorentz = [ L.FFV1 ],
              couplings = {(0,0):C.GC_5})

V_11 = Vertex(name = 'V_11',
              particles = [ P.u__tilde__, P.u, P.G ],
              color = [ 'T(3,2,1)' ],
              lorentz = [ L.FFV1 ],
              couplings = {(0,0):C.GC_5})

V_12 = Vertex(name = 'V_12',
              particles = [ P.A, P.Vc__minus__, P.Vc__plus__ ],
              color = [ '1' ],
              lorentz = [ L.VVV1, L.VVV7, L.VVV9 ],
              couplings = {(0,0):C.GC_95,(0,1):C.GC_98,(0,2):C.GC_93})

V_13 = Vertex(name = 'V_13',
              particles = [ P.Vc__minus__, P.Vc__plus__, P.h, P.h ],
              color = [ '1' ],
              lorentz = [ L.VVSS1 ],
              couplings = {(0,0):C.GC_119})

V_14 = Vertex(name = 'V_14',
              particles = [ P.Vc__minus__, P.Vc__plus__, P.h ],
              color = [ '1' ],
              lorentz = [ L.VVS1 ],
              couplings = {(0,0):C.GC_124})

V_15 = Vertex(name = 'V_15',
              particles = [ P.A, P.A, P.Vc__minus__, P.Vc__plus__ ],
              color = [ '1' ],
              lorentz = [ L.VVVV2 ],
              couplings = {(0,0):C.GC_115})

V_16 = Vertex(name = 'V_16',
              particles = [ P.Vc__minus__, P.Vc__plus__, P.Vz ],
              color = [ '1' ],
              lorentz = [ L.VVV3, L.VVV4, L.VVV5, L.VVV7 ],
              couplings = {(0,3):C.GC_57,(0,1):C.GC_14,(0,0):C.GC_49,(0,2):C.GC_38})

V_17 = Vertex(name = 'V_17',
              particles = [ P.Vc__minus__, P.Vc__plus__, P.Z ],
              color = [ '1' ],
              lorentz = [ L.VVV3, L.VVV4, L.VVV5, L.VVV7 ],
              couplings = {(0,0):C.GC_51,(0,3):C.GC_48,(0,1):C.GC_41,(0,2):C.GC_11})

V_18 = Vertex(name = 'V_18',
              particles = [ P.Vc__minus__, P.Vc__minus__, P.Vc__plus__, P.Vc__plus__ ],
              color = [ '1' ],
              lorentz = [ L.VVVV2 ],
              couplings = {(0,0):C.GC_30})

V_19 = Vertex(name = 'V_19',
              particles = [ P.Vc__minus__, P.Vz, P.W__plus__ ],
              color = [ '1' ],
              lorentz = [ L.VVV1, L.VVV2, L.VVV3, L.VVV5, L.VVV7, L.VVV9 ],
              couplings = {(0,2):C.GC_7,(0,4):C.GC_12,(0,0):C.GC_19,(0,5):C.GC_16,(0,1):C.GC_53,(0,3):C.GC_9})

V_20 = Vertex(name = 'V_20',
              particles = [ P.A, P.Vc__minus__, P.Vc__plus__, P.Vz ],
              color = [ '1' ],
              lorentz = [ L.VVVV5 ],
              couplings = {(0,0):C.GC_114})

V_21 = Vertex(name = 'V_21',
              particles = [ P.Vc__plus__, P.Vz, P.W__minus__ ],
              color = [ '1' ],
              lorentz = [ L.VVV1, L.VVV2, L.VVV3, L.VVV5, L.VVV7, L.VVV9 ],
              couplings = {(0,2):C.GC_8,(0,4):C.GC_13,(0,0):C.GC_18,(0,5):C.GC_17,(0,1):C.GC_54,(0,3):C.GC_10})

V_22 = Vertex(name = 'V_22',
              particles = [ P.Vz, P.Vz, P.h, P.h ],
              color = [ '1' ],
              lorentz = [ L.VVSS1 ],
              couplings = {(0,0):C.GC_121})

V_23 = Vertex(name = 'V_23',
              particles = [ P.Vz, P.Vz, P.h ],
              color = [ '1' ],
              lorentz = [ L.VVS1 ],
              couplings = {(0,0):C.GC_127})

V_24 = Vertex(name = 'V_24',
              particles = [ P.Vc__minus__, P.Vc__plus__, P.Vz, P.Vz ],
              color = [ '1' ],
              lorentz = [ L.VVVV2 ],
              couplings = {(0,0):C.GC_67})

V_25 = Vertex(name = 'V_25',
              particles = [ P.A, P.W__minus__, P.W__plus__ ],
              color = [ '1' ],
              lorentz = [ L.VVV1, L.VVV7, L.VVV9 ],
              couplings = {(0,1):C.GC_94,(0,0):C.GC_99,(0,2):C.GC_97})

V_26 = Vertex(name = 'V_26',
              particles = [ P.Vc__plus__, P.W__minus__, P.h, P.h ],
              color = [ '1' ],
              lorentz = [ L.VVSS1 ],
              couplings = {(0,0):C.GC_118})

V_27 = Vertex(name = 'V_27',
              particles = [ P.Vc__plus__, P.W__minus__, P.h ],
              color = [ '1' ],
              lorentz = [ L.VVS1 ],
              couplings = {(0,0):C.GC_126})

V_28 = Vertex(name = 'V_28',
              particles = [ P.A, P.Vc__plus__, P.W__minus__ ],
              color = [ '1' ],
              lorentz = [ L.VVV1 ],
              couplings = {(0,0):C.GC_105})

V_29 = Vertex(name = 'V_29',
              particles = [ P.Vc__plus__, P.W__minus__, P.Z ],
              color = [ '1' ],
              lorentz = [ L.VVV1, L.VVV2, L.VVV5, L.VVV6, L.VVV7, L.VVV9 ],
              couplings = {(0,3):C.GC_35,(0,4):C.GC_40,(0,0):C.GC_45,(0,1):C.GC_36,(0,5):C.GC_44,(0,2):C.GC_23})

V_30 = Vertex(name = 'V_30',
              particles = [ P.Vc__minus__, P.Vc__plus__, P.Vc__plus__, P.W__minus__ ],
              color = [ '1' ],
              lorentz = [ L.VVVV5 ],
              couplings = {(0,0):C.GC_26})

V_31 = Vertex(name = 'V_31',
              particles = [ P.A, P.Vc__plus__, P.Vz, P.W__minus__ ],
              color = [ '1' ],
              lorentz = [ L.VVVV1, L.VVVV4, L.VVVV7 ],
              couplings = {(0,0):C.GC_107,(0,2):C.GC_96,(0,1):C.GC_106})

V_32 = Vertex(name = 'V_32',
              particles = [ P.Vz, P.W__minus__, P.W__plus__ ],
              color = [ '1' ],
              lorentz = [ L.VVV1, L.VVV7, L.VVV8, L.VVV9 ],
              couplings = {(0,2):C.GC_15,(0,1):C.GC_50,(0,0):C.GC_47,(0,3):C.GC_56})

V_33 = Vertex(name = 'V_33',
              particles = [ P.Vc__plus__, P.Vz, P.Vz, P.W__minus__ ],
              color = [ '1' ],
              lorentz = [ L.VVVV5 ],
              couplings = {(0,0):C.GC_58})

V_34 = Vertex(name = 'V_34',
              particles = [ P.Vc__plus__, P.Vc__plus__, P.W__minus__, P.W__minus__ ],
              color = [ '1' ],
              lorentz = [ L.VVVV2 ],
              couplings = {(0,0):C.GC_31})

V_35 = Vertex(name = 'V_35',
              particles = [ P.Vc__minus__, P.W__plus__, P.h, P.h ],
              color = [ '1' ],
              lorentz = [ L.VVSS1 ],
              couplings = {(0,0):C.GC_118})

V_36 = Vertex(name = 'V_36',
              particles = [ P.Vc__minus__, P.W__plus__, P.h ],
              color = [ '1' ],
              lorentz = [ L.VVS1 ],
              couplings = {(0,0):C.GC_126})

V_37 = Vertex(name = 'V_37',
              particles = [ P.A, P.Vc__minus__, P.W__plus__ ],
              color = [ '1' ],
              lorentz = [ L.VVV1 ],
              couplings = {(0,0):C.GC_104})

V_38 = Vertex(name = 'V_38',
              particles = [ P.Vc__minus__, P.W__plus__, P.Z ],
              color = [ '1' ],
              lorentz = [ L.VVV1, L.VVV2, L.VVV5, L.VVV6, L.VVV7, L.VVV9 ],
              couplings = {(0,3):C.GC_34,(0,4):C.GC_39,(0,0):C.GC_46,(0,1):C.GC_37,(0,5):C.GC_43,(0,2):C.GC_22})

V_39 = Vertex(name = 'V_39',
              particles = [ P.Vc__minus__, P.Vc__minus__, P.Vc__plus__, P.W__plus__ ],
              color = [ '1' ],
              lorentz = [ L.VVVV2 ],
              couplings = {(0,0):C.GC_25})

V_40 = Vertex(name = 'V_40',
              particles = [ P.A, P.Vc__minus__, P.Vz, P.W__plus__ ],
              color = [ '1' ],
              lorentz = [ L.VVVV1, L.VVVV4, L.VVVV7 ],
              couplings = {(0,0):C.GC_107,(0,2):C.GC_96,(0,1):C.GC_106})

V_41 = Vertex(name = 'V_41',
              particles = [ P.Vc__minus__, P.Vz, P.Vz, P.W__plus__ ],
              color = [ '1' ],
              lorentz = [ L.VVVV5 ],
              couplings = {(0,0):C.GC_58})

V_42 = Vertex(name = 'V_42',
              particles = [ P.W__minus__, P.W__plus__, P.h, P.h ],
              color = [ '1' ],
              lorentz = [ L.VVSS1 ],
              couplings = {(0,0):C.GC_116})

V_43 = Vertex(name = 'V_43',
              particles = [ P.W__minus__, P.W__plus__, P.h ],
              color = [ '1' ],
              lorentz = [ L.VVS1 ],
              couplings = {(0,0):C.GC_125})

V_44 = Vertex(name = 'V_44',
              particles = [ P.A, P.A, P.W__minus__, P.W__plus__ ],
              color = [ '1' ],
              lorentz = [ L.VVVV2 ],
              couplings = {(0,0):C.GC_115})

V_45 = Vertex(name = 'V_45',
              particles = [ P.Vc__minus__, P.Vc__plus__, P.W__minus__, P.W__plus__ ],
              color = [ '1' ],
              lorentz = [ L.VVVV1, L.VVVV4, L.VVVV6, L.VVVV7 ],
              couplings = {(0,0):C.GC_33,(0,3):C.GC_24,(0,2):C.GC_21,(0,1):C.GC_32})

V_46 = Vertex(name = 'V_46',
              particles = [ P.A, P.Vz, P.W__minus__, P.W__plus__ ],
              color = [ '1' ],
              lorentz = [ L.VVVV2 ],
              couplings = {(0,0):C.GC_113})

V_47 = Vertex(name = 'V_47',
              particles = [ P.Vz, P.Vz, P.W__minus__, P.W__plus__ ],
              color = [ '1' ],
              lorentz = [ L.VVVV2 ],
              couplings = {(0,0):C.GC_68})

V_48 = Vertex(name = 'V_48',
              particles = [ P.W__minus__, P.W__plus__, P.Z ],
              color = [ '1' ],
              lorentz = [ L.VVV3, L.VVV4, L.VVV5, L.VVV7 ],
              couplings = {(0,3):C.GC_55,(0,0):C.GC_52,(0,1):C.GC_42,(0,2):C.GC_20})

V_49 = Vertex(name = 'V_49',
              particles = [ P.Vc__plus__, P.W__minus__, P.W__minus__, P.W__plus__ ],
              color = [ '1' ],
              lorentz = [ L.VVVV5 ],
              couplings = {(0,0):C.GC_29})

V_50 = Vertex(name = 'V_50',
              particles = [ P.Vc__minus__, P.Vc__minus__, P.W__plus__, P.W__plus__ ],
              color = [ '1' ],
              lorentz = [ L.VVVV2 ],
              couplings = {(0,0):C.GC_31})

V_51 = Vertex(name = 'V_51',
              particles = [ P.Vc__minus__, P.W__minus__, P.W__plus__, P.W__plus__ ],
              color = [ '1' ],
              lorentz = [ L.VVVV2 ],
              couplings = {(0,0):C.GC_28})

V_52 = Vertex(name = 'V_52',
              particles = [ P.W__minus__, P.W__minus__, P.W__plus__, P.W__plus__ ],
              color = [ '1' ],
              lorentz = [ L.VVVV2 ],
              couplings = {(0,0):C.GC_27})

V_53 = Vertex(name = 'V_53',
              particles = [ P.b__tilde__, P.b, P.h ],
              color = [ 'Identity(1,2)' ],
              lorentz = [ L.FFS1 ],
              couplings = {(0,0):C.GC_130})

V_54 = Vertex(name = 'V_54',
              particles = [ P.tt__plus__, P.tt__minus__, P.h ],
              color = [ '1' ],
              lorentz = [ L.FFS1 ],
              couplings = {(0,0):C.GC_133})

V_55 = Vertex(name = 'V_55',
              particles = [ P.c__tilde__, P.c, P.h ],
              color = [ 'Identity(1,2)' ],
              lorentz = [ L.FFS1 ],
              couplings = {(0,0):C.GC_131})

V_56 = Vertex(name = 'V_56',
              particles = [ P.t__tilde__, P.t, P.h ],
              color = [ 'Identity(1,2)' ],
              lorentz = [ L.FFS1 ],
              couplings = {(0,0):C.GC_132})

V_57 = Vertex(name = 'V_57',
              particles = [ P.A, P.Vc__minus__, P.Vc__plus__, P.Z ],
              color = [ '1' ],
              lorentz = [ L.VVVV5 ],
              couplings = {(0,0):C.GC_109})

V_58 = Vertex(name = 'V_58',
              particles = [ P.Vz, P.Z, P.h, P.h ],
              color = [ '1' ],
              lorentz = [ L.VVSS1 ],
              couplings = {(0,0):C.GC_120})

V_59 = Vertex(name = 'V_59',
              particles = [ P.Vz, P.Z, P.h ],
              color = [ '1' ],
              lorentz = [ L.VVS1 ],
              couplings = {(0,0):C.GC_129})

V_60 = Vertex(name = 'V_60',
              particles = [ P.Vc__minus__, P.Vc__plus__, P.Vz, P.Z ],
              color = [ '1' ],
              lorentz = [ L.VVVV2 ],
              couplings = {(0,0):C.GC_60})

V_61 = Vertex(name = 'V_61',
              particles = [ P.A, P.Vc__plus__, P.W__minus__, P.Z ],
              color = [ '1' ],
              lorentz = [ L.VVVV1, L.VVVV3, L.VVVV5 ],
              couplings = {(0,0):C.GC_112,(0,1):C.GC_111,(0,2):C.GC_102})

V_62 = Vertex(name = 'V_62',
              particles = [ P.Vc__plus__, P.Vz, P.W__minus__, P.Z ],
              color = [ '1' ],
              lorentz = [ L.VVVV1, L.VVVV4, L.VVVV7 ],
              couplings = {(0,0):C.GC_66,(0,2):C.GC_59,(0,1):C.GC_65})

V_63 = Vertex(name = 'V_63',
              particles = [ P.A, P.Vc__minus__, P.W__plus__, P.Z ],
              color = [ '1' ],
              lorentz = [ L.VVVV1, L.VVVV3, L.VVVV5 ],
              couplings = {(0,0):C.GC_112,(0,1):C.GC_111,(0,2):C.GC_102})

V_64 = Vertex(name = 'V_64',
              particles = [ P.Vc__minus__, P.Vz, P.W__plus__, P.Z ],
              color = [ '1' ],
              lorentz = [ L.VVVV1, L.VVVV4, L.VVVV7 ],
              couplings = {(0,0):C.GC_66,(0,2):C.GC_59,(0,1):C.GC_65})

V_65 = Vertex(name = 'V_65',
              particles = [ P.A, P.W__minus__, P.W__plus__, P.Z ],
              color = [ '1' ],
              lorentz = [ L.VVVV5 ],
              couplings = {(0,0):C.GC_110})

V_66 = Vertex(name = 'V_66',
              particles = [ P.Vz, P.W__minus__, P.W__plus__, P.Z ],
              color = [ '1' ],
              lorentz = [ L.VVVV5 ],
              couplings = {(0,0):C.GC_63})

V_67 = Vertex(name = 'V_67',
              particles = [ P.Z, P.Z, P.h, P.h ],
              color = [ '1' ],
              lorentz = [ L.VVSS1 ],
              couplings = {(0,0):C.GC_117})

V_68 = Vertex(name = 'V_68',
              particles = [ P.Z, P.Z, P.h ],
              color = [ '1' ],
              lorentz = [ L.VVS1 ],
              couplings = {(0,0):C.GC_128})

V_69 = Vertex(name = 'V_69',
              particles = [ P.Vc__minus__, P.Vc__plus__, P.Z, P.Z ],
              color = [ '1' ],
              lorentz = [ L.VVVV2 ],
              couplings = {(0,0):C.GC_64})

V_70 = Vertex(name = 'V_70',
              particles = [ P.Vc__plus__, P.W__minus__, P.Z, P.Z ],
              color = [ '1' ],
              lorentz = [ L.VVVV2 ],
              couplings = {(0,0):C.GC_62})

V_71 = Vertex(name = 'V_71',
              particles = [ P.Vc__minus__, P.W__plus__, P.Z, P.Z ],
              color = [ '1' ],
              lorentz = [ L.VVVV2 ],
              couplings = {(0,0):C.GC_62})

V_72 = Vertex(name = 'V_72',
              particles = [ P.W__minus__, P.W__plus__, P.Z, P.Z ],
              color = [ '1' ],
              lorentz = [ L.VVVV2 ],
              couplings = {(0,0):C.GC_61})

V_73 = Vertex(name = 'V_73',
              particles = [ P.b__tilde__, P.b, P.A ],
              color = [ 'Identity(1,2)' ],
              lorentz = [ L.FFV1 ],
              couplings = {(0,0):C.GC_1})

V_74 = Vertex(name = 'V_74',
              particles = [ P.c__tilde__, P.c, P.A ],
              color = [ 'Identity(1,2)' ],
              lorentz = [ L.FFV1 ],
              couplings = {(0,0):C.GC_2})

V_75 = Vertex(name = 'V_75',
              particles = [ P.d__tilde__, P.d, P.A ],
              color = [ 'Identity(1,2)' ],
              lorentz = [ L.FFV1 ],
              couplings = {(0,0):C.GC_1})

V_76 = Vertex(name = 'V_76',
              particles = [ P.s__tilde__, P.s, P.A ],
              color = [ 'Identity(1,2)' ],
              lorentz = [ L.FFV1 ],
              couplings = {(0,0):C.GC_1})

V_77 = Vertex(name = 'V_77',
              particles = [ P.t__tilde__, P.t, P.A ],
              color = [ 'Identity(1,2)' ],
              lorentz = [ L.FFV1 ],
              couplings = {(0,0):C.GC_2})

V_78 = Vertex(name = 'V_78',
              particles = [ P.u__tilde__, P.u, P.A ],
              color = [ 'Identity(1,2)' ],
              lorentz = [ L.FFV1 ],
              couplings = {(0,0):C.GC_2})

V_88 = Vertex(name = 'V_88',
              particles = [ P.b__tilde__, P.b, P.Z ],
              color = [ 'Identity(1,2)' ],
              lorentz = [ L.FFV2, L.FFV3 ],
              couplings = {(0,0):C.GC_75,(0,1):C.GC_91})

V_89 = Vertex(name = 'V_89',
              particles = [ P.c__tilde__, P.c, P.Z ],
              color = [ 'Identity(1,2)' ],
              lorentz = [ L.FFV2, L.FFV5 ],
              couplings = {(0,0):C.GC_79,(0,1):C.GC_91})

V_90 = Vertex(name = 'V_90',
              particles = [ P.d__tilde__, P.d, P.Z ],
              color = [ 'Identity(1,2)' ],
              lorentz = [ L.FFV2, L.FFV3 ],
              couplings = {(0,0):C.GC_77,(0,1):C.GC_91})

V_91 = Vertex(name = 'V_91',
              particles = [ P.s__tilde__, P.s, P.Z ],
              color = [ 'Identity(1,2)' ],
              lorentz = [ L.FFV2, L.FFV3 ],
              couplings = {(0,0):C.GC_77,(0,1):C.GC_91})

V_92 = Vertex(name = 'V_92',
              particles = [ P.t__tilde__, P.t, P.Z ],
              color = [ 'Identity(1,2)' ],
              lorentz = [ L.FFV2, L.FFV5 ],
              couplings = {(0,0):C.GC_78,(0,1):C.GC_91})

V_93 = Vertex(name = 'V_93',
              particles = [ P.u__tilde__, P.u, P.Z ],
              color = [ 'Identity(1,2)' ],
              lorentz = [ L.FFV2, L.FFV5 ],
              couplings = {(0,0):C.GC_79,(0,1):C.GC_91})

V_94 = Vertex(name = 'V_94',
              particles = [ P.e__plus__, P.e__minus__, P.A ],
              color = [ '1' ],
              lorentz = [ L.FFV1 ],
              couplings = {(0,0):C.GC_3})

V_95 = Vertex(name = 'V_95',
              particles = [ P.m__plus__, P.m__minus__, P.A ],
              color = [ '1' ],
              lorentz = [ L.FFV1 ],
              couplings = {(0,0):C.GC_3})

V_96 = Vertex(name = 'V_96',
              particles = [ P.tt__plus__, P.tt__minus__, P.A ],
              color = [ '1' ],
              lorentz = [ L.FFV1 ],
              couplings = {(0,0):C.GC_3})

V_97 = Vertex(name = 'V_97',
              particles = [ P.ve__tilde__, P.ve, P.Z ],
              color = [ '1' ],
              lorentz = [ L.FFV2 ],
              couplings = {(0,0):C.GC_103})

V_98 = Vertex(name = 'V_98',
              particles = [ P.vm__tilde__, P.vm, P.Z ],
              color = [ '1' ],
              lorentz = [ L.FFV2 ],
              couplings = {(0,0):C.GC_103})

V_99 = Vertex(name = 'V_99',
              particles = [ P.vt__tilde__, P.vt, P.Z ],
              color = [ '1' ],
              lorentz = [ L.FFV2 ],
              couplings = {(0,0):C.GC_103})

V_100 = Vertex(name = 'V_100',
               particles = [ P.e__plus__, P.e__minus__, P.Vz ],
               color = [ '1' ],
               lorentz = [ L.FFV2, L.FFV4 ],
               couplings = {(0,0):C.GC_87,(0,1):C.GC_101})

V_101 = Vertex(name = 'V_101',
               particles = [ P.m__plus__, P.m__minus__, P.Vz ],
               color = [ '1' ],
               lorentz = [ L.FFV2, L.FFV4 ],
               couplings = {(0,0):C.GC_87,(0,1):C.GC_101})

V_102 = Vertex(name = 'V_102',
               particles = [ P.tt__plus__, P.tt__minus__, P.Vz ],
               color = [ '1' ],
               lorentz = [ L.FFV2, L.FFV4 ],
               couplings = {(0,0):C.GC_87,(0,1):C.GC_101})

V_108 = Vertex(name = 'V_108',
               particles = [ P.e__plus__, P.e__minus__, P.Z ],
               color = [ '1' ],
               lorentz = [ L.FFV2, L.FFV4 ],
               couplings = {(0,0):C.GC_76,(0,1):C.GC_92})

V_109 = Vertex(name = 'V_109',
               particles = [ P.m__plus__, P.m__minus__, P.Z ],
               color = [ '1' ],
               lorentz = [ L.FFV2, L.FFV4 ],
               couplings = {(0,0):C.GC_76,(0,1):C.GC_92})

V_110 = Vertex(name = 'V_110',
               particles = [ P.tt__plus__, P.tt__minus__, P.Z ],
               color = [ '1' ],
               lorentz = [ L.FFV2, L.FFV4 ],
               couplings = {(0,0):C.GC_76,(0,1):C.GC_92})

V_111 = Vertex(name = 'V_111',
               particles = [ P.t__tilde__, P.b, P.Vc__plus__ ],
               color = [ 'Identity(1,2)' ],
               lorentz = [ L.FFV2 ],
               couplings = {(0,0):C.GC_80})

V_112 = Vertex(name = 'V_112',
               particles = [ P.ve__tilde__, P.e__minus__, P.Vc__plus__ ],
               color = [ '1' ],
               lorentz = [ L.FFV2 ],
               couplings = {(0,0):C.GC_81})

V_113 = Vertex(name = 'V_113',
               particles = [ P.vm__tilde__, P.m__minus__, P.Vc__plus__ ],
               color = [ '1' ],
               lorentz = [ L.FFV2 ],
               couplings = {(0,0):C.GC_81})

V_114 = Vertex(name = 'V_114',
               particles = [ P.vt__tilde__, P.tt__minus__, P.Vc__plus__ ],
               color = [ '1' ],
               lorentz = [ L.FFV2 ],
               couplings = {(0,0):C.GC_81})

V_119 = Vertex(name = 'V_119',
               particles = [ P.d__tilde__, P.c, P.W__minus__ ],
               color = [ 'Identity(1,2)' ],
               lorentz = [ L.FFV2 ],
               couplings = {(0,0):C.GC_73})

V_120 = Vertex(name = 'V_120',
               particles = [ P.s__tilde__, P.c, P.W__minus__ ],
               color = [ 'Identity(1,2)' ],
               lorentz = [ L.FFV2 ],
               couplings = {(0,0):C.GC_74})

V_121 = Vertex(name = 'V_121',
               particles = [ P.b__tilde__, P.t, P.W__minus__ ],
               color = [ 'Identity(1,2)' ],
               lorentz = [ L.FFV2 ],
               couplings = {(0,0):C.GC_69})

V_122 = Vertex(name = 'V_122',
               particles = [ P.d__tilde__, P.u, P.W__minus__ ],
               color = [ 'Identity(1,2)' ],
               lorentz = [ L.FFV2 ],
               couplings = {(0,0):C.GC_71})

V_123 = Vertex(name = 'V_123',
               particles = [ P.s__tilde__, P.u, P.W__minus__ ],
               color = [ 'Identity(1,2)' ],
               lorentz = [ L.FFV2 ],
               couplings = {(0,0):C.GC_72})

V_124 = Vertex(name = 'V_124',
               particles = [ P.t__tilde__, P.b, P.W__plus__ ],
               color = [ 'Identity(1,2)' ],
               lorentz = [ L.FFV2 ],
               couplings = {(0,0):C.GC_69})

V_125 = Vertex(name = 'V_125',
               particles = [ P.ve__tilde__, P.e__minus__, P.W__plus__ ],
               color = [ '1' ],
               lorentz = [ L.FFV2 ],
               couplings = {(0,0):C.GC_70})

V_126 = Vertex(name = 'V_126',
               particles = [ P.vm__tilde__, P.m__minus__, P.W__plus__ ],
               color = [ '1' ],
               lorentz = [ L.FFV2 ],
               couplings = {(0,0):C.GC_70})

V_127 = Vertex(name = 'V_127',
               particles = [ P.vt__tilde__, P.tt__minus__, P.W__plus__ ],
               color = [ '1' ],
               lorentz = [ L.FFV2 ],
               couplings = {(0,0):C.GC_70})

V_128 = Vertex(name = 'V_128',
               particles = [ P.c__tilde__, P.d, P.W__plus__ ],
               color = [ 'Identity(1,2)' ],
               lorentz = [ L.FFV2 ],
               couplings = {(0,0):C.GC_138})

V_129 = Vertex(name = 'V_129',
               particles = [ P.u__tilde__, P.d, P.W__plus__ ],
               color = [ 'Identity(1,2)' ],
               lorentz = [ L.FFV2 ],
               couplings = {(0,0):C.GC_134})

V_130 = Vertex(name = 'V_130',
               particles = [ P.c__tilde__, P.s, P.W__plus__ ],
               color = [ 'Identity(1,2)' ],
               lorentz = [ L.FFV2 ],
               couplings = {(0,0):C.GC_140})

V_131 = Vertex(name = 'V_131',
               particles = [ P.u__tilde__, P.s, P.W__plus__ ],
               color = [ 'Identity(1,2)' ],
               lorentz = [ L.FFV2 ],
               couplings = {(0,0):C.GC_136})

V_132 = Vertex(name = 'V_132',
               particles = [ P.e__plus__, P.ve, P.Vc__minus__ ],
               color = [ '1' ],
               lorentz = [ L.FFV2 ],
               couplings = {(0,0):C.GC_81})

V_133 = Vertex(name = 'V_133',
               particles = [ P.m__plus__, P.vm, P.Vc__minus__ ],
               color = [ '1' ],
               lorentz = [ L.FFV2 ],
               couplings = {(0,0):C.GC_81})

V_134 = Vertex(name = 'V_134',
               particles = [ P.tt__plus__, P.vt, P.Vc__minus__ ],
               color = [ '1' ],
               lorentz = [ L.FFV2 ],
               couplings = {(0,0):C.GC_81})

V_135 = Vertex(name = 'V_135',
               particles = [ P.e__plus__, P.ve, P.W__minus__ ],
               color = [ '1' ],
               lorentz = [ L.FFV2 ],
               couplings = {(0,0):C.GC_70})

V_136 = Vertex(name = 'V_136',
               particles = [ P.m__plus__, P.vm, P.W__minus__ ],
               color = [ '1' ],
               lorentz = [ L.FFV2 ],
               couplings = {(0,0):C.GC_70})

V_137 = Vertex(name = 'V_137',
               particles = [ P.tt__plus__, P.vt, P.W__minus__ ],
               color = [ '1' ],
               lorentz = [ L.FFV2 ],
               couplings = {(0,0):C.GC_70})

V_138 = Vertex(name = 'V_138',
             particles = [ P.A, P.A, P.h ],
             color = [ '1' ],
             lorentz = [ L.VVS2 ],
             couplings = {(0,0):C.GC_142})


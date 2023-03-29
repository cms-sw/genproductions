# This file was automatically created by FeynRules 2.3.22
# Mathematica version: 11.0.0 for Mac OS X x86 (64-bit) (July 28, 2016)
# Date: Tue 14 Aug 2018 21:43:14


from object_library import all_vertices, Vertex
import particles as P
import couplings as C
import lorentz as L


V_1 = Vertex(name = 'V_1',
             particles = [ P.d__tilde__, P.d, P.Zh ],
             color = [ 'Identity(1,2)' ],
             lorentz = [ L.FFV1, L.FFV2 ],
             couplings = {(0,1):C.GC_7,(0,0):C.GC_10})

V_2 = Vertex(name = 'V_2',
             particles = [ P.s__tilde__, P.s, P.Zh ],
             color = [ 'Identity(1,2)' ],
             lorentz = [ L.FFV1, L.FFV2 ],
             couplings = {(0,1):C.GC_7,(0,0):C.GC_10})

V_3 = Vertex(name = 'V_3',
             particles = [ P.b__tilde__, P.b, P.Zh ],
             color = [ 'Identity(1,2)' ],
             lorentz = [ L.FFV1, L.FFV2 ],
             couplings = {(0,1):C.GC_7,(0,0):C.GC_10})

V_4 = Vertex(name = 'V_4',
             particles = [ P.u__tilde__, P.u, P.Zh ],
             color = [ 'Identity(1,2)' ],
             lorentz = [ L.FFV1, L.FFV2 ],
             couplings = {(0,1):C.GC_7,(0,0):C.GC_10})

V_5 = Vertex(name = 'V_5',
             particles = [ P.c__tilde__, P.c, P.Zh ],
             color = [ 'Identity(1,2)' ],
             lorentz = [ L.FFV1, L.FFV2 ],
             couplings = {(0,1):C.GC_7,(0,0):C.GC_10})

V_6 = Vertex(name = 'V_6',
             particles = [ P.t__tilde__, P.t, P.Zh ],
             color = [ 'Identity(1,2)' ],
             lorentz = [ L.FFV1, L.FFV2 ],
             couplings = {(0,1):C.GC_7,(0,0):C.GC_10})

V_7 = Vertex(name = 'V_7',
             particles = [ P.vt__tilde__, P.vt, P.Zh ],
             color = [ '1' ],
             lorentz = [ L.FFV1, L.FFV2 ],
             couplings = {(0,1):C.GC_8,(0,0):C.GC_11})

V_8 = Vertex(name = 'V_8',
             particles = [ P.vt__tilde__, P.vt, P.Zp ],
             color = [ '1' ],
             lorentz = [ L.FFV1 ],
             couplings = {(0,0):C.GC_9})

V_9 = Vertex(name = 'V_9',
             particles = [ P.u__tilde__, P.u, P.Zp ],
             color = [ 'Identity(1,2)' ],
             lorentz = [ L.FFV1 ],
             couplings = {(0,0):C.GC_12})

V_10 = Vertex(name = 'V_10',
              particles = [ P.d__tilde__, P.d, P.G ],
              color = [ 'T(3,2,1)' ],
              lorentz = [ L.FFV1 ],
              couplings = {(0,0):C.GC_5})

V_11 = Vertex(name = 'V_11',
              particles = [ P.s__tilde__, P.s, P.G ],
              color = [ 'T(3,2,1)' ],
              lorentz = [ L.FFV1 ],
              couplings = {(0,0):C.GC_5})

V_12 = Vertex(name = 'V_12',
              particles = [ P.b__tilde__, P.b, P.G ],
              color = [ 'T(3,2,1)' ],
              lorentz = [ L.FFV1 ],
              couplings = {(0,0):C.GC_5})

V_13 = Vertex(name = 'V_13',
              particles = [ P.u__tilde__, P.u, P.G ],
              color = [ 'T(3,2,1)' ],
              lorentz = [ L.FFV1 ],
              couplings = {(0,0):C.GC_5})

V_14 = Vertex(name = 'V_14',
              particles = [ P.c__tilde__, P.c, P.G ],
              color = [ 'T(3,2,1)' ],
              lorentz = [ L.FFV1 ],
              couplings = {(0,0):C.GC_5})

V_15 = Vertex(name = 'V_15',
              particles = [ P.t__tilde__, P.t, P.G ],
              color = [ 'T(3,2,1)' ],
              lorentz = [ L.FFV1 ],
              couplings = {(0,0):C.GC_5})

V_16 = Vertex(name = 'V_16',
              particles = [ P.d__tilde__, P.d, P.A ],
              color = [ 'Identity(1,2)' ],
              lorentz = [ L.FFV1 ],
              couplings = {(0,0):C.GC_1})

V_17 = Vertex(name = 'V_17',
              particles = [ P.s__tilde__, P.s, P.A ],
              color = [ 'Identity(1,2)' ],
              lorentz = [ L.FFV1 ],
              couplings = {(0,0):C.GC_1})

V_18 = Vertex(name = 'V_18',
              particles = [ P.b__tilde__, P.b, P.A ],
              color = [ 'Identity(1,2)' ],
              lorentz = [ L.FFV1 ],
              couplings = {(0,0):C.GC_1})

V_19 = Vertex(name = 'V_19',
              particles = [ P.d__tilde__, P.d, P.Z ],
              color = [ 'Identity(1,2)' ],
              lorentz = [ L.FFV3, L.FFV4 ],
              couplings = {(0,0):C.GC_28,(0,1):C.GC_25})

V_20 = Vertex(name = 'V_20',
              particles = [ P.s__tilde__, P.s, P.Z ],
              color = [ 'Identity(1,2)' ],
              lorentz = [ L.FFV3, L.FFV4 ],
              couplings = {(0,0):C.GC_28,(0,1):C.GC_25})

V_21 = Vertex(name = 'V_21',
              particles = [ P.b__tilde__, P.b, P.Z ],
              color = [ 'Identity(1,2)' ],
              lorentz = [ L.FFV3, L.FFV4 ],
              couplings = {(0,0):C.GC_28,(0,1):C.GC_25})

V_22 = Vertex(name = 'V_22',
              particles = [ P.e__plus__, P.e__minus__, P.A ],
              color = [ '1' ],
              lorentz = [ L.FFV1 ],
              couplings = {(0,0):C.GC_3})

V_23 = Vertex(name = 'V_23',
              particles = [ P.m__plus__, P.m__minus__, P.A ],
              color = [ '1' ],
              lorentz = [ L.FFV1 ],
              couplings = {(0,0):C.GC_3})

V_24 = Vertex(name = 'V_24',
              particles = [ P.tt__plus__, P.tt__minus__, P.A ],
              color = [ '1' ],
              lorentz = [ L.FFV1 ],
              couplings = {(0,0):C.GC_3})

V_25 = Vertex(name = 'V_25',
              particles = [ P.e__plus__, P.e__minus__, P.Z ],
              color = [ '1' ],
              lorentz = [ L.FFV3, L.FFV4 ],
              couplings = {(0,0):C.GC_30,(0,1):C.GC_27})

V_26 = Vertex(name = 'V_26',
              particles = [ P.m__plus__, P.m__minus__, P.Z ],
              color = [ '1' ],
              lorentz = [ L.FFV3, L.FFV4 ],
              couplings = {(0,0):C.GC_30,(0,1):C.GC_27})

V_27 = Vertex(name = 'V_27',
              particles = [ P.tt__plus__, P.tt__minus__, P.Z ],
              color = [ '1' ],
              lorentz = [ L.FFV3, L.FFV4 ],
              couplings = {(0,0):C.GC_30,(0,1):C.GC_27})

V_28 = Vertex(name = 'V_28',
              particles = [ P.u__tilde__, P.u, P.A ],
              color = [ 'Identity(1,2)' ],
              lorentz = [ L.FFV1 ],
              couplings = {(0,0):C.GC_2})

V_29 = Vertex(name = 'V_29',
              particles = [ P.c__tilde__, P.c, P.A ],
              color = [ 'Identity(1,2)' ],
              lorentz = [ L.FFV1 ],
              couplings = {(0,0):C.GC_2})

V_30 = Vertex(name = 'V_30',
              particles = [ P.t__tilde__, P.t, P.A ],
              color = [ 'Identity(1,2)' ],
              lorentz = [ L.FFV1 ],
              couplings = {(0,0):C.GC_2})

V_31 = Vertex(name = 'V_31',
              particles = [ P.u__tilde__, P.u, P.Z ],
              color = [ 'Identity(1,2)' ],
              lorentz = [ L.FFV3, L.FFV4 ],
              couplings = {(0,0):C.GC_29,(0,1):C.GC_26})

V_32 = Vertex(name = 'V_32',
              particles = [ P.c__tilde__, P.c, P.Z ],
              color = [ 'Identity(1,2)' ],
              lorentz = [ L.FFV3, L.FFV4 ],
              couplings = {(0,0):C.GC_29,(0,1):C.GC_26})

V_33 = Vertex(name = 'V_33',
              particles = [ P.t__tilde__, P.t, P.Z ],
              color = [ 'Identity(1,2)' ],
              lorentz = [ L.FFV3, L.FFV5 ],
              couplings = {(0,0):C.GC_23,(0,1):C.GC_24})

V_34 = Vertex(name = 'V_34',
              particles = [ P.ve__tilde__, P.ve, P.Z ],
              color = [ '1' ],
              lorentz = [ L.FFV3 ],
              couplings = {(0,0):C.GC_31})

V_35 = Vertex(name = 'V_35',
              particles = [ P.vm__tilde__, P.vm, P.Z ],
              color = [ '1' ],
              lorentz = [ L.FFV3 ],
              couplings = {(0,0):C.GC_31})

V_36 = Vertex(name = 'V_36',
              particles = [ P.vt__tilde__, P.vt, P.Z ],
              color = [ '1' ],
              lorentz = [ L.FFV3 ],
              couplings = {(0,0):C.GC_31})

V_37 = Vertex(name = 'V_37',
              particles = [ P.e__plus__, P.ve, P.W__minus__ ],
              color = [ '1' ],
              lorentz = [ L.FFV3 ],
              couplings = {(0,0):C.GC_13})

V_38 = Vertex(name = 'V_38',
              particles = [ P.m__plus__, P.vm, P.W__minus__ ],
              color = [ '1' ],
              lorentz = [ L.FFV3 ],
              couplings = {(0,0):C.GC_13})

V_39 = Vertex(name = 'V_39',
              particles = [ P.tt__plus__, P.vt, P.W__minus__ ],
              color = [ '1' ],
              lorentz = [ L.FFV3 ],
              couplings = {(0,0):C.GC_13})

V_40 = Vertex(name = 'V_40',
              particles = [ P.d__tilde__, P.u, P.W__minus__ ],
              color = [ 'Identity(1,2)' ],
              lorentz = [ L.FFV3 ],
              couplings = {(0,0):C.GC_14})

V_41 = Vertex(name = 'V_41',
              particles = [ P.s__tilde__, P.u, P.W__minus__ ],
              color = [ 'Identity(1,2)' ],
              lorentz = [ L.FFV3 ],
              couplings = {(0,0):C.GC_15})

V_42 = Vertex(name = 'V_42',
              particles = [ P.b__tilde__, P.u, P.W__minus__ ],
              color = [ 'Identity(1,2)' ],
              lorentz = [ L.FFV3 ],
              couplings = {(0,0):C.GC_16})

V_43 = Vertex(name = 'V_43',
              particles = [ P.d__tilde__, P.c, P.W__minus__ ],
              color = [ 'Identity(1,2)' ],
              lorentz = [ L.FFV3 ],
              couplings = {(0,0):C.GC_17})

V_44 = Vertex(name = 'V_44',
              particles = [ P.s__tilde__, P.c, P.W__minus__ ],
              color = [ 'Identity(1,2)' ],
              lorentz = [ L.FFV3 ],
              couplings = {(0,0):C.GC_18})

V_45 = Vertex(name = 'V_45',
              particles = [ P.b__tilde__, P.c, P.W__minus__ ],
              color = [ 'Identity(1,2)' ],
              lorentz = [ L.FFV3 ],
              couplings = {(0,0):C.GC_19})

V_46 = Vertex(name = 'V_46',
              particles = [ P.d__tilde__, P.t, P.W__minus__ ],
              color = [ 'Identity(1,2)' ],
              lorentz = [ L.FFV3 ],
              couplings = {(0,0):C.GC_20})

V_47 = Vertex(name = 'V_47',
              particles = [ P.s__tilde__, P.t, P.W__minus__ ],
              color = [ 'Identity(1,2)' ],
              lorentz = [ L.FFV3 ],
              couplings = {(0,0):C.GC_21})

V_48 = Vertex(name = 'V_48',
              particles = [ P.b__tilde__, P.t, P.W__minus__ ],
              color = [ 'Identity(1,2)' ],
              lorentz = [ L.FFV3 ],
              couplings = {(0,0):C.GC_22})

V_49 = Vertex(name = 'V_49',
              particles = [ P.ve__tilde__, P.e__minus__, P.W__plus__ ],
              color = [ '1' ],
              lorentz = [ L.FFV3 ],
              couplings = {(0,0):C.GC_13})

V_50 = Vertex(name = 'V_50',
              particles = [ P.vm__tilde__, P.m__minus__, P.W__plus__ ],
              color = [ '1' ],
              lorentz = [ L.FFV3 ],
              couplings = {(0,0):C.GC_13})

V_51 = Vertex(name = 'V_51',
              particles = [ P.vt__tilde__, P.tt__minus__, P.W__plus__ ],
              color = [ '1' ],
              lorentz = [ L.FFV3 ],
              couplings = {(0,0):C.GC_13})

V_52 = Vertex(name = 'V_52',
              particles = [ P.u__tilde__, P.d, P.W__plus__ ],
              color = [ 'Identity(1,2)' ],
              lorentz = [ L.FFV3 ],
              couplings = {(0,0):C.GC_32})

V_53 = Vertex(name = 'V_53',
              particles = [ P.c__tilde__, P.d, P.W__plus__ ],
              color = [ 'Identity(1,2)' ],
              lorentz = [ L.FFV3 ],
              couplings = {(0,0):C.GC_35})

V_54 = Vertex(name = 'V_54',
              particles = [ P.t__tilde__, P.d, P.W__plus__ ],
              color = [ 'Identity(1,2)' ],
              lorentz = [ L.FFV3 ],
              couplings = {(0,0):C.GC_38})

V_55 = Vertex(name = 'V_55',
              particles = [ P.u__tilde__, P.s, P.W__plus__ ],
              color = [ 'Identity(1,2)' ],
              lorentz = [ L.FFV3 ],
              couplings = {(0,0):C.GC_33})

V_56 = Vertex(name = 'V_56',
              particles = [ P.c__tilde__, P.s, P.W__plus__ ],
              color = [ 'Identity(1,2)' ],
              lorentz = [ L.FFV3 ],
              couplings = {(0,0):C.GC_36})

V_57 = Vertex(name = 'V_57',
              particles = [ P.t__tilde__, P.s, P.W__plus__ ],
              color = [ 'Identity(1,2)' ],
              lorentz = [ L.FFV3 ],
              couplings = {(0,0):C.GC_39})

V_58 = Vertex(name = 'V_58',
              particles = [ P.u__tilde__, P.b, P.W__plus__ ],
              color = [ 'Identity(1,2)' ],
              lorentz = [ L.FFV3 ],
              couplings = {(0,0):C.GC_34})

V_59 = Vertex(name = 'V_59',
              particles = [ P.c__tilde__, P.b, P.W__plus__ ],
              color = [ 'Identity(1,2)' ],
              lorentz = [ L.FFV3 ],
              couplings = {(0,0):C.GC_37})

V_60 = Vertex(name = 'V_60',
              particles = [ P.t__tilde__, P.b, P.W__plus__ ],
              color = [ 'Identity(1,2)' ],
              lorentz = [ L.FFV3 ],
              couplings = {(0,0):C.GC_40})

V_61 = Vertex(name = 'V_61',
              particles = [ P.G, P.G, P.G ],
              color = [ 'f(1,2,3)' ],
              lorentz = [ L.VVV1 ],
              couplings = {(0,0):C.GC_4})

V_62 = Vertex(name = 'V_62',
              particles = [ P.G, P.G, P.G, P.G ],
              color = [ 'f(-1,1,2)*f(3,4,-1)', 'f(-1,1,3)*f(2,4,-1)', 'f(-1,1,4)*f(2,3,-1)' ],
              lorentz = [ L.VVVV1, L.VVVV2, L.VVVV3 ],
              couplings = {(1,1):C.GC_6,(0,0):C.GC_6,(2,2):C.GC_6})


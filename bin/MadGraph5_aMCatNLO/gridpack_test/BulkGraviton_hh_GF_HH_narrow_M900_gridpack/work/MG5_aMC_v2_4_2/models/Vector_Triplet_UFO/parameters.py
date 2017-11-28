# This file was automatically created by FeynRules 1.7.178
# Mathematica version: 9.0 for Mac OS X x86 (64-bit) (January 24, 2013)
# Date: Thu 17 Jul 2014 17:34:00



from object_library import all_parameters, Parameter


from function_library import complexconjugate, re, im, csc, sec, acsc, asec, cot

# This is a default parameter object representing 0.
ZERO = Parameter(name = 'ZERO',
                 nature = 'internal',
                 type = 'real',
                 value = '0.0',
                 texname = '0')

# User-defined parameters.
cpsi = Parameter(name = 'cpsi',
                 nature = 'external',
                 type = 'real',
                 value = 1.,
                 texname = '\\text{cpsi}',
                 lhablock = 'CHINPUTS',
                 lhacode = [ 1 ])

aa = Parameter(name = 'aa',
               nature = 'external',
               type = 'real',
               value = 1.,
               texname = 'a',
               lhablock = 'CHINPUTS',
               lhacode = [ 2 ])

bb = Parameter(name = 'bb',
               nature = 'external',
               type = 'real',
               value = 1.,
               texname = 'b',
               lhablock = 'CHINPUTS',
               lhacode = [ 3 ])

d3 = Parameter(name = 'd3',
               nature = 'external',
               type = 'real',
               value = 1.,
               texname = '\\text{d3}',
               lhablock = 'CHINPUTS',
               lhacode = [ 4 ])

d4 = Parameter(name = 'd4',
               nature = 'external',
               type = 'real',
               value = 1.,
               texname = '\\text{d4}',
               lhablock = 'CHINPUTS',
               lhacode = [ 5 ])

cabi = Parameter(name = 'cabi',
                 nature = 'external',
                 type = 'real',
                 value = 0.22759,
                 texname = '\\theta _{\\text{cab}}',
                 lhablock = 'CKMBLOCK',
                 lhacode = [ 1 ])

gst = Parameter(name = 'gst',
                nature = 'external',
                type = 'real',
                value = 2,
                texname = 'g_{\\text{st}}',
                lhablock = 'RHOINPUTS',
                lhacode = [ 1 ])

MVz = Parameter(name = 'MVz',
                nature = 'external',
                type = 'real',
                value = 2000,
                texname = 'M_{V^0}',
                lhablock = 'RHOINPUTS',
                lhacode = [ 2 ])

cvvw = Parameter(name = 'cvvw',
                 nature = 'external',
                 type = 'real',
                 value = 1,
                 texname = '\\text{cvvw}',
                 lhablock = 'RHOINPUTS',
                 lhacode = [ 3 ])

cq = Parameter(name = 'cq',
               nature = 'external',
               type = 'real',
               value = 1,
               texname = '\\text{cq}',
               lhablock = 'RHOINPUTS',
               lhacode = [ 4 ])

cl = Parameter(name = 'cl',
               nature = 'external',
               type = 'real',
               value = 1,
               texname = '\\text{cl}',
               lhablock = 'RHOINPUTS',
               lhacode = [ 5 ])

c3 = Parameter(name = 'c3',
               nature = 'external',
               type = 'real',
               value = 1,
               texname = '\\text{c3}',
               lhablock = 'RHOINPUTS',
               lhacode = [ 6 ])

ch = Parameter(name = 'ch',
               nature = 'external',
               type = 'real',
               value = 1,
               texname = '\\text{ch}',
               lhablock = 'RHOINPUTS',
               lhacode = [ 7 ])

cvvhh = Parameter(name = 'cvvhh',
                  nature = 'external',
                  type = 'real',
                  value = 0,
                  texname = '\\text{cvvhh}',
                  lhablock = 'RHOINPUTS',
                  lhacode = [ 8 ])

cvvv = Parameter(name = 'cvvv',
                 nature = 'external',
                 type = 'real',
                 value = 1,
                 texname = '\\text{cvvv}',
                 lhablock = 'RHOINPUTS',
                 lhacode = [ 9 ])

cvvvv = Parameter(name = 'cvvvv',
                  nature = 'external',
                  type = 'real',
                  value = 1,
                  texname = '\\text{cvvvv}',
                  lhablock = 'RHOINPUTS',
                  lhacode = [ 10 ])

aEWM1 = Parameter(name = 'aEWM1',
                  nature = 'external',
                  type = 'real',
                  value = 127.9,
                  texname = '\\alpha _{\\text{EW}}{}^{-1}',
                  lhablock = 'SMINPUTS',
                  lhacode = [ 1 ])

Gf = Parameter(name = 'Gf',
               nature = 'external',
               type = 'real',
               value = 0.00001166378,
               texname = 'G_f',
               lhablock = 'SMINPUTS',
               lhacode = [ 2 ])

aS = Parameter(name = 'aS',
               nature = 'external',
               type = 'real',
               value = 0.118,
               texname = '\\text{aS}',
               lhablock = 'SMINPUTS',
               lhacode = [ 3 ])

ymc = Parameter(name = 'ymc',
                nature = 'external',
                type = 'real',
                value = 0,
                texname = '\\text{ymc}',
                lhablock = 'YUKAWA',
                lhacode = [ 4 ])

ymb = Parameter(name = 'ymb',
                nature = 'external',
                type = 'real',
                value = 4.8,
                texname = '\\text{ymb}',
                lhablock = 'YUKAWA',
                lhacode = [ 5 ])

ymt = Parameter(name = 'ymt',
                nature = 'external',
                type = 'real',
                value = 172.5,
                texname = '\\text{ymt}',
                lhablock = 'YUKAWA',
                lhacode = [ 6 ])

ymtau = Parameter(name = 'ymtau',
                  nature = 'external',
                  type = 'real',
                  value = 1.777,
                  texname = '\\text{ymtau}',
                  lhablock = 'YUKAWA',
                  lhacode = [ 15 ])

MTA = Parameter(name = 'MTA',
                nature = 'external',
                type = 'real',
                value = 1.777,
                texname = '\\text{MTA}',
                lhablock = 'MASS',
                lhacode = [ 15 ])

MC = Parameter(name = 'MC',
               nature = 'external',
               type = 'real',
               value = 0,
               texname = '\\text{MC}',
               lhablock = 'MASS',
               lhacode = [ 4 ])

MT = Parameter(name = 'MT',
               nature = 'external',
               type = 'real',
               value = 172.5,
               texname = '\\text{MT}',
               lhablock = 'MASS',
               lhacode = [ 6 ])

MB = Parameter(name = 'MB',
               nature = 'external',
               type = 'real',
               value = 4.8,
               texname = '\\text{MB}',
               lhablock = 'MASS',
               lhacode = [ 5 ])

MH = Parameter(name = 'MH',
               nature = 'external',
               type = 'real',
               value = 125,
               texname = '\\text{MH}',
               lhablock = 'MASS',
               lhacode = [ 25 ])

MZ = Parameter(name = 'MZ',
               nature = 'external',
               type = 'real',
               value = 91.1876,
               texname = '\\text{MZ}',
               lhablock = 'MASS',
               lhacode = [ 23 ])

WT = Parameter(name = 'WT',
               nature = 'external',
               type = 'real',
               value = 1.491500,
               texname = '\\text{WT}',
               lhablock = 'DECAY',
               lhacode = [ 6 ])

WH = Parameter(name = 'WH',
               nature = 'external',
               type = 'real',
               value = 0.006382339,
               texname = '\\text{WH}',
               lhablock = 'DECAY',
               lhacode = [ 25 ])

WZ = Parameter(name = 'WZ',
               nature = 'external',
               type = 'real',
               value = 2.4952,
               texname = '\\text{WZ}',
               lhablock = 'DECAY',
               lhacode = [ 23 ])

WW = Parameter(name = 'WW',
               nature = 'external',
               type = 'real',
               value = 2.085,
               texname = '\\text{WW}',
               lhablock = 'DECAY',
               lhacode = [ 24 ])

aEW = Parameter(name = 'aEW',
                nature = 'internal',
                type = 'real',
                value = '1/aEWM1',
                texname = '\\alpha _{\\text{EW}}')

G = Parameter(name = 'G',
              nature = 'internal',
              type = 'real',
              value = '2*cmath.sqrt(aS)*cmath.sqrt(cmath.pi)',
              texname = 'G')

CKM1x1 = Parameter(name = 'CKM1x1',
                   nature = 'internal',
                   type = 'complex',
                   value = 'cmath.cos(cabi)',
                   texname = '\\text{CKM1x1}')

CKM1x2 = Parameter(name = 'CKM1x2',
                   nature = 'internal',
                   type = 'complex',
                   value = 'cmath.sin(cabi)',
                   texname = '\\text{CKM1x2}')

CKM2x1 = Parameter(name = 'CKM2x1',
                   nature = 'internal',
                   type = 'complex',
                   value = '-cmath.sin(cabi)',
                   texname = '\\text{CKM2x1}')

CKM2x2 = Parameter(name = 'CKM2x2',
                   nature = 'internal',
                   type = 'complex',
                   value = 'cmath.cos(cabi)',
                   texname = '\\text{CKM2x2}')

ee = Parameter(name = 'ee',
               nature = 'internal',
               type = 'real',
               value = '2*cmath.sqrt(aEW)*cmath.sqrt(cmath.pi)',
               texname = 'e')

gw = Parameter(name = 'gw',
               nature = 'internal',
               type = 'real',
               value = 'cmath.sqrt(2)*cmath.sqrt((gst**4*MVz**4*(-8*aEW*cmath.pi*Gf**3*MZ**6 + 8*aEW**2*cmath.pi**2*Gf**2*MZ**4*cmath.sqrt(2) + Gf**4*MZ**8*cmath.sqrt(2) + 8*aEW**2*cmath.pi**2*Gf*cmath.sqrt(2)*(Gf**2*MZ**12*(8*aEW**2*cmath.pi**2 + Gf**2*MZ**4 - 4*aEW*cmath.pi*Gf*MZ**2*cmath.sqrt(2)))**0.25 - 8*aEW*cmath.pi*cmath.sqrt(Gf**5*MZ**10*(Gf*MZ**2 - 2*aEW*cmath.pi*cmath.sqrt(2))) + cmath.sqrt(2)*cmath.sqrt(Gf**7*MZ**14*(Gf*MZ**2 - 2*aEW*cmath.pi*cmath.sqrt(2)))) + 4*ch**2*cl**2*gst**4*(-8*aEW**3*cmath.pi**3*Gf*MZ**6 - 9*aEW*cmath.pi*Gf**3*MZ**10 + 12*aEW**2*cmath.pi**2*Gf**2*MZ**8*cmath.sqrt(2) + Gf**4*MZ**12*cmath.sqrt(2) - aEW*cmath.pi*(aEW*cmath.pi*(aEW*cmath.pi*MZ**2*(Gf**2*MZ**12*(8*aEW**2*cmath.pi**2 + Gf**2*MZ**4 - 4*aEW*cmath.pi*Gf*MZ**2*cmath.sqrt(2)))**0.25 - 6*cmath.sqrt(2)*cmath.sqrt(Gf**3*MZ**14*(Gf*MZ**2 - 2*aEW*cmath.pi*cmath.sqrt(2)))) + 7*cmath.sqrt(Gf**5*MZ**18*(Gf*MZ**2 - 2*aEW*cmath.pi*cmath.sqrt(2)))) + cmath.sqrt(2)*cmath.sqrt(Gf**7*MZ**22*(Gf*MZ**2 - 2*aEW*cmath.pi*cmath.sqrt(2)))) - 8*cl**2*gst**2*MVz**2*(16*aEW**2*cmath.pi**2*Gf**3*MZ**8 + Gf**5*MZ**12 - 8*aEW**3*cmath.pi**3*Gf**2*MZ**6*cmath.sqrt(2) - 5*aEW*cmath.pi*Gf**4*MZ**10*cmath.sqrt(2) + aEW*cmath.pi*(aEW*cmath.pi*(-2*aEW*cmath.pi*cmath.sqrt(2)*cmath.sqrt(Gf**3*MZ**10*(Gf*MZ**2 - 2*aEW*cmath.pi*cmath.sqrt(2))) + 9*cmath.sqrt(Gf**5*MZ**14*(Gf*MZ**2 - 2*aEW*cmath.pi*cmath.sqrt(2)))) - 4*cmath.sqrt(2)*cmath.sqrt(Gf**7*MZ**18*(Gf*MZ**2 - 2*aEW*cmath.pi*cmath.sqrt(2)))) + cmath.sqrt(Gf**9*MZ**22*(Gf*MZ**2 - 2*aEW*cmath.pi*cmath.sqrt(2)))) - 2*ch*cl*gst**2*(-(gst**2*MVz**2*(-8*aEW**3*cmath.pi**3*Gf*MZ**4 - 9*aEW*cmath.pi*Gf**3*MZ**8 + 12*aEW**2*cmath.pi**2*Gf**2*MZ**6*cmath.sqrt(2) + Gf**4*MZ**10*cmath.sqrt(2) + aEW*cmath.pi*(6*aEW*cmath.pi*cmath.sqrt(2)*cmath.sqrt(Gf**3*MZ**10*(Gf*MZ**2 - 2*aEW*cmath.pi*cmath.sqrt(2))) - 7*cmath.sqrt(Gf**5*MZ**14*(Gf*MZ**2 - 2*aEW*cmath.pi*cmath.sqrt(2)))) + cmath.sqrt(2)*cmath.sqrt(Gf**7*MZ**18*(Gf*MZ**2 - 2*aEW*cmath.pi*cmath.sqrt(2))))) + 4*cl**2*(8*aEW**4*cmath.pi**4*Gf*MZ**6 + 97*aEW**2*cmath.pi**2*Gf**3*MZ**10 + 6*Gf**5*MZ**14 - 52*aEW**3*cmath.pi**3*Gf**2*MZ**8*cmath.sqrt(2) - 30*aEW*cmath.pi*Gf**4*MZ**12*cmath.sqrt(2) + aEW*cmath.pi*(5*aEW*cmath.pi*(-3*aEW*cmath.pi*cmath.sqrt(2)*cmath.sqrt(Gf**3*MZ**14*(Gf*MZ**2 - 2*aEW*cmath.pi*cmath.sqrt(2))) + 11*cmath.sqrt(Gf**5*MZ**18*(Gf*MZ**2 - 2*aEW*cmath.pi*cmath.sqrt(2)))) - 24*cmath.sqrt(2)*cmath.sqrt(Gf**7*MZ**22*(Gf*MZ**2 - 2*aEW*cmath.pi*cmath.sqrt(2)))) + 6*cmath.sqrt(Gf**9*MZ**26*(Gf*MZ**2 - 2*aEW*cmath.pi*cmath.sqrt(2))))) + 16*cl**4*(-120*aEW**3*cmath.pi**3*Gf**3*MZ**10 - 44*aEW*cmath.pi*Gf**5*MZ**14 + 24*aEW**4*cmath.pi**4*Gf**2*MZ**8*cmath.sqrt(2) + 83*aEW**2*cmath.pi**2*Gf**4*MZ**12*cmath.sqrt(2) + 4*Gf**6*MZ**16*cmath.sqrt(2) + aEW*cmath.pi*(aEW*cmath.pi*(aEW*cmath.pi*(3*aEW*cmath.pi*cmath.sqrt(2)*cmath.sqrt(Gf**3*MZ**14*(Gf*MZ**2 - 2*aEW*cmath.pi*cmath.sqrt(2))) - 46*cmath.sqrt(Gf**5*MZ**18*(Gf*MZ**2 - 2*aEW*cmath.pi*cmath.sqrt(2)))) + 51*cmath.sqrt(2)*cmath.sqrt(Gf**7*MZ**22*(Gf*MZ**2 - 2*aEW*cmath.pi*cmath.sqrt(2)))) - 36*cmath.sqrt(Gf**9*MZ**26*(Gf*MZ**2 - 2*aEW*cmath.pi*cmath.sqrt(2)))) + 4*cmath.sqrt(2)*cmath.sqrt(Gf**11*MZ**30*(Gf*MZ**2 - 2*aEW*cmath.pi*cmath.sqrt(2)))))/(Gf*gst**4*MVz**4*MZ**2*(Gf*MZ**2 - 2*aEW*cmath.pi*cmath.sqrt(2))**2))',
               texname = 'g_w')

mV = Parameter(name = 'mV',
               nature = 'internal',
               type = 'real',
               value = 'cmath.sqrt((-4*aEW*cmath.pi*gw**8*(MVz**2 + MZ**2) + gw**10*(MVz**2 + MZ**2) + 2*ch**2*gst**2*gw**4*(-4*aEW*cmath.pi + gw**2)**2*(MVz**2 + MZ**2) + gw**4*cmath.sqrt(gw**4*(-4*aEW*cmath.pi + gw**2)**2*(16*aEW*ch**2*cmath.pi*gst**2*MVz**2*MZ**2 - 4*ch**2*gst**2*gw**2*MVz**2*MZ**2 + gw**4*(MVz**2 - MZ**2)**2)) + 4*cvvhh*gst**2*(-4*aEW*cmath.pi + gw**2)*(-(gw**4*(-4*aEW*cmath.pi + gw**2)*(MVz**2 + MZ**2)) + cmath.sqrt(gw**4*(-4*aEW*cmath.pi + gw**2)**2*(16*aEW*ch**2*cmath.pi*gst**2*MVz**2*MZ**2 - 4*ch**2*gst**2*gw**2*MVz**2*MZ**2 + gw**4*(MVz**2 - MZ**2)**2))))/(gw**4*(-4*aEW*cmath.pi + gw**2)*(gw**4 + ch**2*gst**2*(-4*aEW*cmath.pi + gw**2))))/cmath.sqrt(2)',
               texname = 'm_V')

swt = Parameter(name = 'swt',
                nature = 'internal',
                type = 'real',
                value = 'ee/gw',
                texname = '\\tilde{s}_w')

vv = Parameter(name = 'vv',
               nature = 'internal',
               type = 'real',
               value = 'cmath.sqrt((2*gw**4*(-4*aEW*cmath.pi + gw**2)*(MVz**2 + MZ**2) - 2*cmath.sqrt(gw**4*(-4*aEW*cmath.pi + gw**2)**2*(16*aEW*ch**2*cmath.pi*gst**2*MVz**2*MZ**2 - 4*ch**2*gst**2*gw**2*MVz**2*MZ**2 + gw**4*(MVz**2 - MZ**2)**2)))/(gw**4*(gw**4 + ch**2*gst**2*(-4*aEW*cmath.pi + gw**2))))',
               texname = '\\tilde{v}')

cwt = Parameter(name = 'cwt',
                nature = 'internal',
                type = 'real',
                value = 'cmath.sqrt(1 - swt**2)',
                texname = '\\tilde{c}_w')

MWt = Parameter(name = 'MWt',
                nature = 'internal',
                type = 'real',
                value = '(gw*vv)/2.',
                texname = '\\tilde{M}_W')

yb = Parameter(name = 'yb',
               nature = 'internal',
               type = 'real',
               value = '(ymb*cmath.sqrt(2))/vv',
               texname = '\\text{yb}')

yc = Parameter(name = 'yc',
               nature = 'internal',
               type = 'real',
               value = '(ymc*cmath.sqrt(2))/vv',
               texname = '\\text{yc}')

yt = Parameter(name = 'yt',
               nature = 'internal',
               type = 'real',
               value = '(ymt*cmath.sqrt(2))/vv',
               texname = '\\text{yt}')

ytau = Parameter(name = 'ytau',
                 nature = 'internal',
                 type = 'real',
                 value = '(ymtau*cmath.sqrt(2))/vv',
                 texname = '\\text{ytau}')

MVc = Parameter(name = 'MVc',
                nature = 'internal',
                type = 'real',
                value = 'cmath.sqrt(16*mV**2 + 16*MWt**2 + 16*cvvhh*gst**2*vv**2 + cmath.sqrt((-16*mV**2 - 16*MWt**2 - 16*cvvhh*gst**2*vv**2)**2 - 64*(16*mV**2*MWt**2 + 16*cvvhh*gst**2*MWt**2*vv**2 - ch**2*gst**2*gw**2*vv**4)))/(4.*cmath.sqrt(2))',
                texname = 'M_{V^+}')

MW = Parameter(name = 'MW',
               nature = 'internal',
               type = 'real',
               value = 'cmath.sqrt(16*mV**2 + 16*MWt**2 + 16*cvvhh*gst**2*vv**2 - cmath.sqrt((-16*mV**2 - 16*MWt**2 - 16*cvvhh*gst**2*vv**2)**2 - 64*(16*mV**2*MWt**2 + 16*cvvhh*gst**2*MWt**2*vv**2 - ch**2*gst**2*gw**2*vv**4)))/(4.*cmath.sqrt(2))',
               texname = 'M_W')

MZt = Parameter(name = 'MZt',
                nature = 'internal',
                type = 'real',
                value = 'MWt/cwt',
                texname = '\\tilde{M}_Z')

thC = Parameter(name = 'thC',
                nature = 'internal',
                type = 'real',
                value = 'cmath.atan((ch*gst*gw*vv**2)/(2.*(mV**2 - MWt**2 + cvvhh*gst**2*vv**2)))/2.',
                texname = '\\theta _C')

twt = Parameter(name = 'twt',
                nature = 'internal',
                type = 'real',
                value = 'swt/cwt',
                texname = '\\tilde{t}_w')

gz = Parameter(name = 'gz',
               nature = 'internal',
               type = 'real',
               value = 'gw/cwt',
               texname = 'g_z')

cC = Parameter(name = 'cC',
               nature = 'internal',
               type = 'real',
               value = 'cmath.cos(thC)',
               texname = 'c_C')

sC = Parameter(name = 'sC',
               nature = 'internal',
               type = 'real',
               value = 'cmath.sin(thC)',
               texname = 's_C')

g1 = Parameter(name = 'g1',
               nature = 'internal',
               type = 'real',
               value = 'gw*twt',
               texname = 'g_1')

thN = Parameter(name = 'thN',
                nature = 'internal',
                type = 'real',
                value = 'cmath.atan((ch*gst*gz*vv**2)/(2.*(mV**2 - MZt**2 + cvvhh*gst**2*vv**2)))/2.',
                texname = '\\theta _N')

cN = Parameter(name = 'cN',
               nature = 'internal',
               type = 'real',
               value = 'cmath.cos(thN)',
               texname = 'c_N')

sN = Parameter(name = 'sN',
               nature = 'internal',
               type = 'real',
               value = 'cmath.sin(thN)',
               texname = 's_N')

WVc = Parameter(name = 'WVc',
                nature = 'internal',
                type = 'real',
                value = '((MVc**4 + (MW**2 - MZ**2)**2 - 2*MVc**2*(MW**2 + MZ**2))**1.5*(cC**4*gw**2*(MVc**4 + 2*MW**2*MZ**2 + 6*cvvw*MW**2*MZ**2 + MZ**4 + 2*MVc**2*((1 + 3*cvvw + cvvw**2)*MW**2 + 5*MZ**2) + cvvw**2*(MW**4 + 2*MW**2*MZ**2))*sN**2 + gw**2*(MW**4 + 10*MW**2*MZ**2 + MZ**4 + 2*MVc**2*(MW**2 + MZ**2) + 6*cvvw*MVc**2*(MW**2 + MZ**2) + cvvw**2*MVc**2*(MVc**2 + 2*(MW**2 + MZ**2)))*sC**4*sN**2 + 2*cC**3*gw*sC*sN*(cN*(-1 + cvvw)*cwt*gw*MZ**2*(5*MVc**2 + (2 + 3*cvvw)*MW**2 + MZ**2) + cvvv*gst*(MVc**4 + 5*MW**2*MZ**2 + MZ**4 + 5*MVc**2*((1 + cvvw)*MW**2 + 2*MZ**2) + cvvw*(MW**4 + 5*MW**2*MZ**2))*sN) - 2*cC*gw*sC**3*sN*(cN*(-1 + cvvw)*cwt*gw*MZ**2*((2 + 3*cvvw)*MVc**2 + 5*MW**2 + MZ**2) + cvvv*gst*(MW**4 + 10*MW**2*MZ**2 + MZ**4 + 5*MVc**2*(MW**2 + MZ**2) + cvvw*MVc**2*(MVc**2 + 5*(MW**2 + MZ**2)))*sN) + cC**2*sC**2*(cN**2*(-1 + cvvw)**2*cwt**2*gw**2*MZ**2*(2*MVc**2 + 2*MW**2 + MZ**2) + 2*cN*cvvv*(-1 + cvvw)*cwt*gst*gw*MZ**2*(5*MVc**2 + 5*MW**2 + MZ**2)*sN + (cvvv**2*gst**2*(MVc**4 + MW**4 + 10*MW**2*MZ**2 + MZ**4 + 10*MVc**2*(MW**2 + MZ**2)) - 2*gw**2*(3*cvvw**2*MVc**2*MW**2 + 5*MW**2*MZ**2 + MZ**4 + MVc**2*(3*MW**2 + 5*MZ**2) + cvvw*(MVc**4 + MW**4 + 5*MW**2*MZ**2 + MVc**2*(4*MW**2 + 5*MZ**2))))*sN**2)))/(192.*cmath.pi*MVc**5*MW**2*MZ**2) + (MVc*(ee*gst*sC + cC*cl*gw**2*swt)**2)/(16.*cmath.pi*gst**2*swt**2) + (CKM1x1**2*MVc*(ee*gst*sC + cC*cq*gw**2*swt)**2)/(8.*cmath.pi*gst**2*swt**2) + (CKM1x2**2*MVc*(ee*gst*sC + cC*cq*gw**2*swt)**2)/(8.*cmath.pi*gst**2*swt**2) - ((MB**4 + MT**4 + MT**2*MVc**2 - 2*MVc**4 + MB**2*(-2*MT**2 + MVc**2))*(ee*gst*sC + c3*cC*gw**2*swt)**2*cmath.sqrt(MB**4 + (MT**2 - MVc**2)**2 - 2*MB**2*(MT**2 + MVc**2)))/(32.*cmath.pi*gst**2*MVc**5*swt**2) + ((MH**4 + MVc**4 + 10*MVc**2*MW**2 + MW**4 - 2*MH**2*(MVc**2 + MW**2))*(4*aa*cC*MWt**2*sC + gst*(cC**2*ch*gw - 4*cC*cvvhh*gst*sC - ch*gw*sC**2)*vv**2)**2*cmath.sqrt(MH**4 + (MVc**2 - MW**2)**2 - 2*MH**2*(MVc**2 + MW**2)))/(768.*cmath.pi*MVc**5*MW**2*vv**2)',
                texname = '\\gamma _{V^+}')

WVz = Parameter(name = 'WVz',
                nature = 'internal',
                type = 'real',
                value = '(MVz*(cwt**2*ee*gst*sN + cl*cN*cwt*gw**2*swt + ee*gst*sN*swt**2)**2)/(32.*cwt**2*cmath.pi*gst**2*swt**2) + (MVz*(cwt**4*ee**2*gst**2*sN**2 + 2*cl*cN*cwt**3*ee*gst*gw**2*sN*swt + cwt**2*(cl**2*cN**2*gw**4 - 2*ee**2*gst**2*sN**2)*swt**2 - 2*cl*cN*cwt*ee*gst*gw**2*sN*swt**3 + 5*ee**2*gst**2*sN**2*swt**4))/(32.*cwt**2*cmath.pi*gst**2*swt**2) + (MVz*(9*cwt**4*ee**2*gst**2*sN**2 + 18*cN*cq*cwt**3*ee*gst*gw**2*sN*swt + 3*cwt**2*(3*cN**2*cq**2*gw**4 + 2*ee**2*gst**2*sN**2)*swt**2 + 6*cN*cq*cwt*ee*gst*gw**2*sN*swt**3 + 5*ee**2*gst**2*sN**2*swt**4))/(144.*cwt**2*cmath.pi*gst**2*swt**2) + (MVz*(9*cwt**4*ee**2*gst**2*sN**2 + 18*cN*cq*cwt**3*ee*gst*gw**2*sN*swt + 3*cwt**2*(3*cN**2*cq**2*gw**4 - 2*ee**2*gst**2*sN**2)*swt**2 - 6*cN*cq*cwt*ee*gst*gw**2*sN*swt**3 + 17*ee**2*gst**2*sN**2*swt**4))/(144.*cwt**2*cmath.pi*gst**2*swt**2) + ((9*cwt**4*ee**2*gst**2*(-MB**2 + MVz**2)*sN**2 - 18*c3*cN*cwt**3*ee*gst*gw**2*(MB**2 - MVz**2)*sN*swt + 3*cwt**2*(3*c3**2*cN**2*gw**4*(-MB**2 + MVz**2) + 2*ee**2*gst**2*(-7*MB**2 + MVz**2)*sN**2)*swt**2 + 6*c3*cN*cwt*ee*gst*gw**2*(-7*MB**2 + MVz**2)*sN*swt**3 + ee**2*gst**2*(-17*MB**2 + 5*MVz**2)*sN**2*swt**4)*cmath.sqrt(-4*MB**2*MVz**2 + MVz**4))/(288.*cwt**2*cmath.pi*gst**2*MVz**3*swt**2) + ((9*cwt**4*ee**2*gst**2*(-MT**2 + MVz**2)*sN**2 - 18*c3*cN*cwt**3*ee*gst*gw**2*(MT**2 - MVz**2)*sN*swt - 3*cwt**2*(3*c3**2*cN**2*gw**4*(MT**2 - MVz**2) + 2*ee**2*gst**2*(11*MT**2 + MVz**2)*sN**2)*swt**2 - 6*c3*cN*cwt*ee*gst*gw**2*(11*MT**2 + MVz**2)*sN*swt**3 + ee**2*gst**2*(7*MT**2 + 17*MVz**2)*sN**2*swt**4)*cmath.sqrt(-4*MT**2*MVz**2 + MVz**4))/(288.*cwt**2*cmath.pi*gst**2*MVz**3*swt**2) + ((MVz - 2*MW)*(MVz + 2*MW)*(-4*cC**3*cN*cwt*gw**2*(MVz**4 + 5*(3 + cvvw)*MVz**2*MW**2 + 6*(1 + cvvw)*MW**4)*sC*sN + cC**4*cwt**2*gw**2*(MVz**4 + 20*MVz**2*MW**2 + 12*MW**4)*sN**2 + 4*cC*cN*gw*sC**3*(cN*cvvv*gst*(MVz**4 + 5*(3 + cvvw)*MVz**2*MW**2 + 6*(1 + cvvw)*MW**4) - cwt*gw*(8*MVz**2*MW**2 + 3*cvvw**2*MVz**2*MW**2 + 6*MW**4 + cvvw*(MVz**4 + 9*MVz**2*MW**2 + 6*MW**4))*sN) + 2*cC**2*gw*sC**2*(2*cN**2*gw*(MVz**4 + (11 + 8*cvvw + cvvw**2)*MVz**2*MW**2 + 3*(1 + cvvw)**2*MW**4) - cN*cvvv*cwt*gst*(MVz**4 + 20*MVz**2*MW**2 + 12*MW**4)*sN + cwt**2*gw*(cvvw*MVz**4 + 10*MVz**2*MW**2 + 10*cvvw*MVz**2*MW**2 + 12*MW**4)*sN**2) + sC**4*(cN**2*cvvv**2*gst**2*(MVz**4 + 20*MVz**2*MW**2 + 12*MW**4) - 2*cN*cvvv*cwt*gst*gw*(cvvw*MVz**4 + 10*MVz**2*MW**2 + 10*cvvw*MVz**2*MW**2 + 12*MW**4)*sN + cwt**2*gw**2*(12*cvvw*MVz**2*MW**2 + 4*MW**2*(MVz**2 + 3*MW**2) + cvvw**2*(MVz**4 + 4*MVz**2*MW**2))*sN**2))*cmath.sqrt(MVz**4 - 4*MVz**2*MW**2))/(192.*cmath.pi*MVz**3*MW**4) + ((MH**4 + MVz**4 + 10*MVz**2*MZ**2 + MZ**4 - 2*MH**2*(MVz**2 + MZ**2))*(4*aa*cN*MZt**2*sN + gst*(-4*cN*cvvhh*gst*sN + ch*gz*(cN**2 - sN**2))*vv**2)**2*cmath.sqrt(MH**4 + (MVz**2 - MZ**2)**2 - 2*MH**2*(MVz**2 + MZ**2)))/(768.*cmath.pi*MVz**5*MZ**2*vv**2)',
                texname = '\\gamma _{V^0}')


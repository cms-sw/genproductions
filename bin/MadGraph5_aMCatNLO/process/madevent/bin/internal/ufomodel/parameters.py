# This file was automatically created by FeynRules 2.4.62
# Mathematica version: 10.4.1 for Mac OS X x86 (64-bit) (April 11, 2016)
# Date: Fri 26 Jan 2018 23:46:39



from object_library import all_parameters, Parameter


from function_library import complexconjugate, re, im, csc, sec, acsc, asec, cot

# This is a default parameter object representing 0.
ZERO = Parameter(name = 'ZERO',
                 nature = 'internal',
                 type = 'real',
                 value = '0.0',
                 texname = '0')

# User-defined parameters.
cabi = Parameter(name = 'cabi',
                 nature = 'external',
                 type = 'real',
                 value = 0.227736,
                 texname = '\\theta _c',
                 lhablock = 'CKMBLOCK',
                 lhacode = [ 1 ])

CLl1x1 = Parameter(name = 'CLl1x1',
                   nature = 'external',
                   type = 'real',
                   value = 1.,
                   texname = '\\text{CLl1x1}',
                   lhablock = 'CLl',
                   lhacode = [ 1, 1 ])

CLl1x2 = Parameter(name = 'CLl1x2',
                   nature = 'external',
                   type = 'real',
                   value = 0.,
                   texname = '\\text{CLl1x2}',
                   lhablock = 'CLl',
                   lhacode = [ 1, 2 ])

CLl1x3 = Parameter(name = 'CLl1x3',
                   nature = 'external',
                   type = 'real',
                   value = 0.,
                   texname = '\\text{CLl1x3}',
                   lhablock = 'CLl',
                   lhacode = [ 1, 3 ])

CLl2x1 = Parameter(name = 'CLl2x1',
                   nature = 'external',
                   type = 'real',
                   value = 0.,
                   texname = '\\text{CLl2x1}',
                   lhablock = 'CLl',
                   lhacode = [ 2, 1 ])

CLl2x2 = Parameter(name = 'CLl2x2',
                   nature = 'external',
                   type = 'real',
                   value = 1.,
                   texname = '\\text{CLl2x2}',
                   lhablock = 'CLl',
                   lhacode = [ 2, 2 ])

CLl2x3 = Parameter(name = 'CLl2x3',
                   nature = 'external',
                   type = 'real',
                   value = 0.,
                   texname = '\\text{CLl2x3}',
                   lhablock = 'CLl',
                   lhacode = [ 2, 3 ])

CLl3x1 = Parameter(name = 'CLl3x1',
                   nature = 'external',
                   type = 'real',
                   value = 0.,
                   texname = '\\text{CLl3x1}',
                   lhablock = 'CLl',
                   lhacode = [ 3, 1 ])

CLl3x2 = Parameter(name = 'CLl3x2',
                   nature = 'external',
                   type = 'real',
                   value = 0.,
                   texname = '\\text{CLl3x2}',
                   lhablock = 'CLl',
                   lhacode = [ 3, 2 ])

CLl3x3 = Parameter(name = 'CLl3x3',
                   nature = 'external',
                   type = 'real',
                   value = 1.,
                   texname = '\\text{CLl3x3}',
                   lhablock = 'CLl',
                   lhacode = [ 3, 3 ])

CLq1x1 = Parameter(name = 'CLq1x1',
                   nature = 'external',
                   type = 'real',
                   value = 0.97418,
                   texname = '\\text{CLq1x1}',
                   lhablock = 'CLq',
                   lhacode = [ 1, 1 ])

CLq1x2 = Parameter(name = 'CLq1x2',
                   nature = 'external',
                   type = 'real',
                   value = 0.225773,
                   texname = '\\text{CLq1x2}',
                   lhablock = 'CLq',
                   lhacode = [ 1, 2 ])

CLq1x3 = Parameter(name = 'CLq1x3',
                   nature = 'external',
                   type = 'real',
                   value = 0.,
                   texname = '\\text{CLq1x3}',
                   lhablock = 'CLq',
                   lhacode = [ 1, 3 ])

CLq2x1 = Parameter(name = 'CLq2x1',
                   nature = 'external',
                   type = 'real',
                   value = -0.225773,
                   texname = '\\text{CLq2x1}',
                   lhablock = 'CLq',
                   lhacode = [ 2, 1 ])

CLq2x2 = Parameter(name = 'CLq2x2',
                   nature = 'external',
                   type = 'real',
                   value = 0.97418,
                   texname = '\\text{CLq2x2}',
                   lhablock = 'CLq',
                   lhacode = [ 2, 2 ])

CLq2x3 = Parameter(name = 'CLq2x3',
                   nature = 'external',
                   type = 'real',
                   value = 0.,
                   texname = '\\text{CLq2x3}',
                   lhablock = 'CLq',
                   lhacode = [ 2, 3 ])

CLq3x1 = Parameter(name = 'CLq3x1',
                   nature = 'external',
                   type = 'real',
                   value = 0.,
                   texname = '\\text{CLq3x1}',
                   lhablock = 'CLq',
                   lhacode = [ 3, 1 ])

CLq3x2 = Parameter(name = 'CLq3x2',
                   nature = 'external',
                   type = 'real',
                   value = 0.,
                   texname = '\\text{CLq3x2}',
                   lhablock = 'CLq',
                   lhacode = [ 3, 2 ])

CLq3x3 = Parameter(name = 'CLq3x3',
                   nature = 'external',
                   type = 'real',
                   value = 1.,
                   texname = '\\text{CLq3x3}',
                   lhablock = 'CLq',
                   lhacode = [ 3, 3 ])

CRl1x1 = Parameter(name = 'CRl1x1',
                   nature = 'external',
                   type = 'real',
                   value = 1.,
                   texname = '\\text{CRl1x1}',
                   lhablock = 'CRl',
                   lhacode = [ 1, 1 ])

CRl1x2 = Parameter(name = 'CRl1x2',
                   nature = 'external',
                   type = 'real',
                   value = 0.,
                   texname = '\\text{CRl1x2}',
                   lhablock = 'CRl',
                   lhacode = [ 1, 2 ])

CRl1x3 = Parameter(name = 'CRl1x3',
                   nature = 'external',
                   type = 'real',
                   value = 0.,
                   texname = '\\text{CRl1x3}',
                   lhablock = 'CRl',
                   lhacode = [ 1, 3 ])

CRl2x1 = Parameter(name = 'CRl2x1',
                   nature = 'external',
                   type = 'real',
                   value = 0.,
                   texname = '\\text{CRl2x1}',
                   lhablock = 'CRl',
                   lhacode = [ 2, 1 ])

CRl2x2 = Parameter(name = 'CRl2x2',
                   nature = 'external',
                   type = 'real',
                   value = 1.,
                   texname = '\\text{CRl2x2}',
                   lhablock = 'CRl',
                   lhacode = [ 2, 2 ])

CRl2x3 = Parameter(name = 'CRl2x3',
                   nature = 'external',
                   type = 'real',
                   value = 0.,
                   texname = '\\text{CRl2x3}',
                   lhablock = 'CRl',
                   lhacode = [ 2, 3 ])

CRl3x1 = Parameter(name = 'CRl3x1',
                   nature = 'external',
                   type = 'real',
                   value = 0.,
                   texname = '\\text{CRl3x1}',
                   lhablock = 'CRl',
                   lhacode = [ 3, 1 ])

CRl3x2 = Parameter(name = 'CRl3x2',
                   nature = 'external',
                   type = 'real',
                   value = 0.,
                   texname = '\\text{CRl3x2}',
                   lhablock = 'CRl',
                   lhacode = [ 3, 2 ])

CRl3x3 = Parameter(name = 'CRl3x3',
                   nature = 'external',
                   type = 'real',
                   value = 1.,
                   texname = '\\text{CRl3x3}',
                   lhablock = 'CRl',
                   lhacode = [ 3, 3 ])

CRq1x1 = Parameter(name = 'CRq1x1',
                   nature = 'external',
                   type = 'real',
                   value = 1.,
                   texname = '\\text{CRq1x1}',
                   lhablock = 'CRq',
                   lhacode = [ 1, 1 ])

CRq1x2 = Parameter(name = 'CRq1x2',
                   nature = 'external',
                   type = 'real',
                   value = 0.225773,
                   texname = '\\text{CRq1x2}',
                   lhablock = 'CRq',
                   lhacode = [ 1, 2 ])

CRq1x3 = Parameter(name = 'CRq1x3',
                   nature = 'external',
                   type = 'real',
                   value = 0.,
                   texname = '\\text{CRq1x3}',
                   lhablock = 'CRq',
                   lhacode = [ 1, 3 ])

CRq2x1 = Parameter(name = 'CRq2x1',
                   nature = 'external',
                   type = 'real',
                   value = 0,
                   texname = '\\text{CRq2x1}',
                   lhablock = 'CRq',
                   lhacode = [ 2, 1 ])

CRq2x2 = Parameter(name = 'CRq2x2',
                   nature = 'external',
                   type = 'real',
                   value = 0.97418,
                   texname = '\\text{CRq2x2}',
                   lhablock = 'CRq',
                   lhacode = [ 2, 2 ])

CRq2x3 = Parameter(name = 'CRq2x3',
                   nature = 'external',
                   type = 'real',
                   value = 0.,
                   texname = '\\text{CRq2x3}',
                   lhablock = 'CRq',
                   lhacode = [ 2, 3 ])

CRq3x1 = Parameter(name = 'CRq3x1',
                   nature = 'external',
                   type = 'real',
                   value = 0,
                   texname = '\\text{CRq3x1}',
                   lhablock = 'CRq',
                   lhacode = [ 3, 1 ])

CRq3x2 = Parameter(name = 'CRq3x2',
                   nature = 'external',
                   type = 'real',
                   value = 0.,
                   texname = '\\text{CRq3x2}',
                   lhablock = 'CRq',
                   lhacode = [ 3, 2 ])

CRq3x3 = Parameter(name = 'CRq3x3',
                   nature = 'external',
                   type = 'real',
                   value = 1.,
                   texname = '\\text{CRq3x3}',
                   lhablock = 'CRq',
                   lhacode = [ 3, 3 ])

aEWM1 = Parameter(name = 'aEWM1',
                  nature = 'external',
                  type = 'real',
                  value = 127.9,
                  texname = '\\text{aEWM1}',
                  lhablock = 'SMINPUTS',
                  lhacode = [ 1 ])

Gf = Parameter(name = 'Gf',
               nature = 'external',
               type = 'real',
               value = 0.0000116637,
               texname = 'G_f',
               lhablock = 'SMINPUTS',
               lhacode = [ 2 ])

aS = Parameter(name = 'aS',
               nature = 'external',
               type = 'real',
               value = 0.1184,
               texname = '\\alpha _s',
               lhablock = 'SMINPUTS',
               lhacode = [ 3 ])

kL = Parameter(name = 'kL',
               nature = 'external',
               type = 'real',
               value = 0.,
               texname = '\\kappa _L',
               lhablock = 'WPCOUP',
               lhacode = [ 1 ])

kR = Parameter(name = 'kR',
               nature = 'external',
               type = 'real',
               value = 1.,
               texname = '\\kappa _R',
               lhablock = 'WPCOUP',
               lhacode = [ 2 ])

ymdo = Parameter(name = 'ymdo',
                 nature = 'external',
                 type = 'real',
                 value = 0.00504,
                 texname = '\\text{ymdo}',
                 lhablock = 'YUKAWA',
                 lhacode = [ 1 ])

ymup = Parameter(name = 'ymup',
                 nature = 'external',
                 type = 'real',
                 value = 0.00255,
                 texname = '\\text{ymup}',
                 lhablock = 'YUKAWA',
                 lhacode = [ 2 ])

yms = Parameter(name = 'yms',
                nature = 'external',
                type = 'real',
                value = 0.101,
                texname = '\\text{yms}',
                lhablock = 'YUKAWA',
                lhacode = [ 3 ])

ymc = Parameter(name = 'ymc',
                nature = 'external',
                type = 'real',
                value = 1.27,
                texname = '\\text{ymc}',
                lhablock = 'YUKAWA',
                lhacode = [ 4 ])

ymb = Parameter(name = 'ymb',
                nature = 'external',
                type = 'real',
                value = 4.7,
                texname = '\\text{ymb}',
                lhablock = 'YUKAWA',
                lhacode = [ 5 ])

ymt = Parameter(name = 'ymt',
                nature = 'external',
                type = 'real',
                value = 172,
                texname = '\\text{ymt}',
                lhablock = 'YUKAWA',
                lhacode = [ 6 ])

yme = Parameter(name = 'yme',
                nature = 'external',
                type = 'real',
                value = 0.000511,
                texname = '\\text{yme}',
                lhablock = 'YUKAWA',
                lhacode = [ 11 ])

ymm = Parameter(name = 'ymm',
                nature = 'external',
                type = 'real',
                value = 0.10566,
                texname = '\\text{ymm}',
                lhablock = 'YUKAWA',
                lhacode = [ 13 ])

ymtau = Parameter(name = 'ymtau',
                  nature = 'external',
                  type = 'real',
                  value = 1.777,
                  texname = '\\text{ymtau}',
                  lhablock = 'YUKAWA',
                  lhacode = [ 15 ])

MZ = Parameter(name = 'MZ',
               nature = 'external',
               type = 'real',
               value = 91.1876,
               texname = '\\text{MZ}',
               lhablock = 'MASS',
               lhacode = [ 23 ])

Me = Parameter(name = 'Me',
               nature = 'external',
               type = 'real',
               value = 0.000511,
               texname = '\\text{Me}',
               lhablock = 'MASS',
               lhacode = [ 11 ])

MMU = Parameter(name = 'MMU',
                nature = 'external',
                type = 'real',
                value = 0.10566,
                texname = '\\text{MMU}',
                lhablock = 'MASS',
                lhacode = [ 13 ])

MTA = Parameter(name = 'MTA',
                nature = 'external',
                type = 'real',
                value = 1.777,
                texname = '\\text{MTA}',
                lhablock = 'MASS',
                lhacode = [ 15 ])

MU = Parameter(name = 'MU',
               nature = 'external',
               type = 'real',
               value = 0.00255,
               texname = 'M',
               lhablock = 'MASS',
               lhacode = [ 2 ])

MC = Parameter(name = 'MC',
               nature = 'external',
               type = 'real',
               value = 1.27,
               texname = '\\text{MC}',
               lhablock = 'MASS',
               lhacode = [ 4 ])

MT = Parameter(name = 'MT',
               nature = 'external',
               type = 'real',
               value = 172,
               texname = '\\text{MT}',
               lhablock = 'MASS',
               lhacode = [ 6 ])

MD = Parameter(name = 'MD',
               nature = 'external',
               type = 'real',
               value = 0.00504,
               texname = '\\text{MD}',
               lhablock = 'MASS',
               lhacode = [ 1 ])

MS = Parameter(name = 'MS',
               nature = 'external',
               type = 'real',
               value = 0.101,
               texname = '\\text{MS}',
               lhablock = 'MASS',
               lhacode = [ 3 ])

MB = Parameter(name = 'MB',
               nature = 'external',
               type = 'real',
               value = 4.7,
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

MWp = Parameter(name = 'MWp',
                nature = 'external',
                type = 'real',
                value = 1000.,
                texname = '\\text{MWp}',
                lhablock = 'MASS',
                lhacode = [ 34 ])

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

WT = Parameter(name = 'WT',
               nature = 'external',
               type = 'real',
               value = 1.50833649,
               texname = '\\text{WT}',
               lhablock = 'DECAY',
               lhacode = [ 6 ])

WH = Parameter(name = 'WH',
               nature = 'external',
               type = 'real',
               value = 0.00407,
               texname = '\\text{WH}',
               lhablock = 'DECAY',
               lhacode = [ 25 ])

WWp = Parameter(name = 'WWp',
                nature = 'external',
                type = 'real',
                value = 10.,
                texname = '\\text{WWp}',
                lhablock = 'DECAY',
                lhacode = [ 34 ])

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

CKM1x3 = Parameter(name = 'CKM1x3',
                   nature = 'internal',
                   type = 'complex',
                   value = '0',
                   texname = '\\text{CKM1x3}')

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

CKM2x3 = Parameter(name = 'CKM2x3',
                   nature = 'internal',
                   type = 'complex',
                   value = '0',
                   texname = '\\text{CKM2x3}')

CKM3x1 = Parameter(name = 'CKM3x1',
                   nature = 'internal',
                   type = 'complex',
                   value = '0',
                   texname = '\\text{CKM3x1}')

CKM3x2 = Parameter(name = 'CKM3x2',
                   nature = 'internal',
                   type = 'complex',
                   value = '0',
                   texname = '\\text{CKM3x2}')

CKM3x3 = Parameter(name = 'CKM3x3',
                   nature = 'internal',
                   type = 'complex',
                   value = '1',
                   texname = '\\text{CKM3x3}')

MW = Parameter(name = 'MW',
               nature = 'internal',
               type = 'real',
               value = 'cmath.sqrt(MZ**2/2. + cmath.sqrt(MZ**4/4. - (aEW*cmath.pi*MZ**2)/(Gf*cmath.sqrt(2))))',
               texname = 'M_W')

ee = Parameter(name = 'ee',
               nature = 'internal',
               type = 'real',
               value = '2*cmath.sqrt(aEW)*cmath.sqrt(cmath.pi)',
               texname = 'e')

sw2 = Parameter(name = 'sw2',
                nature = 'internal',
                type = 'real',
                value = '1 - MW**2/MZ**2',
                texname = '\\text{sw2}')

cw = Parameter(name = 'cw',
               nature = 'internal',
               type = 'real',
               value = 'cmath.sqrt(1 - sw2)',
               texname = 'c_w')

sw = Parameter(name = 'sw',
               nature = 'internal',
               type = 'real',
               value = 'cmath.sqrt(sw2)',
               texname = 's_w')

g1 = Parameter(name = 'g1',
               nature = 'internal',
               type = 'real',
               value = 'ee/cw',
               texname = 'g_1')

gL = Parameter(name = 'gL',
               nature = 'internal',
               type = 'real',
               value = '(ee*kL)/sw',
               texname = 'g_L')

gR = Parameter(name = 'gR',
               nature = 'internal',
               type = 'real',
               value = '(ee*kR)/sw',
               texname = 'g_R')

gw = Parameter(name = 'gw',
               nature = 'internal',
               type = 'real',
               value = 'ee/sw',
               texname = 'g_w')

vev = Parameter(name = 'vev',
                nature = 'internal',
                type = 'real',
                value = '(2*MW*sw)/ee',
                texname = '\\text{vev}')

lam = Parameter(name = 'lam',
                nature = 'internal',
                type = 'real',
                value = 'MH**2/(2.*vev**2)',
                texname = '\\text{lam}')

yb = Parameter(name = 'yb',
               nature = 'internal',
               type = 'real',
               value = '(ymb*cmath.sqrt(2))/vev',
               texname = '\\text{yb}')

yc = Parameter(name = 'yc',
               nature = 'internal',
               type = 'real',
               value = '(ymc*cmath.sqrt(2))/vev',
               texname = '\\text{yc}')

ydo = Parameter(name = 'ydo',
                nature = 'internal',
                type = 'real',
                value = '(ymdo*cmath.sqrt(2))/vev',
                texname = '\\text{ydo}')

ye = Parameter(name = 'ye',
               nature = 'internal',
               type = 'real',
               value = '(yme*cmath.sqrt(2))/vev',
               texname = '\\text{ye}')

ym = Parameter(name = 'ym',
               nature = 'internal',
               type = 'real',
               value = '(ymm*cmath.sqrt(2))/vev',
               texname = '\\text{ym}')

ys = Parameter(name = 'ys',
               nature = 'internal',
               type = 'real',
               value = '(yms*cmath.sqrt(2))/vev',
               texname = '\\text{ys}')

yt = Parameter(name = 'yt',
               nature = 'internal',
               type = 'real',
               value = '(ymt*cmath.sqrt(2))/vev',
               texname = '\\text{yt}')

ytau = Parameter(name = 'ytau',
                 nature = 'internal',
                 type = 'real',
                 value = '(ymtau*cmath.sqrt(2))/vev',
                 texname = '\\text{ytau}')

yup = Parameter(name = 'yup',
                nature = 'internal',
                type = 'real',
                value = '(ymup*cmath.sqrt(2))/vev',
                texname = '\\text{yup}')

muH = Parameter(name = 'muH',
                nature = 'internal',
                type = 'real',
                value = 'cmath.sqrt(lam*vev**2)',
                texname = '\\mu')

I1a11 = Parameter(name = 'I1a11',
                  nature = 'internal',
                  type = 'complex',
                  value = 'ydo*complexconjugate(CKM1x1)',
                  texname = '\\text{I1a11}')

I1a12 = Parameter(name = 'I1a12',
                  nature = 'internal',
                  type = 'complex',
                  value = 'ydo*complexconjugate(CKM2x1)',
                  texname = '\\text{I1a12}')

I1a13 = Parameter(name = 'I1a13',
                  nature = 'internal',
                  type = 'complex',
                  value = 'ydo*complexconjugate(CKM3x1)',
                  texname = '\\text{I1a13}')

I1a21 = Parameter(name = 'I1a21',
                  nature = 'internal',
                  type = 'complex',
                  value = 'ys*complexconjugate(CKM1x2)',
                  texname = '\\text{I1a21}')

I1a22 = Parameter(name = 'I1a22',
                  nature = 'internal',
                  type = 'complex',
                  value = 'ys*complexconjugate(CKM2x2)',
                  texname = '\\text{I1a22}')

I1a23 = Parameter(name = 'I1a23',
                  nature = 'internal',
                  type = 'complex',
                  value = 'ys*complexconjugate(CKM3x2)',
                  texname = '\\text{I1a23}')

I1a31 = Parameter(name = 'I1a31',
                  nature = 'internal',
                  type = 'complex',
                  value = 'yb*complexconjugate(CKM1x3)',
                  texname = '\\text{I1a31}')

I1a32 = Parameter(name = 'I1a32',
                  nature = 'internal',
                  type = 'complex',
                  value = 'yb*complexconjugate(CKM2x3)',
                  texname = '\\text{I1a32}')

I1a33 = Parameter(name = 'I1a33',
                  nature = 'internal',
                  type = 'complex',
                  value = 'yb*complexconjugate(CKM3x3)',
                  texname = '\\text{I1a33}')

I2a11 = Parameter(name = 'I2a11',
                  nature = 'internal',
                  type = 'complex',
                  value = 'yup*complexconjugate(CKM1x1)',
                  texname = '\\text{I2a11}')

I2a12 = Parameter(name = 'I2a12',
                  nature = 'internal',
                  type = 'complex',
                  value = 'yc*complexconjugate(CKM2x1)',
                  texname = '\\text{I2a12}')

I2a13 = Parameter(name = 'I2a13',
                  nature = 'internal',
                  type = 'complex',
                  value = 'yt*complexconjugate(CKM3x1)',
                  texname = '\\text{I2a13}')

I2a21 = Parameter(name = 'I2a21',
                  nature = 'internal',
                  type = 'complex',
                  value = 'yup*complexconjugate(CKM1x2)',
                  texname = '\\text{I2a21}')

I2a22 = Parameter(name = 'I2a22',
                  nature = 'internal',
                  type = 'complex',
                  value = 'yc*complexconjugate(CKM2x2)',
                  texname = '\\text{I2a22}')

I2a23 = Parameter(name = 'I2a23',
                  nature = 'internal',
                  type = 'complex',
                  value = 'yt*complexconjugate(CKM3x2)',
                  texname = '\\text{I2a23}')

I2a31 = Parameter(name = 'I2a31',
                  nature = 'internal',
                  type = 'complex',
                  value = 'yup*complexconjugate(CKM1x3)',
                  texname = '\\text{I2a31}')

I2a32 = Parameter(name = 'I2a32',
                  nature = 'internal',
                  type = 'complex',
                  value = 'yc*complexconjugate(CKM2x3)',
                  texname = '\\text{I2a32}')

I2a33 = Parameter(name = 'I2a33',
                  nature = 'internal',
                  type = 'complex',
                  value = 'yt*complexconjugate(CKM3x3)',
                  texname = '\\text{I2a33}')

I3a11 = Parameter(name = 'I3a11',
                  nature = 'internal',
                  type = 'complex',
                  value = 'CKM1x1*yup',
                  texname = '\\text{I3a11}')

I3a12 = Parameter(name = 'I3a12',
                  nature = 'internal',
                  type = 'complex',
                  value = 'CKM1x2*yup',
                  texname = '\\text{I3a12}')

I3a13 = Parameter(name = 'I3a13',
                  nature = 'internal',
                  type = 'complex',
                  value = 'CKM1x3*yup',
                  texname = '\\text{I3a13}')

I3a21 = Parameter(name = 'I3a21',
                  nature = 'internal',
                  type = 'complex',
                  value = 'CKM2x1*yc',
                  texname = '\\text{I3a21}')

I3a22 = Parameter(name = 'I3a22',
                  nature = 'internal',
                  type = 'complex',
                  value = 'CKM2x2*yc',
                  texname = '\\text{I3a22}')

I3a23 = Parameter(name = 'I3a23',
                  nature = 'internal',
                  type = 'complex',
                  value = 'CKM2x3*yc',
                  texname = '\\text{I3a23}')

I3a31 = Parameter(name = 'I3a31',
                  nature = 'internal',
                  type = 'complex',
                  value = 'CKM3x1*yt',
                  texname = '\\text{I3a31}')

I3a32 = Parameter(name = 'I3a32',
                  nature = 'internal',
                  type = 'complex',
                  value = 'CKM3x2*yt',
                  texname = '\\text{I3a32}')

I3a33 = Parameter(name = 'I3a33',
                  nature = 'internal',
                  type = 'complex',
                  value = 'CKM3x3*yt',
                  texname = '\\text{I3a33}')

I4a11 = Parameter(name = 'I4a11',
                  nature = 'internal',
                  type = 'complex',
                  value = 'CKM1x1*ydo',
                  texname = '\\text{I4a11}')

I4a12 = Parameter(name = 'I4a12',
                  nature = 'internal',
                  type = 'complex',
                  value = 'CKM1x2*ys',
                  texname = '\\text{I4a12}')

I4a13 = Parameter(name = 'I4a13',
                  nature = 'internal',
                  type = 'complex',
                  value = 'CKM1x3*yb',
                  texname = '\\text{I4a13}')

I4a21 = Parameter(name = 'I4a21',
                  nature = 'internal',
                  type = 'complex',
                  value = 'CKM2x1*ydo',
                  texname = '\\text{I4a21}')

I4a22 = Parameter(name = 'I4a22',
                  nature = 'internal',
                  type = 'complex',
                  value = 'CKM2x2*ys',
                  texname = '\\text{I4a22}')

I4a23 = Parameter(name = 'I4a23',
                  nature = 'internal',
                  type = 'complex',
                  value = 'CKM2x3*yb',
                  texname = '\\text{I4a23}')

I4a31 = Parameter(name = 'I4a31',
                  nature = 'internal',
                  type = 'complex',
                  value = 'CKM3x1*ydo',
                  texname = '\\text{I4a31}')

I4a32 = Parameter(name = 'I4a32',
                  nature = 'internal',
                  type = 'complex',
                  value = 'CKM3x2*ys',
                  texname = '\\text{I4a32}')

I4a33 = Parameter(name = 'I4a33',
                  nature = 'internal',
                  type = 'complex',
                  value = 'CKM3x3*yb',
                  texname = '\\text{I4a33}')


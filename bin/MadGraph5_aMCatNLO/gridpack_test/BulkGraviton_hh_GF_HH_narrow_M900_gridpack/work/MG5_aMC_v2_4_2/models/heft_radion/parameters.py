# This file was automatically created by FeynRules $Revision: 634 $
# Mathematica version: 8.0 for Mac OS X x86 (64-bit) (November 6, 2010)
# Date: Wed 20 Jul 2011 12:58:03



from object_library import all_parameters, Parameter


from function_library import complexconjugate, re, im, csc, sec, acsc, asec

# This is a default parameter object representing 0.
ZERO = Parameter(name = 'ZERO',
                 nature = 'internal',
                 type = 'real',
                 value = '0.0',
                 texname = '0')

# User-defined parameters.
LR = Parameter(name = 'LR',
                  nature = 'external',
                  type = 'real',
                  value = 1000,
                  texname = '\\text{LR}',
                  lhablock = 'Mass',
                  lhacode = [ 9000002 ])

xi = Parameter(name = 'xi',
                  nature = 'external',
                  type = 'real',
                  value = 0.00000001,
                  texname = '\\text{xi}',
                  lhablock = 'Mass',
                  lhacode = [ 9000004 ])
#declare -a MR=(300 700 1000 2000 3000) 
#declare -a ImFmeio=(0 1.40622 1.1555 0.69043 0.239419) 
#declare -a ReFmeio=(-1.67995 -1.81233 -0.146203 0.142379 0.122819) 
ReFmeio = Parameter(name = 'ReFmeio',
                  nature = 'external',
                  type = 'real',
                  value = -1.81233,
                  texname = '\\text{ReFmeio}',
                  lhablock = 'Mass',
                  lhacode = [ 9000003 ])

FmeioH = Parameter(name = 'FmeioH',
                  nature = 'external',
                  type = 'real',
                  value = -1.54428,
                  texname = '\\text{FmeioH}',
                  lhablock = 'Mass',
                  lhacode = [ 9000005 ])

ImFmeio = Parameter(name = 'ImFmeio',
                  nature = 'external',
                  type = 'real',
                  value = 1.40622,
                  texname = '\\text{ImFmeio}',
                  lhablock = 'Mass',
                  lhacode = [ 9000001 ])

K = Parameter(name = 'K',
                  nature = 'external',
                  type = 'real',
                  value = 35.,
                  texname = '\\text{K}',
                  lhablock = 'Mass',
                  lhacode = [ 250 ]) 

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

lamWS = Parameter(name = 'lamWS',
                  nature = 'external',
                  type = 'real',
                  value = 0.2253,
                  texname = '\\text{lamWS}',
                  lhablock = 'Wolfenstein',
                  lhacode = [ 1 ])

AWS = Parameter(name = 'AWS',
                nature = 'external',
                type = 'real',
                value = 0.808,
                texname = '\\text{AWS}',
                lhablock = 'Wolfenstein',
                lhacode = [ 2 ])

rhoWS = Parameter(name = 'rhoWS',
                  nature = 'external',
                  type = 'real',
                  value = 0.132,
                  texname = '\\text{rhoWS}',
                  lhablock = 'Wolfenstein',
                  lhacode = [ 3 ])

etaWS = Parameter(name = 'etaWS',
                  nature = 'external',
                  type = 'real',
                  value = 0.341,
                  texname = '\\text{etaWS}',
                  lhablock = 'Wolfenstein',
                  lhacode = [ 4 ])

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

yme = Parameter(name = 'yme',
                nature = 'external',
                type = 'real',
                value = 0.0005110000000000001,
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

MZ = Parameter(name = 'MZ',
               nature = 'external',
               type = 'real',
               value = 91.1876,
               texname = '\\text{MZ}',
               lhablock = 'MASS',
               lhacode = [ 23 ])

MH = Parameter(name = 'MH',
               nature = 'external',
               type = 'real',
               value = 125,
               texname = '\\text{MH}',
               lhablock = 'MASS',
               lhacode = [ 25 ])

MH02 = Parameter(name = 'MH02',
               nature = 'external',
               type = 'real',
               value = 700.,
               texname = '\\text{MH02}',
               lhablock = 'MASS',
               lhacode = [ 35 ])

Me = Parameter(name = 'Me',
               nature = 'external',
               type = 'real',
               value = 0.0005110000000000001,
               texname = '\\text{Me}',
               lhablock = 'MASS',
               lhacode = [ 11 ])

MM = Parameter(name = 'MM',
               nature = 'external',
               type = 'real',
               value = 0.10566,
               texname = '\\text{MM}',
               lhablock = 'MASS',
               lhacode = [ 13 ])

MTA = Parameter(name = 'MTA',
                nature = 'external',
                type = 'real',
                value = 1.777,
                texname = '\\text{MTA}',
                lhablock = 'MASS',
                lhacode = [ 15 ])

MP = Parameter(name = 'MP',
               nature = 'external',
               type = 'real',
               value = 120,
               texname = '\\text{MP}',
               lhablock = 'MASS',
               lhacode = [ 9000006 ])

WT = Parameter(name = 'WT',
               nature = 'external',
               type = 'real',
               value = 1.491500,
               texname = '\\text{WT}',
               lhablock = 'DECAY',
               lhacode = [ 6 ])

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

WH = Parameter(name = 'WH',
               nature = 'external',
               type = 'real',
               value =  0.006382339,
               texname = '\\text{WH}',
               lhablock = 'DECAY',
               lhacode = [ 25 ])

WH02 = Parameter(name = 'WH02',
               nature = 'external',
               type = 'real',
               value = 0.006382339,
               texname = '\\text{WH02}',
               lhablock = 'DECAY',
               lhacode = [ 35 ])

WTau = Parameter(name = 'WTau',
                 nature = 'external',
                 type = 'real',
                 value = 2.27e-12,
                 texname = '\\text{WTau}',
                 lhablock = 'DECAY',
                 lhacode = [ 15 ])

WH1 = Parameter(name = 'WH1',
                nature = 'external',
                type = 'real',
                value =  0.006382339,
                texname = '\\text{WH1}',
                lhablock = 'DECAY',
                lhacode = [ 9000006 ])

CKM11 = Parameter(name = 'CKM11',
                  nature = 'internal',
                  type = 'complex',
                  value = '1 - lamWS**2/2.',
                  texname = '\\text{CKM11}')

CKM12 = Parameter(name = 'CKM12',
                  nature = 'internal',
                  type = 'complex',
                  value = 'lamWS',
                  texname = '\\text{CKM12}')

CKM13 = Parameter(name = 'CKM13',
                  nature = 'internal',
                  type = 'complex',
                  value = 'AWS*lamWS**3*(-(etaWS*complex(0,1)) + rhoWS)',
                  texname = '\\text{CKM13}')

CKM21 = Parameter(name = 'CKM21',
                  nature = 'internal',
                  type = 'complex',
                  value = '-lamWS',
                  texname = '\\text{CKM21}')

CKM22 = Parameter(name = 'CKM22',
                  nature = 'internal',
                  type = 'complex',
                  value = '1 - lamWS**2/2.',
                  texname = '\\text{CKM22}')

CKM23 = Parameter(name = 'CKM23',
                  nature = 'internal',
                  type = 'complex',
                  value = 'AWS*lamWS**2',
                  texname = '\\text{CKM23}')

CKM31 = Parameter(name = 'CKM31',
                  nature = 'internal',
                  type = 'complex',
                  value = 'AWS*lamWS**3*(1 - etaWS*complex(0,1) - rhoWS)',
                  texname = '\\text{CKM31}')

CKM32 = Parameter(name = 'CKM32',
                  nature = 'internal',
                  type = 'complex',
                  value = '-(AWS*lamWS**2)',
                  texname = '\\text{CKM32}')

CKM33 = Parameter(name = 'CKM33',
                  nature = 'internal',
                  type = 'complex',
                  value = '1',
                  texname = '\\text{CKM33}')

aEW = Parameter(name = 'aEW',
                nature = 'internal',
                type = 'real',
                value = '1/aEWM1',
                texname = '\\text{aEW}')

G = Parameter(name = 'G',
              nature = 'internal',
              type = 'real',
              value = '2*cmath.sqrt(aS)*cmath.sqrt(cmath.pi)',
              texname = 'G')

MW = Parameter(name = 'MW',
               nature = 'internal',
               type = 'real',
               value = 'cmath.sqrt(MZ**2/2.+cmath.sqrt(MZ**4/4.-(aEW*cmath.pi*MZ**2)/(Gf*cmath.sqrt(2))))',
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

gw = Parameter(name = 'gw',
               nature = 'internal',
               type = 'real',
               value = 'ee/sw',
               texname = 'g_w')

v = Parameter(name = 'v',
              nature = 'internal',
              type = 'real',
              value = '(2*MW*sw)/ee',
              texname = 'v')

Gphi = Parameter(name = 'Gphi',
                 nature = 'internal',
                 type = 'real',
                 value = '-(aS*(1 + MH**6/(560.*MT**6) + MH**4/(90.*MT**4) + MH**2/(12.*MT**2)))/(8.*cmath.pi**2*v)',
                 texname = 'G_h')

lam = Parameter(name = 'lam',
                nature = 'internal',
                type = 'real',
                value = 'MH**2/(2.*v**2)',
                texname = '\\text{lam}')

yb = Parameter(name = 'yb',
               nature = 'internal',
               type = 'real',
               value = '(ymb*cmath.sqrt(2))/v',
               texname = '\\text{yb}')

yc = Parameter(name = 'yc',
               nature = 'internal',
               type = 'real',
               value = '(ymc*cmath.sqrt(2))/v',
               texname = '\\text{yc}')

ye = Parameter(name = 'ye',
               nature = 'internal',
               type = 'real',
               value = '(yme*cmath.sqrt(2))/v',
               texname = '\\text{ye}')

ym = Parameter(name = 'ym',
               nature = 'internal',
               type = 'real',
               value = '(ymm*cmath.sqrt(2))/v',
               texname = '\\text{ym}')

yt = Parameter(name = 'yt',
               nature = 'internal',
               type = 'real',
               value = '(ymt*cmath.sqrt(2))/v',
               texname = '\\text{yt}')

ytau = Parameter(name = 'ytau',
                 nature = 'internal',
                 type = 'real',
                 value = '(ymtau*cmath.sqrt(2))/v',
                 texname = '\\text{ytau}')

muH = Parameter(name = 'muH',
                nature = 'internal',
                type = 'real',
                value = 'cmath.sqrt(lam*v**2)',
                texname = '\\mu')

Z = Parameter(name = 'Z',
              nature = 'internal',
              type = 'real',
              value = 'cmath.sqrt(1+6*xi*(1-6*xi)*(v/LR)**2)',
              texname = 'Z')

theta = Parameter(name = 'theta',
              nature = 'internal',
              type = 'real',
              value = '(1/2)*cmath.asin(12*(v/LR)*xi*MH**2/((v/LR)*(MH02**2 - MH**2)))',
              texname = 'theta')

a = Parameter(name = 'a',
              nature = 'internal',
              type = 'real',
              value = '-cmath.cos(theta)/Z',
              texname = 'a')

b = Parameter(name = 'b',
              nature = 'internal',
              type = 'real',
              value = 'cmath.sin(theta)/Z',
              texname = 'b')

c = Parameter(name = 'c',
              nature = 'internal',
              type = 'real',
              value = 'cmath.sin(theta)+6*xi*v*cmath.cos(theta)/(Z*LR)',
              texname = 'c')

d = Parameter(name = 'd',
              nature = 'internal',
              type = 'real',
              value = 'cmath.cos(theta)-6*xi*v*cmath.sin(theta)/(Z*LR)',
              texname = 'd')

FmeioMH = Parameter(name = 'FmeioMH',
              nature = 'internal',
              type = 'complex',
              value = '(1 + MH**6/(560.*MT**6) + MH**4/(90.*MT**4) + MH**2/(12.*MT**2))',
              texname = 'FmeioMH')

FmeioMR = Parameter(name = 'FmeioMR',
              nature = 'internal',
              type = 'complex',
              value = '-(3/4)*(1 + MH02**6/(560.*MT**6) + MH02**4/(90.*MT**4) + MH02**2/(12.*MT**2))',
              texname = 'Fmeio')

GR = Parameter(name = 'GR',
               nature = 'internal',
               type = 'complex',
               value = '((aS/(4*cmath.pi*v))*((c+(v/LR)*a)*((ReFmeio+ImFmeio*complex(0,1)))-2*7*(v/LR)*a)-1/(4*LR*K))',
               texname = 'G_R')

GH = Parameter(name = 'GH',
               nature = 'internal',
               type = 'complex',
               value = '(-(aS/(4*cmath.pi*v))*((d+(v/LR)*b)*(FmeioH)-2*7*(v/LR)*b))',
               texname = 'G_H')

AH = Parameter(name = 'AH',
               nature = 'internal',
               type = 'real',
               value = '(47*ee**2*(1 - (2*MH**4)/(987.*MT**4) - (14*MH**2)/(705.*MT**2) + (213*MH**12)/(2.634632e7*MW**12) + (5*MH**10)/(119756.*MW**10) + (41*MH**8)/(180950.*MW**8) + (87*MH**6)/(65800.*MW**6) + (57*MH**4)/(6580.*MW**4) + (33*MH**2)/(470.*MW**2)))/(72.*cmath.pi**2*v)',
               texname = 'A_H')

AR = Parameter(name = 'AR',
               nature = 'internal',
               type = 'real',
               value = '(47*ee**2*(1 - (2*MH02**4)/(987.*MT**4) - (14*MH02**2)/(705.*MT**2) + (213*MH02**12)/(2.634632e7*MW**12) + (5*MH02**10)/(119756.*MW**10) + (41*MH02**8)/(180950.*MW**8) + (87*MH02**6)/(65800.*MW**6) + (57*MH02**4)/(6580.*MW**4) + (33*MH02**2)/(470.*MW**2)))/(72.*cmath.pi**2*LR)',
               texname = 'A_R')

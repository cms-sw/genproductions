# This file was automatically created by FeynRules 1.6.13
# Mathematica version: 8.0 for Linux x86 (32-bit) (October 10, 2011)
# Date: Tue 12 Nov 2013 18:05:55


from object_library import all_lorentz, Lorentz

from function_library import complexconjugate, re, im, csc, sec, acsc, asec



SSS1 = Lorentz(name = 'SSS1',
               spins = [ 1, 1, 1 ],
               structure = '1')

SST1 = Lorentz(name = 'SST1',
               spins = [ 1, 1, 5 ],
               structure = 'Metric(1003,2003)')

SST2 = Lorentz(name = 'SST2',
               spins = [ 1, 1, 5 ],
               structure = 'P(1003,2)*P(2003,1) + P(1003,1)*P(2003,2) - P(-1,1)*P(-1,2)*Metric(1003,2003)')

FFS1 = Lorentz(name = 'FFS1',
               spins = [ 2, 2, 1 ],
               structure = 'Identity(2,1)')

FFV1 = Lorentz(name = 'FFV1',
               spins = [ 2, 2, 3 ],
               structure = 'Gamma(3,2,1)')

FFV2 = Lorentz(name = 'FFV2',
               spins = [ 2, 2, 3 ],
               structure = 'Gamma(3,2,-1)*ProjM(-1,1)')

FFV3 = Lorentz(name = 'FFV3',
               spins = [ 2, 2, 3 ],
               structure = 'Gamma(3,2,-1)*ProjM(-1,1) - 2*Gamma(3,2,-1)*ProjP(-1,1)')

FFV4 = Lorentz(name = 'FFV4',
               spins = [ 2, 2, 3 ],
               structure = 'Gamma(3,2,-1)*ProjM(-1,1) + 2*Gamma(3,2,-1)*ProjP(-1,1)')

FFV5 = Lorentz(name = 'FFV5',
               spins = [ 2, 2, 3 ],
               structure = 'Gamma(3,2,-1)*ProjM(-1,1) + 4*Gamma(3,2,-1)*ProjP(-1,1)')

VVS1 = Lorentz(name = 'VVS1',
               spins = [ 3, 3, 1 ],
               structure = 'Metric(1,2)')

VVS3 = Lorentz(name = 'VVS3',
               spins = [ 3, 3, 1 ],
               structure = 'P(1,2)*P(2,1) - P(-1,1)*P(-1,2)*Metric(1,2)')

VVV1 = Lorentz(name = 'VVV1',
               spins = [ 3, 3, 3 ],
               structure = 'P(3,1)*Metric(1,2) - P(3,2)*Metric(1,2) - P(2,1)*Metric(1,3) + P(2,3)*Metric(1,3) + P(1,2)*Metric(2,3) - P(1,3)*Metric(2,3)')

VVT1 = Lorentz(name = 'VVT1',
               spins = [ 3, 3, 5 ],
               structure = 'P(2,2)*P(2003,2)*Metric(1,1003) + P(2,2)*P(1003,2)*Metric(1,2003) + P(1,1)*P(2003,1)*Metric(2,1003) + P(1,1)*P(1003,1)*Metric(2,2003) - P(1,1)*P(2,1)*Metric(1003,2003) - 4*P(1,1)*P(2,2)*Metric(1003,2003) - P(1,2)*P(2,2)*Metric(1003,2003)')

VVT2 = Lorentz(name = 'VVT2',
               spins = [ 3, 3, 5 ],
               structure = 'Metric(1,2003)*Metric(2,1003) + Metric(1,1003)*Metric(2,2003) - Metric(1,2)*Metric(1003,2003)')

VVT3 = Lorentz(name = 'VVT3',
               spins = [ 3, 3, 5 ],
               structure = 'P(1003,2)*P(2003,1)*Metric(1,2) + P(1003,1)*P(2003,2)*Metric(1,2) - P(2,1)*P(2003,2)*Metric(1,1003) - P(2,1)*P(1003,2)*Metric(1,2003) - P(1,2)*P(2003,1)*Metric(2,1003) + P(-1,1)*P(-1,2)*Metric(1,2003)*Metric(2,1003) - P(1,2)*P(1003,1)*Metric(2,2003) + P(-1,1)*P(-1,2)*Metric(1,1003)*Metric(2,2003) + P(1,2)*P(2,1)*Metric(1003,2003) - P(-1,1)*P(-1,2)*Metric(1,2)*Metric(1003,2003)')

VVT4 = Lorentz(name = 'VVT4',
               spins = [ 3, 3, 5 ],
               structure = 'P(1003,2)*P(2003,1)*Metric(1,2) + P(1003,1)*P(2003,2)*Metric(1,2) - P(2,1)*P(2003,2)*Metric(1,1003) - P(2,2)*P(2003,2)*Metric(1,1003) - P(2,1)*P(1003,2)*Metric(1,2003) - P(2,2)*P(1003,2)*Metric(1,2003) - P(1,1)*P(2003,1)*Metric(2,1003) - P(1,2)*P(2003,1)*Metric(2,1003) + P(-1,1)*P(-1,2)*Metric(1,2003)*Metric(2,1003) - P(1,1)*P(1003,1)*Metric(2,2003) - P(1,2)*P(1003,1)*Metric(2,2003) + P(-1,1)*P(-1,2)*Metric(1,1003)*Metric(2,2003) + P(1,1)*P(2,1)*Metric(1003,2003) + P(1,2)*P(2,1)*Metric(1003,2003) + 4*P(1,1)*P(2,2)*Metric(1003,2003) + P(1,2)*P(2,2)*Metric(1003,2003) - P(-1,1)*P(-1,2)*Metric(1,2)*Metric(1003,2003)')

SSSS1 = Lorentz(name = 'SSSS1',
                spins = [ 1, 1, 1, 1 ],
                structure = '1')

SSST1 = Lorentz(name = 'SSST1',
                spins = [ 1, 1, 1, 5 ],
                structure = 'Metric(1004,2004)')

VVSS1 = Lorentz(name = 'VVSS1',
                spins = [ 3, 3, 1, 1 ],
                structure = 'Metric(1,2)')

VVST1 = Lorentz(name = 'VVST1',
                spins = [ 3, 3, 1, 5 ],
                structure = 'Metric(1,2004)*Metric(2,1004) + Metric(1,1004)*Metric(2,2004) - Metric(1,2)*Metric(1004,2004)')

VVVV1 = Lorentz(name = 'VVVV1',
                spins = [ 3, 3, 3, 3 ],
                structure = 'Metric(1,4)*Metric(2,3) - Metric(1,3)*Metric(2,4)')

VVVV2 = Lorentz(name = 'VVVV2',
                spins = [ 3, 3, 3, 3 ],
                structure = 'Metric(1,4)*Metric(2,3) + Metric(1,3)*Metric(2,4) - 2*Metric(1,2)*Metric(3,4)')

VVVV3 = Lorentz(name = 'VVVV3',
                spins = [ 3, 3, 3, 3 ],
                structure = 'Metric(1,4)*Metric(2,3) - Metric(1,2)*Metric(3,4)')

VVVV4 = Lorentz(name = 'VVVV4',
                spins = [ 3, 3, 3, 3 ],
                structure = 'Metric(1,3)*Metric(2,4) - Metric(1,2)*Metric(3,4)')

VVVV5 = Lorentz(name = 'VVVV5',
                spins = [ 3, 3, 3, 3 ],
                structure = 'Metric(1,4)*Metric(2,3) - (Metric(1,3)*Metric(2,4))/2. - (Metric(1,2)*Metric(3,4))/2.')

VVVT1 = Lorentz(name = 'VVVT1',
                spins = [ 3, 3, 3, 5 ],
                structure = 'P(2004,2)*Metric(1,1004)*Metric(2,3) - P(2004,3)*Metric(1,1004)*Metric(2,3) + P(1004,2)*Metric(1,2004)*Metric(2,3) - P(1004,3)*Metric(1,2004)*Metric(2,3) - P(2004,1)*Metric(1,3)*Metric(2,1004) + P(2004,3)*Metric(1,3)*Metric(2,1004) + P(3,1)*Metric(1,2004)*Metric(2,1004) - P(3,2)*Metric(1,2004)*Metric(2,1004) - P(1004,1)*Metric(1,3)*Metric(2,2004) + P(1004,3)*Metric(1,3)*Metric(2,2004) + P(3,1)*Metric(1,1004)*Metric(2,2004) - P(3,2)*Metric(1,1004)*Metric(2,2004) + P(2004,1)*Metric(1,2)*Metric(3,1004) - P(2004,2)*Metric(1,2)*Metric(3,1004) - P(2,1)*Metric(1,2004)*Metric(3,1004) + P(2,3)*Metric(1,2004)*Metric(3,1004) + P(1,2)*Metric(2,2004)*Metric(3,1004) - P(1,3)*Metric(2,2004)*Metric(3,1004) + P(1004,1)*Metric(1,2)*Metric(3,2004) - P(1004,2)*Metric(1,2)*Metric(3,2004) - P(2,1)*Metric(1,1004)*Metric(3,2004) + P(2,3)*Metric(1,1004)*Metric(3,2004) + P(1,2)*Metric(2,1004)*Metric(3,2004) - P(1,3)*Metric(2,1004)*Metric(3,2004) - P(3,1)*Metric(1,2)*Metric(1004,2004) + P(3,2)*Metric(1,2)*Metric(1004,2004) + P(2,1)*Metric(1,3)*Metric(1004,2004) - P(2,3)*Metric(1,3)*Metric(1004,2004) - P(1,2)*Metric(2,3)*Metric(1004,2004) + P(1,3)*Metric(2,3)*Metric(1004,2004)')

SSSST1 = Lorentz(name = 'SSSST1',
                 spins = [ 1, 1, 1, 1, 5 ],
                 structure = 'Metric(1005,2005)')

VVSST1 = Lorentz(name = 'VVSST1',
                 spins = [ 3, 3, 1, 1, 5 ],
                 structure = 'Metric(1,2005)*Metric(2,1005) + Metric(1,1005)*Metric(2,2005) - Metric(1,2)*Metric(1005,2005)')

VVVVT1 = Lorentz(name = 'VVVVT1',
                 spins = [ 3, 3, 3, 3, 5 ],
                 structure = 'Metric(1,2005)*Metric(2,4)*Metric(3,1005) - Metric(1,4)*Metric(2,2005)*Metric(3,1005) + Metric(1,1005)*Metric(2,4)*Metric(3,2005) - Metric(1,4)*Metric(2,1005)*Metric(3,2005) - Metric(1,2005)*Metric(2,3)*Metric(4,1005) + Metric(1,3)*Metric(2,2005)*Metric(4,1005) - Metric(1,1005)*Metric(2,3)*Metric(4,2005) + Metric(1,3)*Metric(2,1005)*Metric(4,2005) + Metric(1,4)*Metric(2,3)*Metric(1005,2005) - Metric(1,3)*Metric(2,4)*Metric(1005,2005)')

VVVVT2 = Lorentz(name = 'VVVVT2',
                 spins = [ 3, 3, 3, 3, 5 ],
                 structure = 'Metric(1,2005)*Metric(2,1005)*Metric(3,4) + Metric(1,1005)*Metric(2,2005)*Metric(3,4) - Metric(1,4)*Metric(2,2005)*Metric(3,1005) - Metric(1,4)*Metric(2,1005)*Metric(3,2005) - Metric(1,2005)*Metric(2,3)*Metric(4,1005) + Metric(1,2)*Metric(3,2005)*Metric(4,1005) - Metric(1,1005)*Metric(2,3)*Metric(4,2005) + Metric(1,2)*Metric(3,1005)*Metric(4,2005) + Metric(1,4)*Metric(2,3)*Metric(1005,2005) - Metric(1,2)*Metric(3,4)*Metric(1005,2005)')

VVVVT3 = Lorentz(name = 'VVVVT3',
                 spins = [ 3, 3, 3, 3, 5 ],
                 structure = 'Metric(1,2005)*Metric(2,1005)*Metric(3,4) + Metric(1,1005)*Metric(2,2005)*Metric(3,4) + Metric(1,2005)*Metric(2,4)*Metric(3,1005) - 2*Metric(1,4)*Metric(2,2005)*Metric(3,1005) + Metric(1,1005)*Metric(2,4)*Metric(3,2005) - 2*Metric(1,4)*Metric(2,1005)*Metric(3,2005) - 2*Metric(1,2005)*Metric(2,3)*Metric(4,1005) + Metric(1,3)*Metric(2,2005)*Metric(4,1005) + Metric(1,2)*Metric(3,2005)*Metric(4,1005) - 2*Metric(1,1005)*Metric(2,3)*Metric(4,2005) + Metric(1,3)*Metric(2,1005)*Metric(4,2005) + Metric(1,2)*Metric(3,1005)*Metric(4,2005) + 2*Metric(1,4)*Metric(2,3)*Metric(1005,2005) - Metric(1,3)*Metric(2,4)*Metric(1005,2005) - Metric(1,2)*Metric(3,4)*Metric(1005,2005)')

VVVVT4 = Lorentz(name = 'VVVVT4',
                 spins = [ 3, 3, 3, 3, 5 ],
                 structure = 'Metric(1,2005)*Metric(2,1005)*Metric(3,4) + Metric(1,1005)*Metric(2,2005)*Metric(3,4) - (Metric(1,2005)*Metric(2,4)*Metric(3,1005))/2. - (Metric(1,4)*Metric(2,2005)*Metric(3,1005))/2. - (Metric(1,1005)*Metric(2,4)*Metric(3,2005))/2. - (Metric(1,4)*Metric(2,1005)*Metric(3,2005))/2. - (Metric(1,2005)*Metric(2,3)*Metric(4,1005))/2. - (Metric(1,3)*Metric(2,2005)*Metric(4,1005))/2. + Metric(1,2)*Metric(3,2005)*Metric(4,1005) - (Metric(1,1005)*Metric(2,3)*Metric(4,2005))/2. - (Metric(1,3)*Metric(2,1005)*Metric(4,2005))/2. + Metric(1,2)*Metric(3,1005)*Metric(4,2005) + (Metric(1,4)*Metric(2,3)*Metric(1005,2005))/2. + (Metric(1,3)*Metric(2,4)*Metric(1005,2005))/2. - Metric(1,2)*Metric(3,4)*Metric(1005,2005)')

VVVVT5 = Lorentz(name = 'VVVVT5',
                 spins = [ 3, 3, 3, 3, 5 ],
                 structure = 'Metric(1,2005)*Metric(2,1005)*Metric(3,4) + Metric(1,1005)*Metric(2,2005)*Metric(3,4) - Metric(1,2005)*Metric(2,4)*Metric(3,1005) - Metric(1,1005)*Metric(2,4)*Metric(3,2005) - Metric(1,3)*Metric(2,2005)*Metric(4,1005) + Metric(1,2)*Metric(3,2005)*Metric(4,1005) - Metric(1,3)*Metric(2,1005)*Metric(4,2005) + Metric(1,2)*Metric(3,1005)*Metric(4,2005) + Metric(1,3)*Metric(2,4)*Metric(1005,2005) - Metric(1,2)*Metric(3,4)*Metric(1005,2005)')


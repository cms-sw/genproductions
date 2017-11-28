# This file was automatically created by FeynRules $Revision: 535 $
# Mathematica version: 7.0 for Mac OS X x86 (64-bit) (November 11, 2008)
# Date: Fri 18 Mar 2011 18:40:51


from object_library import all_couplings, Coupling

from function_library import complexconjugate, re, im, csc, sec, acsc, asec


GC_9 = Coupling(name = 'GC_9',
                value = '-G',
                order = {'QCD':1})

GC_10 = Coupling(name = 'GC_10',
                 value = 'complex(0,1)*G',
                 order = {'QCD':1})

GC_11 = Coupling(name = 'GC_11',
                 value = 'complex(0,1)*G**2',
                 order = {'QCD':2})

GC_101 = Coupling(name = 'GC_101',
                value = '2.0*G**3/(48.0*cmath.pi**2)',
                order = {'QCD':3})

GC_102 = Coupling(name = 'GC_102',
                 value = 'Ncol*G**3/(48.0*cmath.pi**2)*(7.0/4.0+lhv)',
                 order = {'QCD':3})

#=============================================================================================
#  4-gluon R2 couplings
#=============================================================================================

# Gluon contribution to it

GC_4GR2_delta = Coupling(name = 'GC_4GR2_delta',
                 value = 'RGR2/2.0',
                 order = {'QCD':4})

GC_4GR2_2Struct = Coupling(name = 'GC_4GR2_2Struct',
                 value = '-2.0*RGR2*Ncol*(2.0*lhv+5.0)',
                 order = {'QCD':4})

GC_4GR2_8Struct = Coupling(name = 'GC_4GR2_8Struct',
                 value = '8.0*RGR2*Ncol*(lhv+3.0)',
                 order = {'QCD':4})

# Fermion contribution to it

GC_4GR2_20Struct = Coupling(name = 'GC_4GR2_20Struct',
                 value = '-12.0*RGR2',
                 order = {'QCD':4})

GC_4GR2_12Struct = Coupling(name = 'GC_4GR2_12Struct',
                 value = '20.0*RGR2',
                 order = {'QCD':4})

#=============================================================================================

GC_104 = Coupling(name = 'GC_104',
                value = '-complex(0,1)*G**3/(16.0*cmath.pi**2)*((Ncol**2-1)/(2.0*Ncol))*(1.0+lhv)',
                order = {'QCD':3})

GC_105 = Coupling(name = 'GC_105',
                 value = 'complex(0,1)*G**2/(48.0*cmath.pi**2)',
                 order = {'QCD':2})

GC_115 = Coupling(name = 'GC_115',
                 value = 'complex(0,1)*G**2*Ncol/(48.0*cmath.pi**2)*(1.0/2.0+lhv)',
                 order = {'QCD':2})

GC_125 = Coupling(name = 'GC_125',
                 value = '-complex(0,1)*G**2*Ncol/(48.0*cmath.pi**2)*lhv',
                 order = {'QCD':2})

GC_106 = Coupling(name = 'GC_106',
                 value = '-complex(0,1)*G**2*(Ncol**2-1)/(32.0*cmath.pi**2*Ncol)',
                 order = {'QCD':2})

GC_201 = Coupling(name = 'GC_201',
                 value = '-G_UV',
                 order = {'QCD':3})

GC_202 = Coupling(name = 'GC_202',
                 value = 'complex(0,1)*(G_UV)**2',
                 order = {'QCD':4})

GC_203 = Coupling(name = 'GC_203',
                 value = 'complex(0,1)*G_UV',
                 order = {'QCD':3})

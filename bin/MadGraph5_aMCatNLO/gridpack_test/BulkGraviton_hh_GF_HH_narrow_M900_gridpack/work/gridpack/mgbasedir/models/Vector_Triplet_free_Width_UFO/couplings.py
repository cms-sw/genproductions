# This file was automatically created by FeynRules 1.7.178
# Mathematica version: 9.0 for Mac OS X x86 (64-bit) (November 20, 2012)
# Date: Sun 26 Jan 2014 12:11:59


from object_library import all_couplings, Coupling

from function_library import complexconjugate, re, im, csc, sec, acsc, asec, cot



GC_1 = Coupling(name = 'GC_1',
                value = '-(ee*complex(0,1))/3.',
                order = {'QED':1})

GC_2 = Coupling(name = 'GC_2',
                value = '(2*ee*complex(0,1))/3.',
                order = {'QED':1})

GC_3 = Coupling(name = 'GC_3',
                value = '-(ee*complex(0,1))',
                order = {'QED':1})

GC_4 = Coupling(name = 'GC_4',
                value = '-G',
                order = {'QCD':1})

GC_5 = Coupling(name = 'GC_5',
                value = 'complex(0,1)*G',
                order = {'QCD':1})

GC_6 = Coupling(name = 'GC_6',
                value = 'complex(0,1)*G**2',
                order = {'QCD':2})

GC_7 = Coupling(name = 'GC_7',
                value = '-(cC**2*cN*complex(0,1)*gw)',
                order = {'QED':1})

GC_8 = Coupling(name = 'GC_8',
                value = 'cC**2*cN*complex(0,1)*gw',
                order = {'QED':1})

GC_9 = Coupling(name = 'GC_9',
                value = '-(cC**2*cN*cvvw*complex(0,1)*gw)',
                order = {'QED':1})

GC_10 = Coupling(name = 'GC_10',
                 value = 'cC**2*cN*cvvw*complex(0,1)*gw',
                 order = {'QED':1})

GC_11 = Coupling(name = 'GC_11',
                 value = 'cC**2*cN*cvvw*cwt*complex(0,1)*gw',
                 order = {'QED':1})

GC_12 = Coupling(name = 'GC_12',
                 value = '-(cC*cN*cvvv*complex(0,1)*gst*sC)',
                 order = {'QED':1})

GC_13 = Coupling(name = 'GC_13',
                 value = 'cC*cN*cvvv*complex(0,1)*gst*sC',
                 order = {'QED':1})

GC_14 = Coupling(name = 'GC_14',
                 value = 'cC*cN*complex(0,1)*gw*sC',
                 order = {'QED':1})

GC_15 = Coupling(name = 'GC_15',
                 value = '-2*cC*cN*complex(0,1)*gw*sC',
                 order = {'QED':1})

GC_16 = Coupling(name = 'GC_16',
                 value = '-(cN*complex(0,1)*gw*sC**2)',
                 order = {'QED':1})

GC_17 = Coupling(name = 'GC_17',
                 value = 'cN*complex(0,1)*gw*sC**2',
                 order = {'QED':1})

GC_18 = Coupling(name = 'GC_18',
                 value = '-(cN*cvvw*complex(0,1)*gw*sC**2)',
                 order = {'QED':1})

GC_19 = Coupling(name = 'GC_19',
                 value = 'cN*cvvw*complex(0,1)*gw*sC**2',
                 order = {'QED':1})

GC_20 = Coupling(name = 'GC_20',
                 value = 'cN*cvvw*cwt*complex(0,1)*gw*sC**2',
                 order = {'QED':1})

GC_21 = Coupling(name = 'GC_21',
                 value = '3*cC**2*complex(0,1)*gw**2*sC**2',
                 order = {'QED':2})

GC_22 = Coupling(name = 'GC_22',
                 value = 'cC*cN*cwt*complex(0,1)*gw*sC - cC*cN*cvvw*cwt*complex(0,1)*gw*sC',
                 order = {'QED':1})

GC_23 = Coupling(name = 'GC_23',
                 value = '-(cC*cN*cwt*complex(0,1)*gw*sC) + cC*cN*cvvw*cwt*complex(0,1)*gw*sC',
                 order = {'QED':1})

GC_24 = Coupling(name = 'GC_24',
                 value = '-2*cC**3*cvvv*complex(0,1)*gst*gw*sC + 4*cC**2*cvvvv*complex(0,1)*gst**2*sC**2 + 2*cC*cvvv*complex(0,1)*gst*gw*sC**3',
                 order = {'QED':2})

GC_25 = Coupling(name = 'GC_25',
                 value = 'cC**4*cvvv*complex(0,1)*gst*gw - 4*cC**3*cvvvv*complex(0,1)*gst**2*sC - 2*cC**3*complex(0,1)*gw**2*sC - cC**3*cvvw*complex(0,1)*gw**2*sC - 3*cC**2*cvvv*complex(0,1)*gst*gw*sC**2 + cC*complex(0,1)*gw**2*sC**3 + cC*cvvw*complex(0,1)*gw**2*sC**3',
                 order = {'QED':2})

GC_26 = Coupling(name = 'GC_26',
                 value = '-2*cC**4*cvvv*complex(0,1)*gst*gw + 8*cC**3*cvvvv*complex(0,1)*gst**2*sC + 4*cC**3*complex(0,1)*gw**2*sC + 2*cC**3*cvvw*complex(0,1)*gw**2*sC + 6*cC**2*cvvv*complex(0,1)*gst*gw*sC**2 - 2*cC*complex(0,1)*gw**2*sC**3 - 2*cC*cvvw*complex(0,1)*gw**2*sC**3',
                 order = {'QED':2})

GC_27 = Coupling(name = 'GC_27',
                 value = '-(cC**4*complex(0,1)*gw**2) - 4*cC**2*complex(0,1)*gw**2*sC**2 - 2*cC**2*cvvw*complex(0,1)*gw**2*sC**2 - 4*cC*cvvv*complex(0,1)*gst*gw*sC**3 + 4*cvvvv*complex(0,1)*gst**2*sC**4',
                 order = {'QED':2})

GC_28 = Coupling(name = 'GC_28',
                 value = 'cC**3*complex(0,1)*gw**2*sC + cC**3*cvvw*complex(0,1)*gw**2*sC + 3*cC**2*cvvv*complex(0,1)*gst*gw*sC**2 - 4*cC*cvvvv*complex(0,1)*gst**2*sC**3 - 2*cC*complex(0,1)*gw**2*sC**3 - cC*cvvw*complex(0,1)*gw**2*sC**3 - cvvv*complex(0,1)*gst*gw*sC**4',
                 order = {'QED':2})

GC_29 = Coupling(name = 'GC_29',
                 value = '-2*cC**3*complex(0,1)*gw**2*sC - 2*cC**3*cvvw*complex(0,1)*gw**2*sC - 6*cC**2*cvvv*complex(0,1)*gst*gw*sC**2 + 8*cC*cvvvv*complex(0,1)*gst**2*sC**3 + 4*cC*complex(0,1)*gw**2*sC**3 + 2*cC*cvvw*complex(0,1)*gw**2*sC**3 + 2*cvvv*complex(0,1)*gst*gw*sC**4',
                 order = {'QED':2})

GC_30 = Coupling(name = 'GC_30',
                 value = '4*cC**4*cvvvv*complex(0,1)*gst**2 + 4*cC**3*cvvv*complex(0,1)*gst*gw*sC - 4*cC**2*complex(0,1)*gw**2*sC**2 - 2*cC**2*cvvw*complex(0,1)*gw**2*sC**2 - complex(0,1)*gw**2*sC**4',
                 order = {'QED':2})

GC_31 = Coupling(name = 'GC_31',
                 value = '-(cC**4*complex(0,1)*gw**2) - 2*cC**3*cvvv*complex(0,1)*gst*gw*sC + 4*cC**2*cvvvv*complex(0,1)*gst**2*sC**2 + cC**2*complex(0,1)*gw**2*sC**2 + 2*cC**2*cvvw*complex(0,1)*gw**2*sC**2 + 2*cC*cvvv*complex(0,1)*gst*gw*sC**3 - complex(0,1)*gw**2*sC**4',
                 order = {'QED':2})

GC_32 = Coupling(name = 'GC_32',
                 value = 'cC**4*complex(0,1)*gw**2 - 2*cC**2*cvvw*complex(0,1)*gw**2*sC**2 + complex(0,1)*gw**2*sC**4',
                 order = {'QED':2})

GC_33 = Coupling(name = 'GC_33',
                 value = '-(cC**4*cvvw*complex(0,1)*gw**2) - cvvw*complex(0,1)*gw**2*sC**4',
                 order = {'QED':2})

GC_34 = Coupling(name = 'GC_34',
                 value = '-(cC**2*complex(0,1)*gw*sN)',
                 order = {'QED':1})

GC_35 = Coupling(name = 'GC_35',
                 value = 'cC**2*complex(0,1)*gw*sN',
                 order = {'QED':1})

GC_36 = Coupling(name = 'GC_36',
                 value = '-(cC**2*cvvw*complex(0,1)*gw*sN)',
                 order = {'QED':1})

GC_37 = Coupling(name = 'GC_37',
                 value = 'cC**2*cvvw*complex(0,1)*gw*sN',
                 order = {'QED':1})

GC_38 = Coupling(name = 'GC_38',
                 value = 'cC**2*cvvw*cwt*complex(0,1)*gw*sN',
                 order = {'QED':1})

GC_39 = Coupling(name = 'GC_39',
                 value = '-(cC*cvvv*complex(0,1)*gst*sC*sN)',
                 order = {'QED':1})

GC_40 = Coupling(name = 'GC_40',
                 value = 'cC*cvvv*complex(0,1)*gst*sC*sN',
                 order = {'QED':1})

GC_41 = Coupling(name = 'GC_41',
                 value = '-(cC*complex(0,1)*gw*sC*sN)',
                 order = {'QED':1})

GC_42 = Coupling(name = 'GC_42',
                 value = 'cC*complex(0,1)*gw*sC*sN',
                 order = {'QED':1})

GC_43 = Coupling(name = 'GC_43',
                 value = '-(complex(0,1)*gw*sC**2*sN)',
                 order = {'QED':1})

GC_44 = Coupling(name = 'GC_44',
                 value = 'complex(0,1)*gw*sC**2*sN',
                 order = {'QED':1})

GC_45 = Coupling(name = 'GC_45',
                 value = '-(cvvw*complex(0,1)*gw*sC**2*sN)',
                 order = {'QED':1})

GC_46 = Coupling(name = 'GC_46',
                 value = 'cvvw*complex(0,1)*gw*sC**2*sN',
                 order = {'QED':1})

GC_47 = Coupling(name = 'GC_47',
                 value = 'cvvw*cwt*complex(0,1)*gw*sC**2*sN',
                 order = {'QED':1})

GC_48 = Coupling(name = 'GC_48',
                 value = 'cN*cwt*complex(0,1)*gw*sC**2 + cC**2*cvvv*complex(0,1)*gst*sN',
                 order = {'QED':1})

GC_49 = Coupling(name = 'GC_49',
                 value = 'cC*cN*cvvw*complex(0,1)*gw*sC + cC**2*cwt*complex(0,1)*gw*sN',
                 order = {'QED':1})

GC_50 = Coupling(name = 'GC_50',
                 value = '-(cN*cvvv*complex(0,1)*gst*sC**2) + cC**2*cwt*complex(0,1)*gw*sN',
                 order = {'QED':1})

GC_51 = Coupling(name = 'GC_51',
                 value = 'cC**2*cN*cwt*complex(0,1)*gw - cC*cvvw*complex(0,1)*gw*sC*sN',
                 order = {'QED':1})

GC_52 = Coupling(name = 'GC_52',
                 value = 'cN*cwt*complex(0,1)*gw*sC**2 + cC*cvvw*complex(0,1)*gw*sC*sN',
                 order = {'QED':1})

GC_53 = Coupling(name = 'GC_53',
                 value = 'cC*cwt*complex(0,1)*gw*sC*sN - cC*cvvw*cwt*complex(0,1)*gw*sC*sN',
                 order = {'QED':1})

GC_54 = Coupling(name = 'GC_54',
                 value = '-(cC*cwt*complex(0,1)*gw*sC*sN) + cC*cvvw*cwt*complex(0,1)*gw*sC*sN',
                 order = {'QED':1})

GC_55 = Coupling(name = 'GC_55',
                 value = 'cC**2*cN*cwt*complex(0,1)*gw + cvvv*complex(0,1)*gst*sC**2*sN',
                 order = {'QED':1})

GC_56 = Coupling(name = 'GC_56',
                 value = 'cC*cN*cvvw*complex(0,1)*gw*sC - cwt*complex(0,1)*gw*sC**2*sN',
                 order = {'QED':1})

GC_57 = Coupling(name = 'GC_57',
                 value = '-(cC**2*cN*cvvv*complex(0,1)*gst) + cwt*complex(0,1)*gw*sC**2*sN',
                 order = {'QED':1})

GC_58 = Coupling(name = 'GC_58',
                 value = '2*cC**2*cN**2*cvvv*complex(0,1)*gst*gw - 8*cC*cN**2*cvvvv*complex(0,1)*gst**2*sC - 2*cC*cN**2*complex(0,1)*gw**2*sC - 2*cN**2*cvvv*complex(0,1)*gst*gw*sC**2 - 2*cC**2*cN*cwt*complex(0,1)*gw**2*sN - 2*cC**2*cN*cvvw*cwt*complex(0,1)*gw**2*sN - 4*cC*cN*cvvv*cwt*complex(0,1)*gst*gw*sC*sN + 2*cN*cwt*complex(0,1)*gw**2*sC**2*sN + 2*cN*cvvw*cwt*complex(0,1)*gw**2*sC**2*sN',
                 order = {'QED':2})

GC_59 = Coupling(name = 'GC_59',
                 value = 'cC*cN**2*cvvv*cwt*complex(0,1)*gst*gw*sC + cC**2*cN*cvvv*complex(0,1)*gst*gw*sN - 4*cC*cN*cvvvv*complex(0,1)*gst**2*sC*sN - cC*cN*complex(0,1)*gw**2*sC*sN - cN*cvvv*complex(0,1)*gst*gw*sC**2*sN - cC*cvvv*cwt*complex(0,1)*gst*gw*sC*sN**2',
                 order = {'QED':2})

GC_60 = Coupling(name = 'GC_60',
                 value = '-(cC**2*cN**2*cvvv*cwt*complex(0,1)*gst*gw) + cC*cN**2*cwt*complex(0,1)*gw**2*sC + cC*cN**2*cvvw*cwt*complex(0,1)*gw**2*sC + 4*cC**2*cN*cvvvv*complex(0,1)*gst**2*sN + cC**2*cN*cwt**2*complex(0,1)*gw**2*sN + 2*cC*cN*cvvv*complex(0,1)*gst*gw*sC*sN - cN*complex(0,1)*gw**2*sC**2*sN + cN*cwt**2*complex(0,1)*gw**2*sC**2*sN + cC**2*cvvv*cwt*complex(0,1)*gst*gw*sN**2 - cC*cwt*complex(0,1)*gw**2*sC*sN**2 - cC*cvvw*cwt*complex(0,1)*gw**2*sC*sN**2',
                 order = {'QED':2})

GC_61 = Coupling(name = 'GC_61',
                 value = 'cC**2*cN**2*cwt**2*complex(0,1)*gw**2 + cN**2*cwt**2*complex(0,1)*gw**2*sC**2 + 2*cC*cN*cwt*complex(0,1)*gw**2*sC*sN + 2*cC*cN*cvvw*cwt*complex(0,1)*gw**2*sC*sN + 2*cN*cvvv*cwt*complex(0,1)*gst*gw*sC**2*sN + cC**2*complex(0,1)*gw**2*sN**2 + 2*cC*cvvv*complex(0,1)*gst*gw*sC*sN**2 - 4*cvvvv*complex(0,1)*gst**2*sC**2*sN**2',
                 order = {'QED':2})

GC_62 = Coupling(name = 'GC_62',
                 value = '-(cC**2*cN*cwt*complex(0,1)*gw**2*sN) - cC**2*cN*cvvw*cwt*complex(0,1)*gw**2*sN - 2*cC*cN*cvvv*cwt*complex(0,1)*gst*gw*sC*sN + cN*cwt*complex(0,1)*gw**2*sC**2*sN + cN*cvvw*cwt*complex(0,1)*gw**2*sC**2*sN - cC**2*cvvv*complex(0,1)*gst*gw*sN**2 + 4*cC*cvvvv*complex(0,1)*gst**2*sC*sN**2 + cC*complex(0,1)*gw**2*sC*sN**2 + cvvv*complex(0,1)*gst*gw*sC**2*sN**2',
                 order = {'QED':2})

GC_63 = Coupling(name = 'GC_63',
                 value = '2*cC*cN**2*cwt*complex(0,1)*gw**2*sC + 2*cC*cN**2*cvvw*cwt*complex(0,1)*gw**2*sC + 2*cN**2*cvvv*cwt*complex(0,1)*gst*gw*sC**2 + 2*cC**2*cN*complex(0,1)*gw**2*sN - 2*cC**2*cN*cwt**2*complex(0,1)*gw**2*sN + 4*cC*cN*cvvv*complex(0,1)*gst*gw*sC*sN - 8*cN*cvvvv*complex(0,1)*gst**2*sC**2*sN - 2*cN*cwt**2*complex(0,1)*gw**2*sC**2*sN - 2*cC*cwt*complex(0,1)*gw**2*sC*sN**2 - 2*cC*cvvw*cwt*complex(0,1)*gw**2*sC*sN**2 - 2*cvvv*cwt*complex(0,1)*gst*gw*sC**2*sN**2',
                 order = {'QED':2})

GC_64 = Coupling(name = 'GC_64',
                 value = 'cC**2*cN**2*cwt**2*complex(0,1)*gw**2 + cN**2*cwt**2*complex(0,1)*gw**2*sC**2 + 2*cC**2*cN*cvvv*cwt*complex(0,1)*gst*gw*sN - 2*cC*cN*cwt*complex(0,1)*gw**2*sC*sN - 2*cC*cN*cvvw*cwt*complex(0,1)*gw**2*sC*sN - 4*cC**2*cvvvv*complex(0,1)*gst**2*sN**2 - 2*cC*cvvv*complex(0,1)*gst*gw*sC*sN**2 + complex(0,1)*gw**2*sC**2*sN**2',
                 order = {'QED':2})

GC_65 = Coupling(name = 'GC_65',
                 value = '-(cC**2*cN**2*cwt*complex(0,1)*gw**2) + cN**2*cvvw*cwt*complex(0,1)*gw**2*sC**2 + cC**2*cvvw*cwt*complex(0,1)*gw**2*sN**2 - cwt*complex(0,1)*gw**2*sC**2*sN**2',
                 order = {'QED':2})

GC_66 = Coupling(name = 'GC_66',
                 value = 'cC**2*cN**2*cvvw*cwt*complex(0,1)*gw**2 - cN**2*cwt*complex(0,1)*gw**2*sC**2 - cC**2*cwt*complex(0,1)*gw**2*sN**2 + cvvw*cwt*complex(0,1)*gw**2*sC**2*sN**2',
                 order = {'QED':2})

GC_67 = Coupling(name = 'GC_67',
                 value = '-4*cC**2*cN**2*cvvvv*complex(0,1)*gst**2 - 2*cC*cN**2*cvvv*complex(0,1)*gst*gw*sC + cN**2*complex(0,1)*gw**2*sC**2 - 2*cC**2*cN*cvvv*cwt*complex(0,1)*gst*gw*sN + 2*cC*cN*cwt*complex(0,1)*gw**2*sC*sN + 2*cC*cN*cvvw*cwt*complex(0,1)*gw**2*sC*sN + cC**2*cwt**2*complex(0,1)*gw**2*sN**2 + cwt**2*complex(0,1)*gw**2*sC**2*sN**2',
                 order = {'QED':2})

GC_68 = Coupling(name = 'GC_68',
                 value = 'cC**2*cN**2*complex(0,1)*gw**2 + 2*cC*cN**2*cvvv*complex(0,1)*gst*gw*sC - 4*cN**2*cvvvv*complex(0,1)*gst**2*sC**2 - 2*cC*cN*cwt*complex(0,1)*gw**2*sC*sN - 2*cC*cN*cvvw*cwt*complex(0,1)*gw**2*sC*sN - 2*cN*cvvv*cwt*complex(0,1)*gst*gw*sC**2*sN + cC**2*cwt**2*complex(0,1)*gw**2*sN**2 + cwt**2*complex(0,1)*gw**2*sC**2*sN**2',
                 order = {'QED':2})

GC_69 = Coupling(name = 'GC_69',
                 value = '-((c3*complex(0,1)*gw**2*sC)/(gst*cmath.sqrt(2))) + (cC*ee*complex(0,1))/(swt*cmath.sqrt(2))',
                 order = {'QED':1})

GC_70 = Coupling(name = 'GC_70',
                 value = '-((cl*complex(0,1)*gw**2*sC)/(gst*cmath.sqrt(2))) + (cC*ee*complex(0,1))/(swt*cmath.sqrt(2))',
                 order = {'QED':1})

GC_71 = Coupling(name = 'GC_71',
                 value = '-((CKM1x1*cq*complex(0,1)*gw**2*sC)/(gst*cmath.sqrt(2))) + (cC*CKM1x1*ee*complex(0,1))/(swt*cmath.sqrt(2))',
                 order = {'QED':1})

GC_72 = Coupling(name = 'GC_72',
                 value = '-((CKM1x2*cq*complex(0,1)*gw**2*sC)/(gst*cmath.sqrt(2))) + (cC*CKM1x2*ee*complex(0,1))/(swt*cmath.sqrt(2))',
                 order = {'QED':1})

GC_73 = Coupling(name = 'GC_73',
                 value = '-((CKM2x1*cq*complex(0,1)*gw**2*sC)/(gst*cmath.sqrt(2))) + (cC*CKM2x1*ee*complex(0,1))/(swt*cmath.sqrt(2))',
                 order = {'QED':1})

GC_74 = Coupling(name = 'GC_74',
                 value = '-((CKM2x2*cq*complex(0,1)*gw**2*sC)/(gst*cmath.sqrt(2))) + (cC*CKM2x2*ee*complex(0,1))/(swt*cmath.sqrt(2))',
                 order = {'QED':1})

GC_75 = Coupling(name = 'GC_75',
                 value = '(c3*complex(0,1)*gw**2*sN)/(2.*gst) - (cN*cwt*ee*complex(0,1))/(2.*swt)',
                 order = {'QED':1})

GC_76 = Coupling(name = 'GC_76',
                 value = '(cl*complex(0,1)*gw**2*sN)/(2.*gst) - (cN*cwt*ee*complex(0,1))/(2.*swt)',
                 order = {'QED':1})

GC_77 = Coupling(name = 'GC_77',
                 value = '(cq*complex(0,1)*gw**2*sN)/(2.*gst) - (cN*cwt*ee*complex(0,1))/(2.*swt)',
                 order = {'QED':1})

GC_78 = Coupling(name = 'GC_78',
                 value = '-(c3*complex(0,1)*gw**2*sN)/(2.*gst) + (cN*cwt*ee*complex(0,1))/(2.*swt)',
                 order = {'QED':1})

GC_79 = Coupling(name = 'GC_79',
                 value = '-(cq*complex(0,1)*gw**2*sN)/(2.*gst) + (cN*cwt*ee*complex(0,1))/(2.*swt)',
                 order = {'QED':1})

GC_80 = Coupling(name = 'GC_80',
                 value = '(c3*cC*complex(0,1)*gw**2)/(gst*cmath.sqrt(2)) + (ee*complex(0,1)*sC)/(swt*cmath.sqrt(2))',
                 order = {'QED':1})

GC_81 = Coupling(name = 'GC_81',
                 value = '(cC*cl*complex(0,1)*gw**2)/(gst*cmath.sqrt(2)) + (ee*complex(0,1)*sC)/(swt*cmath.sqrt(2))',
                 order = {'QED':1})

GC_82 = Coupling(name = 'GC_82',
                 value = '(cC*CKM1x1*cq*complex(0,1)*gw**2)/(gst*cmath.sqrt(2)) + (CKM1x1*ee*complex(0,1)*sC)/(swt*cmath.sqrt(2))',
                 order = {'QED':1})

GC_83 = Coupling(name = 'GC_83',
                 value = '(cC*CKM1x2*cq*complex(0,1)*gw**2)/(gst*cmath.sqrt(2)) + (CKM1x2*ee*complex(0,1)*sC)/(swt*cmath.sqrt(2))',
                 order = {'QED':1})

GC_84 = Coupling(name = 'GC_84',
                 value = '(cC*CKM2x1*cq*complex(0,1)*gw**2)/(gst*cmath.sqrt(2)) + (CKM2x1*ee*complex(0,1)*sC)/(swt*cmath.sqrt(2))',
                 order = {'QED':1})

GC_85 = Coupling(name = 'GC_85',
                 value = '(cC*CKM2x2*cq*complex(0,1)*gw**2)/(gst*cmath.sqrt(2)) + (CKM2x2*ee*complex(0,1)*sC)/(swt*cmath.sqrt(2))',
                 order = {'QED':1})

GC_86 = Coupling(name = 'GC_86',
                 value = '-(c3*cN*complex(0,1)*gw**2)/(2.*gst) - (cwt*ee*complex(0,1)*sN)/(2.*swt)',
                 order = {'QED':1})

GC_87 = Coupling(name = 'GC_87',
                 value = '-(cl*cN*complex(0,1)*gw**2)/(2.*gst) - (cwt*ee*complex(0,1)*sN)/(2.*swt)',
                 order = {'QED':1})

GC_88 = Coupling(name = 'GC_88',
                 value = '-(cN*cq*complex(0,1)*gw**2)/(2.*gst) - (cwt*ee*complex(0,1)*sN)/(2.*swt)',
                 order = {'QED':1})

GC_89 = Coupling(name = 'GC_89',
                 value = '(c3*cN*complex(0,1)*gw**2)/(2.*gst) + (cwt*ee*complex(0,1)*sN)/(2.*swt)',
                 order = {'QED':1})

GC_90 = Coupling(name = 'GC_90',
                 value = '(cN*cq*complex(0,1)*gw**2)/(2.*gst) + (cwt*ee*complex(0,1)*sN)/(2.*swt)',
                 order = {'QED':1})

GC_91 = Coupling(name = 'GC_91',
                 value = '-(cN*ee*complex(0,1)*swt)/(6.*cwt)',
                 order = {'QED':1})

GC_92 = Coupling(name = 'GC_92',
                 value = '(cN*ee*complex(0,1)*swt)/(2.*cwt)',
                 order = {'QED':1})

GC_93 = Coupling(name = 'GC_93',
                 value = '-(cC**2*complex(0,1)*gw*swt)',
                 order = {'QED':1})

GC_94 = Coupling(name = 'GC_94',
                 value = 'cC**2*complex(0,1)*gw*swt',
                 order = {'QED':1})

GC_95 = Coupling(name = 'GC_95',
                 value = 'cC**2*cvvw*complex(0,1)*gw*swt',
                 order = {'QED':1})

GC_96 = Coupling(name = 'GC_96',
                 value = 'cC*cN*cvvv*complex(0,1)*gst*gw*sC*swt',
                 order = {'QED':2})

GC_97 = Coupling(name = 'GC_97',
                 value = '-(complex(0,1)*gw*sC**2*swt)',
                 order = {'QED':1})

GC_98 = Coupling(name = 'GC_98',
                 value = 'complex(0,1)*gw*sC**2*swt',
                 order = {'QED':1})

GC_99 = Coupling(name = 'GC_99',
                 value = 'cvvw*complex(0,1)*gw*sC**2*swt',
                 order = {'QED':1})

GC_100 = Coupling(name = 'GC_100',
                  value = '-(ee*complex(0,1)*sN*swt)/(6.*cwt)',
                  order = {'QED':1})

GC_101 = Coupling(name = 'GC_101',
                  value = '(ee*complex(0,1)*sN*swt)/(2.*cwt)',
                  order = {'QED':1})

GC_102 = Coupling(name = 'GC_102',
                  value = '2*cC*cvvv*complex(0,1)*gst*gw*sC*sN*swt',
                  order = {'QED':2})

GC_103 = Coupling(name = 'GC_103',
                  value = '-(cl*complex(0,1)*gw**2*sN)/(2.*gst) + (cN*cwt*ee*complex(0,1))/(2.*swt) + (cN*ee*complex(0,1)*swt)/(2.*cwt)',
                  order = {'QED':1})

GC_104 = Coupling(name = 'GC_104',
                  value = 'cC*complex(0,1)*gw*sC*swt - cC*cvvw*complex(0,1)*gw*sC*swt',
                  order = {'QED':1})

GC_105 = Coupling(name = 'GC_105',
                  value = '-(cC*complex(0,1)*gw*sC*swt) + cC*cvvw*complex(0,1)*gw*sC*swt',
                  order = {'QED':1})

GC_106 = Coupling(name = 'GC_106',
                  value = '-(cC**2*cN*cvvw*complex(0,1)*gw**2*swt) + cN*complex(0,1)*gw**2*sC**2*swt',
                  order = {'QED':2})

GC_107 = Coupling(name = 'GC_107',
                  value = 'cC**2*cN*complex(0,1)*gw**2*swt - cN*cvvw*complex(0,1)*gw**2*sC**2*swt',
                  order = {'QED':2})

GC_108 = Coupling(name = 'GC_108',
                  value = '(cl*cN*complex(0,1)*gw**2)/(2.*gst) + (cwt*ee*complex(0,1)*sN)/(2.*swt) + (ee*complex(0,1)*sN*swt)/(2.*cwt)',
                  order = {'QED':1})

GC_109 = Coupling(name = 'GC_109',
                  value = '-2*cC**2*cN*cwt*complex(0,1)*gw**2*swt - 2*cN*cwt*complex(0,1)*gw**2*sC**2*swt - 2*cC**2*cvvv*complex(0,1)*gst*gw*sN*swt + 2*cC*complex(0,1)*gw**2*sC*sN*swt + 2*cC*cvvw*complex(0,1)*gw**2*sC*sN*swt',
                  order = {'QED':2})

GC_110 = Coupling(name = 'GC_110',
                  value = '-2*cC**2*cN*cwt*complex(0,1)*gw**2*swt - 2*cN*cwt*complex(0,1)*gw**2*sC**2*swt - 2*cC*complex(0,1)*gw**2*sC*sN*swt - 2*cC*cvvw*complex(0,1)*gw**2*sC*sN*swt - 2*cvvv*complex(0,1)*gst*gw*sC**2*sN*swt',
                  order = {'QED':2})

GC_111 = Coupling(name = 'GC_111',
                  value = 'cC**2*cvvw*complex(0,1)*gw**2*sN*swt - complex(0,1)*gw**2*sC**2*sN*swt',
                  order = {'QED':2})

GC_112 = Coupling(name = 'GC_112',
                  value = 'cC**2*complex(0,1)*gw**2*sN*swt - cvvw*complex(0,1)*gw**2*sC**2*sN*swt',
                  order = {'QED':2})

GC_113 = Coupling(name = 'GC_113',
                  value = '-(cC*cN*complex(0,1)*gw**2*sC*swt) - cC*cN*cvvw*complex(0,1)*gw**2*sC*swt - cN*cvvv*complex(0,1)*gst*gw*sC**2*swt + cC**2*cwt*complex(0,1)*gw**2*sN*swt + cwt*complex(0,1)*gw**2*sC**2*sN*swt',
                  order = {'QED':2})

GC_114 = Coupling(name = 'GC_114',
                  value = '2*cC**2*cN*cvvv*complex(0,1)*gst*gw*swt - 2*cC*cN*complex(0,1)*gw**2*sC*swt - 2*cC*cN*cvvw*complex(0,1)*gw**2*sC*swt - 2*cC**2*cwt*complex(0,1)*gw**2*sN*swt - 2*cwt*complex(0,1)*gw**2*sC**2*sN*swt',
                  order = {'QED':2})

GC_115 = Coupling(name = 'GC_115',
                  value = 'cC**2*complex(0,1)*gw**2*swt**2 + complex(0,1)*gw**2*sC**2*swt**2',
                  order = {'QED':2})

GC_116 = Coupling(name = 'GC_116',
                  value = '-(cC*ch*complex(0,1)*gst*gw*sC) + 2*cvvhh*complex(0,1)*gst**2*sC**2 + (2*bb*cC**2*complex(0,1)*MWt**2)/vv**2',
                  order = {'QED':2})

GC_117 = Coupling(name = 'GC_117',
                  value = '-(ch*cN*complex(0,1)*gst*gz*sN) + 2*cvvhh*complex(0,1)*gst**2*sN**2 + (2*bb*cN**2*complex(0,1)*MZt**2)/vv**2',
                  order = {'QED':2})

GC_118 = Coupling(name = 'GC_118',
                  value = '(cC**2*ch*complex(0,1)*gst*gw)/2. - 2*cC*cvvhh*complex(0,1)*gst**2*sC - (ch*complex(0,1)*gst*gw*sC**2)/2. + (2*bb*cC*complex(0,1)*MWt**2*sC)/vv**2',
                  order = {'QED':2})

GC_119 = Coupling(name = 'GC_119',
                  value = '2*cC**2*cvvhh*complex(0,1)*gst**2 + cC*ch*complex(0,1)*gst*gw*sC + (2*bb*complex(0,1)*MWt**2*sC**2)/vv**2',
                  order = {'QED':2})

GC_120 = Coupling(name = 'GC_120',
                  value = '(ch*cN**2*complex(0,1)*gst*gz)/2. - 2*cN*cvvhh*complex(0,1)*gst**2*sN - (ch*complex(0,1)*gst*gz*sN**2)/2. + (2*bb*cN*complex(0,1)*MZt**2*sN)/vv**2',
                  order = {'QED':2})

GC_121 = Coupling(name = 'GC_121',
                  value = '2*cN**2*cvvhh*complex(0,1)*gst**2 + ch*cN*complex(0,1)*gst*gz*sN + (2*bb*complex(0,1)*MZt**2*sN**2)/vv**2',
                  order = {'QED':2})

GC_122 = Coupling(name = 'GC_122',
                  value = '(3*d4*complex(0,1)*MH**2)/vv**2',
                  order = {'QED':2})

GC_123 = Coupling(name = 'GC_123',
                  value = '(3*d3*complex(0,1)*MH**2)/vv',
                  order = {'QED':1})

GC_124 = Coupling(name = 'GC_124',
                  value = '(2*aa*complex(0,1)*MWt**2*sC**2)/vv + 2*cC**2*cvvhh*complex(0,1)*gst**2*vv + cC*ch*complex(0,1)*gst*gw*sC*vv',
                  order = {'QED':1})

GC_125 = Coupling(name = 'GC_125',
                  value = '(2*aa*cC**2*complex(0,1)*MWt**2)/vv - cC*ch*complex(0,1)*gst*gw*sC*vv + 2*cvvhh*complex(0,1)*gst**2*sC**2*vv',
                  order = {'QED':1})

GC_126 = Coupling(name = 'GC_126',
                  value = '(2*aa*cC*complex(0,1)*MWt**2*sC)/vv + (cC**2*ch*complex(0,1)*gst*gw*vv)/2. - 2*cC*cvvhh*complex(0,1)*gst**2*sC*vv - (ch*complex(0,1)*gst*gw*sC**2*vv)/2.',
                  order = {'QED':1})

GC_127 = Coupling(name = 'GC_127',
                  value = '(2*aa*complex(0,1)*MZt**2*sN**2)/vv + 2*cN**2*cvvhh*complex(0,1)*gst**2*vv + ch*cN*complex(0,1)*gst*gz*sN*vv',
                  order = {'QED':1})

GC_128 = Coupling(name = 'GC_128',
                  value = '(2*aa*cN**2*complex(0,1)*MZt**2)/vv - ch*cN*complex(0,1)*gst*gz*sN*vv + 2*cvvhh*complex(0,1)*gst**2*sN**2*vv',
                  order = {'QED':1})

GC_129 = Coupling(name = 'GC_129',
                  value = '(2*aa*cN*complex(0,1)*MZt**2*sN)/vv + (ch*cN**2*complex(0,1)*gst*gz*vv)/2. - 2*cN*cvvhh*complex(0,1)*gst**2*sN*vv - (ch*complex(0,1)*gst*gz*sN**2*vv)/2.',
                  order = {'QED':1})

GC_130 = Coupling(name = 'GC_130',
                  value = '-((cpsi*complex(0,1)*yb)/cmath.sqrt(2))',
                  order = {'QED':1})

GC_131 = Coupling(name = 'GC_131',
                  value = '-((cpsi*complex(0,1)*yc)/cmath.sqrt(2))',
                  order = {'QED':1})

GC_132 = Coupling(name = 'GC_132',
                  value = '-((cpsi*complex(0,1)*yt)/cmath.sqrt(2))',
                  order = {'QED':1})

GC_133 = Coupling(name = 'GC_133',
                  value = '-((cpsi*complex(0,1)*ytau)/cmath.sqrt(2))',
                  order = {'QED':1})

GC_134 = Coupling(name = 'GC_134',
                  value = '-((cq*complex(0,1)*gw**2*sC*complexconjugate(CKM1x1))/(gst*cmath.sqrt(2))) + (cC*ee*complex(0,1)*complexconjugate(CKM1x1))/(swt*cmath.sqrt(2))',
                  order = {'QED':1})

GC_135 = Coupling(name = 'GC_135',
                  value = '(cC*cq*complex(0,1)*gw**2*complexconjugate(CKM1x1))/(gst*cmath.sqrt(2)) + (ee*complex(0,1)*sC*complexconjugate(CKM1x1))/(swt*cmath.sqrt(2))',
                  order = {'QED':1})

GC_136 = Coupling(name = 'GC_136',
                  value = '-((cq*complex(0,1)*gw**2*sC*complexconjugate(CKM1x2))/(gst*cmath.sqrt(2))) + (cC*ee*complex(0,1)*complexconjugate(CKM1x2))/(swt*cmath.sqrt(2))',
                  order = {'QED':1})

GC_137 = Coupling(name = 'GC_137',
                  value = '(cC*cq*complex(0,1)*gw**2*complexconjugate(CKM1x2))/(gst*cmath.sqrt(2)) + (ee*complex(0,1)*sC*complexconjugate(CKM1x2))/(swt*cmath.sqrt(2))',
                  order = {'QED':1})

GC_138 = Coupling(name = 'GC_138',
                  value = '-((cq*complex(0,1)*gw**2*sC*complexconjugate(CKM2x1))/(gst*cmath.sqrt(2))) + (cC*ee*complex(0,1)*complexconjugate(CKM2x1))/(swt*cmath.sqrt(2))',
                  order = {'QED':1})

GC_139 = Coupling(name = 'GC_139',
                  value = '(cC*cq*complex(0,1)*gw**2*complexconjugate(CKM2x1))/(gst*cmath.sqrt(2)) + (ee*complex(0,1)*sC*complexconjugate(CKM2x1))/(swt*cmath.sqrt(2))',
                  order = {'QED':1})

GC_140 = Coupling(name = 'GC_140',
                  value = '-((cq*complex(0,1)*gw**2*sC*complexconjugate(CKM2x2))/(gst*cmath.sqrt(2))) + (cC*ee*complex(0,1)*complexconjugate(CKM2x2))/(swt*cmath.sqrt(2))',
                  order = {'QED':1})

GC_141 = Coupling(name = 'GC_141',
                  value = '(cC*cq*complex(0,1)*gw**2*complexconjugate(CKM2x2))/(gst*cmath.sqrt(2)) + (ee*complex(0,1)*sC*complexconjugate(CKM2x2))/(swt*cmath.sqrt(2))',
                  order = {'QED':1})

GC_142 = Coupling(name = 'GC_142',
                value = '-(AH*complex(0,1))',
                order = {'QED':1})


#! /usr/bin/env python

import sys
import ninjanumgen


# If this is set to true, test_experimental_feature() is called
# instead of main()
TEST_EXPERIMENTAL_FEATURE = False


# main function
def main():
    diag = ninjanumgen.DiagramExpansion('6photons.frm',
                                        '6photons_num.cc',
                                        6,
                                        rank = 6,
                                        diagname = 'SixPhotons')
    diag.writeSource()


# Here we test an experimental feature of NinjaNumGen.  Instead of
# having a numerator as a function of Q and Mu2, this feature allows
# to write it as a function of the momenta L`i' = Q+p`i' significantly
# reducing the number of terms in the expression.  As a downside, more
# input is needed because Ninja must know how to define the L`i' at
# runtime.
def test_experimental_feature():
    inline_evaluate = ['ninja::ComplexMomentum L1 = ninjaQ+k1;',
                       'ninja::ComplexMomentum L2 = ninjaQ+p2;',
                       'ninja::ComplexMomentum L3 = ninjaQ+p3;',
                       'ninja::ComplexMomentum L4 = ninjaQ+p4;',
                       'ninja::ComplexMomentum L5 = ninjaQ-k6;',
                       'ninja::ComplexMomentum L6 = ninjaQ;'
                       ]
    inline_t3 = ['ninja::ComplexMomentum ninjaAl1 = ninjaA+k1;',
                 'ninja::ComplexMomentum ninjaAl2 = ninjaA+p2;',
                 'ninja::ComplexMomentum ninjaAl3 = ninjaA+p3;',
                 'ninja::ComplexMomentum ninjaAl4 = ninjaA+p4;',
                 'ninja::ComplexMomentum ninjaAl5 = ninjaA-k6;',
                 'ninja::ComplexMomentum ninjaAl6 = ninjaA;'
                 ]
    inline_t2 = ['ninja::ComplexMomentum ninjaA0l1 = ninjaA0+k1;',
                 'ninja::ComplexMomentum ninjaA0l2 = ninjaA0+p2;',
                 'ninja::ComplexMomentum ninjaA0l3 = ninjaA0+p3;',
                 'ninja::ComplexMomentum ninjaA0l4 = ninjaA0+p4;',
                 'ninja::ComplexMomentum ninjaA0l5 = ninjaA0-k6;',
                 'ninja::ComplexMomentum ninjaA0l6 = ninjaA0;'
                 ]
    diag = ninjanumgen.DiagramExpansion('6photons.frm',
                                        '6photons_num.cc',
                                        6,
                                        rank = 6,
                                        diagname = 'SixPhotons',
                                        formflags='-D NINJAGEN_EXP_FEATURE=1',
                                        inline_evaluate=inline_evaluate,
                                        inline_t3expansion=inline_t3,
                                        inline_t2expansion=inline_t2,
                                        loop_prefix='L')
    diag.writeSource()


if __name__ == "__main__":
    
    if (len(sys.argv) >= 2) and (sys.argv[1]=='--experimental'):
        TEST_EXPERIMENTAL_FEATURE = True
    
    if TEST_EXPERIMENTAL_FEATURE:
        test_experimental_feature()
    else:
        main()

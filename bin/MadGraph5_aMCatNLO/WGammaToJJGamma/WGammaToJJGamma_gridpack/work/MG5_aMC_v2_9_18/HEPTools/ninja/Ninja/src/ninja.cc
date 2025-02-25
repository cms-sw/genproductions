// Definition of the public method ninja::Amplitude::evaluate() and
// the fuctions and variables regulating the global options.


#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdexcept>
#include <algorithm>

#include <ninja/types.hh>
#include <tmp_utils.hh>
#include <cuts.hh>
#include <ninja/ninja.hh>
#include <integermath.hh>
#include <cuts_utils.hh>
#include <coefficient_level_subs.hh>
#include <integral_library_wrapper.hh>
#include <s_mat_wrapper.hh>

#include <ninja/avholo.hh>
#if defined(NINJA_USE_LOOPTOOLS) && !defined(NINJA_USE_ONELOOP)
# include <ninja/looptools.hh>
#endif

using namespace ninja;
using namespace cuts;
using namespace std;
using namespace ninja_integer_math;
using namespace integral_library_wrapper;
using namespace s_matrix_wrapper;


//quadninja//#define QUADNINJA_NINJA_CC 1
#ifndef QUADNINJA_NINJA_CC
namespace quadninja {
  void printBanner(std::ostream & banner_out, bool force_print)
  {
    ninja::printBanner(banner_out, force_print);
  }
}
#endif


namespace ninja {

  namespace {
    char ninja_banner[] = 
      "  +----------------------------------------------------------------+\n"
      "  |                                                                |\n"
      "  |  Ninja - version " VERSION
      "                                         |\n"
      "  |                                                                |\n"
      "  |  Author: Tiziano Peraro                                        |\n"
      "  |                                                                |\n"
      "  |  Based on:                                                     |\n"
      "  |                                                                |\n"
      "  |      P. Mastrolia, E. Mirabella and T. Peraro,                 |\n"
      "  |      \"Integrand reduction of one-loop scattering amplitudes    |\n"
      "  |      through Laurent series expansion,\"                        |\n"
      "  |      JHEP 1206 (2012) 095  [arXiv:1203.0291 [hep-ph]].         |\n"
      "  |                                                                |\n"
      "  |      T. Peraro,                                                |\n"
      "  |      \"Ninja: Automated Integrand Reduction via Laurent         |\n"
      "  |      Expansion for One-Loop Amplitudes,\"                       |\n"
      "  |      Comput.Phys.Commun. 185 (2014) [arXiv:1403.1229 [hep-ph]] |\n"
      "  |                                                                |\n"
      "  +----------------------------------------------------------------+\n";
  }

#ifndef QUADNINJA_NINJA_CC
  // Print the banner
  void printBanner(std::ostream & banner_out, bool force_print)
  {
    if (force_print || !Options::quiet) {
      banner_out << endl;
      banner_out << ninja_banner << endl;
      banner_out << endl;
    }
    Options::quiet = true;
  }
#endif

  // Initialize Options
  std::ostream * Options::out = & std::cout;
  Real Options::chop_tol = 1.e-10;
  Real Options::test_tol = 1.e-05;
  Real Options::fp_threshold = REAL_MIN;
  bool Options::quiet = false;
  unsigned Options::verb = Verbose::NONE;
  unsigned Options::test = Test::NONE;
#if defined(NINJA_USE_LOOPTOOLS) && !defined(NINJA_USE_ONELOOP)
  IntegralLibrary * Options::mis = & loop_tools;
#else
  IntegralLibrary * Options::mis = & avh_olo;
#endif

} // namespace ninja


// namespace {
//   typedef long double HigherPrecision;
// }


namespace ninja {


  template<typename MassType>
  int Amplitude<MassType>::evaluate(Numerator & num)
  {

    if (!Options::quiet)
      printBanner(std::cout,false);

    // Checks the rank.  In this implementation it prints a message in
    // to stderr and throws an invalid_argument exception

#ifdef NINJA_X1RANK
    if (rank>n+1) {
      cerr << "ERROR IN NINJA: "
           << "The rank is too high: maximum_rank = n_denominators+1" << endl;
      NINJA_THROW (invalid_argument("Rank is too high."));
    } else if (rank==n+1) {
      return higherRankEvaluate(num);
    }
#else // NINJA_X1RANK
    if (rank>n) {
      cerr << "ERROR IN NINJA: "
           << "The rank is too high: maximum_rank = n_denominators\n";
      cerr << "NOTE: Configuring with --enable-higher_rank"
           << "would make maximum_rank = n_denominators+1" << endl;
      NINJA_THROW (invalid_argument("Rank is too high."));
    }
#endif // ! NINJA_X1RANK


    if (Options::verb) {
      (*Options::out) << "----------------------------"
                      << "----------------------------\n\n" << endl;
      (*Options::out) << "Ninja is evaluating a loop integral\n\n" << endl;
      (*Options::out) << "Loop propagators:" << endl;
      for (int i=0; i<n; ++i)
        (*Options::out) << "D(" << i << ") : "
                        << "momentum = " << V[i] << ", " << endl
                        << "       mass^2 = " << m2[i] << endl;
      (*Options::out) << endl << endl;
    }


    // If S-mat is not set, compute it
    WrapSMatrix wrap_s_mat(s_mat,n,V);


    // flags
    bool tests;
    bool local_test;
    bool global_test = Options::test & Test::GLOBAL;
    return_val = Amplitude::SUCCESS;


    ///////////////////
    // Quintuple cut //
    ///////////////////
  
    // create Pentagons
    tests = Options::test;
    bool anyPentagon = (n >= 5) && (min_cut <= 5)
      && (tests || (Options::verb & Verbose::C5) || !use_mu_exp);
    int n5cuts = anyPentagon ? combinations5(n) : 0;
    CutsVector<Pentagon> pentagons(n5cuts,n);

    // store the results
    if (anyPentagon) {
      evaluatePentagons(num, pentagons);
      if (unstable_kinematics()) return return_val;
      if (Options::verb & Verbose::C5)
        print(pentagons);
    }


    ///////////////////
    // Quadruple cut //
    ///////////////////
  
    // create Boxes
    local_test = Options::test & Test::LOCAL_4;
    tests = global_test || local_test;
    bool anyBox = n >= 4 && min_cut <= 4;
    int n4cuts = anyBox ? combinations4(n) : 0;
    CutsVector<Box> boxes(n4cuts,n);

    // store the results
    if (anyBox) {
      if (tests || (Options::verb & Verbose::C4)) {
        evaluateFullBoxes(num, pentagons, boxes);
        if (unstable_kinematics()) return return_val;
        if (Options::verb & Verbose::C4)
          print(boxes);
        if (local_test)
          local4NeqNtests(num, pentagons, boxes);
      }
      else evaluateBoxes(num, pentagons, boxes);
      if (unstable_kinematics()) return return_val;
    }


    ////////////////
    // Triple cut //
    ////////////////
  
    // create Triangles
    local_test = Options::test & Test::LOCAL_3;
    tests = global_test || local_test;
    bool anyTriangle = n >= 3 && min_cut <= 3 && rank - n + 3 >= 0;
    int n3cuts = anyTriangle ? combinations3(n) : 0;
    CutsVector<Triangle> triangles(n3cuts,n);

    // store the results
    if (anyTriangle) {
      evaluateTriangles(num, triangles);
      if (unstable_kinematics()) return return_val;
      if (Options::verb & Verbose::C3)
        print(triangles);
      if (Options::test & Test::LOCAL_3)
        local3NeqNtests(num, pentagons, boxes, triangles);
    }


    ////////////////
    // double cut //
    ////////////////
  
    // create Bubbles
    local_test = Options::test & Test::LOCAL_2;
    tests = global_test || local_test;
    bool anyBubble = n >= 2 && min_cut <= 2 && rank - n + 2 >= 0 ;
    int n2cuts = anyBubble ? combinations2(n) : 0;
    CutsVector<Bubble> bubbles(n2cuts,n);

    // store the results
    if (anyBubble) {
      evaluateBubbles(num, triangles, bubbles);
      if (unstable_kinematics()) return return_val;
      if (Options::verb & Verbose::C2)
        print(bubbles);
      if (local_test)
        local2NeqNtests(num, pentagons, boxes, triangles, bubbles);
      }


    ////////////////
    // Single cut //
    ////////////////
  
    // create Tadpoles
    local_test = Options::test & Test::LOCAL_1;
    tests = global_test || local_test;
    bool anyTadpole = n >= 1 && min_cut <= 1 && rank - n + 1 >= 0 ;
    int n1cuts = anyTadpole ? n : 0;
    CutsVector<Tadpole> tadpoles(n1cuts,n);

    // store the results
    if (anyTadpole) {
      if (tests || (Options::verb & Verbose::C1)) {
        evaluateFullTadpoles(num, triangles, bubbles, tadpoles);
        if (unstable_kinematics()) return return_val;
        if (Options::verb & Verbose::C1)
          print(tadpoles);
        if (local_test)
          local1NeqNtests(num, pentagons, boxes, triangles, bubbles, tadpoles);
      }
      else evaluateTadpoles(num, triangles, bubbles, tadpoles);
      if (unstable_kinematics()) return return_val;
    }


    /////////////////////////
    // N = N test (global) //
    /////////////////////////

    if (global_test && min_cut <= 1)
      NeqNtest(num, pentagons, boxes, triangles, bubbles, tadpoles,
               ComplexMomentum(10.3,10.3,10.3,10.3), 13.);


    ////////////////
    // Power test //
    ////////////////

    if ((global_test || local_test) && (Options::verb & Verbose::LOCAL_TEST_1)
        && (min_cut <= 1) && (rank == n)) {
      Complex ptest = 0.;
      for(unsigned int i =0;i<tadpoles.size();++i) 
        for(unsigned int j =1;j<5;++j) ptest += tadpoles[i].c[j]; 
      (*Options::out) << "Power test                     :  "
                      << ptest << endl << endl;
    }


    /////////
    // MIs //
    /////////

    Complex rational_part_temp = Real();
    Complex result_temp[3] = {Real(),Real(),Real()};

    {
      // This will call mis->init now and mis->exit at the end of this
      // scope
      WrapIntegralLibrary wrap_mis(mis,scale);

      if (Options::verb & Verbose::INTEGRALS)
        (*Options::out) << "Master Integrals :" << endl << endl;

      // 4-point
      if (anyBox) {
        for (BoxesIter i = boxes.begin(); i!=boxes.end(); ++i) {
          Complex integral[3];
          int i1 = (*i).p[0];
          int i2 = (*i).p[1];
          int i3 = (*i).p[2];
          int i4 = (*i).p[3];
          wrap_mis.getBoxIntegral(integral,
                                  s_mat(i2,i1), s_mat(i3,i2), s_mat(i4,i3),
                                  s_mat(i1,i4), s_mat(i3,i1), s_mat(i4,i2),
                                  m2[i1], m2[i2], m2[i3], m2[i4]);
          if (Options::verb & Verbose::INTEGRALS) {
            (*Options::out) << "I("
                            << i1 << "," << i2 << ","
                            << i3 << "," << i4 << ") = "
                            << integral[0] << integral[1]
                            << integral[2] << endl;
          }
          result_temp[0] += (*i).c[0]*integral[0];
          result_temp[1] += (*i).c[0]*integral[1];
          result_temp[2] += (*i).c[0]*integral[2];
          rational_part_temp +=  - (*i).c[4]/SIX;
        }
        if (Options::verb & Verbose::INTEGRALS)
          (*Options::out) << endl;
      }

      // 3-point
      if (anyTriangle) {
        for (TrianglesIter i = triangles.begin(); i!=triangles.end(); ++i) {
          Complex integral[3];
          int i1 = (*i).p[0];
          int i2 = (*i).p[1];
          int i3 = (*i).p[2];
          wrap_mis.getTriangleIntegral(integral,
                                       s_mat(i2,i1), s_mat(i3,i2),
                                       s_mat(i1,i3),
                                       m2[i1],m2[i2],m2[i3]);
          if (Options::verb & Verbose::INTEGRALS) {
            (*Options::out) << "I(" << i1 << "," << i2 << "," << i3 << ") = "
                            << integral[0] << integral[1]
                            << integral[2] << endl;
          }
          result_temp[0] += (*i).c[0]*integral[0];
          result_temp[1] += (*i).c[0]*integral[1];
          result_temp[2] += (*i).c[0]*integral[2];
          rational_part_temp += HALF*(*i).c[7];
        }
        if (Options::verb & Verbose::INTEGRALS)
          (*Options::out) << endl;
      }

      // 2-point
      if (anyBubble) {
        for (BubblesIter i = bubbles.begin(); i!=bubbles.end(); ++i) {
          int i1 = (*i).p[0];
          int i2 = (*i).p[1];
          Real k = s_mat(i2,i1);
          if ((m2[i1] == ZERO) && (m2[i2] == ZERO)
              && taxicab_norm(k)<INFRARED_EPS)
            continue;
          Complex b11[3], b1[3], b0[3];
          wrap_mis.getRank2BubbleIntegral(b11, b1, b0, k, m2[i1], m2[i2]);
          if (Options::verb & Verbose::INTEGRALS) {
            (*Options::out) << "B11(" << i1 << "," << i2 << ") = "
                            << b11[0] << b11[1] << b11[2] << endl;
            (*Options::out) << " B1(" << i1 << "," << i2 << ") = "
                            << b1[0] << b1[1] << b1[2] << endl;
            (*Options::out) << " B0(" << i1 << "," << i2 << ") = "
                            << b0[0] << b0[1] << b0[2] << endl;
          }
          Complex ke2 = mp(V[i2]-V[i1] ,(*i).e2);
          Complex B06 = -( k-THREE*(m2[i1]+m2[i2]) )/SIX;
          result_temp[0] += (*i).c[0]*b0[0] + (*i).c[1]*ke2*b1[0]
            + (*i).c[2]*ke2*ke2*b11[0];
          result_temp[1] += (*i).c[0]*b0[1] + (*i).c[1]*ke2*b1[1]
            + (*i).c[2]*ke2*ke2*b11[1];
          result_temp[2] += (*i).c[0]*b0[2] + (*i).c[1]*ke2*b1[2]
            + (*i).c[2]*ke2*ke2*b11[2];
          rational_part_temp += B06*(*i).c[9];
        }
        if (Options::verb & Verbose::INTEGRALS)
          (*Options::out) << endl;
      }

      // 1-point
      if (anyTadpole) {
        for (TadpolesIter i = tadpoles.begin(); i!=tadpoles.end(); ++i) {
          int i1 = (*i).p[0];
          if (m2[i1]==ZERO)
            continue;
          Complex integral[3];
          wrap_mis.getTadpoleIntegral(integral, m2[i1]);
          if (Options::verb & Verbose::INTEGRALS) {
            (*Options::out) << "I(" << i1 << ") = "
                            << integral[0] << integral[1]
                            << integral[2] << endl;
          }
          result_temp[0] += (*i).c[0]*integral[0];
          result_temp[1] += (*i).c[0]*integral[1];
          result_temp[2] += (*i).c[0]*integral[2];
        }
        if (Options::verb & Verbose::INTEGRALS)
          (*Options::out) << endl;
      }
    }

    // add to the total
    cut_constr += result_temp[0];
    result_temp[0] += rational_part_temp;
    result[0] += result_temp[0];
    result[1] += result_temp[1];
    result[2] += result_temp[2];


    ////////////
    // Return //
    ////////////

    if (Options::verb) {
      (*Options::out) << endl;
      if (Options::verb & Verbose::RESULT) {
        (*Options::out) << "Partial Result:" << endl;
        (*Options::out) << "eps^0  =" << result_temp[0] << endl;
        (*Options::out) << "eps^-1 =" << result_temp[1] << endl;
        (*Options::out) << "eps^-2 =" << result_temp[2] << endl;
        (*Options::out) << "rat.   =" << rational_part_temp << endl;
        (*Options::out) << endl;
      }
      if (return_val == Amplitude::SUCCESS) {
        (*Options::out) << "ninja::Amplitude is returning SUCCESS" << endl;
      } else {
        (*Options::out) << "ninja::Amplitude is returning TEST_FAILED" << endl;
      }
      (*Options::out) << "\n\n----------------------------"
                      << "----------------------------" << endl;
    }

    return return_val;
  }

  using namespace cuts_utils;

  // don't implement extra-rank here (this undef shouldn't be
  // necessary, it is just a precaution)
#undef NINJA_IMPLEMENTING_X1RANK
  //#include <coefficient_level_subs.cxx>
#include <ninja_implem.cxx>   // the implementation is in ninja_implem.cxx



  // explicit template instantiations
  template int Amplitude<RealMasses>::evaluate(Numerator & num);
  template int Amplitude<ComplexMasses>::evaluate(Numerator & num);
#ifdef NINJA_MASSLESS
  template int Amplitude<Massless>::evaluate(Numerator & num);
#endif


} // namespace ninja

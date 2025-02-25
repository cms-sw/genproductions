// -*- C++ -*- header file, defininng a structure for the cache of
// -*- Master Integrals

#ifndef QUADNINJA_INTEGRAL_CACHE_HH
#define QUADNINJA_INTEGRAL_CACHE_HH

#include <quadsources/ninja_hash_table.hh>

namespace quadninja {
  namespace detail {

    struct  MIsResult {
      Complex data[3];
    };

    struct MIsRank2BubbleResult {
      Complex data11[3];
      Complex data1[3];
      Complex data0[3];
    };


    struct BoxArgsCM {
      Real arg1[6];
      Complex arg2[4];
    };
    inline bool operator== (const BoxArgsCM & x, const BoxArgsCM & y)
    {
      return (((x).arg1[0] == (y).arg1[0]) &&    
              ( (x).arg1[1] == (y).arg1[1] ) &&
              ( (x).arg1[2] == (y).arg1[2] ) &&
              ( (x).arg1[3] == (y).arg1[3] ) &&
              ( (x).arg1[4] == (y).arg1[4] ) &&
              ( (x).arg1[5] == (y).arg1[5] ) &&
              ( (x).arg2[0] == (y).arg2[0] ) &&
              ( (x).arg2[1] == (y).arg2[1] ) &&
              ( (x).arg2[2] == (y).arg2[2] ) &&
              ( (x).arg2[3] == (y).arg2[3] ));
    }

    struct BoxArgsRM {
      Real arg1[6];
      Real arg2[4];
    };
    inline bool operator== (const BoxArgsRM & x, const BoxArgsRM & y)
    {
      return (((x).arg1[0] == (y).arg1[0]) &&    
              ( (x).arg1[1] == (y).arg1[1] ) &&
              ( (x).arg1[2] == (y).arg1[2] ) &&
              ( (x).arg1[3] == (y).arg1[3] ) &&
              ( (x).arg1[4] == (y).arg1[4] ) &&
              ( (x).arg1[5] == (y).arg1[5] ) &&
              ( (x).arg2[0] == (y).arg2[0] ) &&
              ( (x).arg2[1] == (y).arg2[1] ) &&
              ( (x).arg2[2] == (y).arg2[2] ) &&
              ( (x).arg2[3] == (y).arg2[3] ));
    }

    struct BoxArgsNM {
      Real arg1[6];
    };
    inline bool operator== (const BoxArgsNM & x, const BoxArgsNM & y)
    {
      return (((x).arg1[0] == (y).arg1[0]) &&    
              ( (x).arg1[1] == (y).arg1[1] ) &&
              ( (x).arg1[2] == (y).arg1[2] ) &&
              ( (x).arg1[3] == (y).arg1[3] ) &&
              ( (x).arg1[4] == (y).arg1[4] ) &&
              ( (x).arg1[5] == (y).arg1[5] ));
    }

    struct  TriangleArgsCM {
      Real arg1[3];
      Complex arg2[3];
    };
    inline bool
    operator== (const TriangleArgsCM & x, const TriangleArgsCM & y)
    {
      return (((x).arg1[0] == (y).arg1[0]) &&
              ( (x).arg1[1] == (y).arg1[1] ) &&
              ( (x).arg1[2] == (y).arg1[2] ) &&
              ( (x).arg2[0] == (y).arg2[0] ) &&
              ( (x).arg2[1] == (y).arg2[1] ) &&
              ( (x).arg2[2] == (y).arg2[2] ));
    }

    struct TriangleArgsRM {
      Real arg1[3];
      Real arg2[3];
    };
    inline bool
    operator== (const TriangleArgsRM & x, const TriangleArgsRM & y)
    {
      return (((x).arg1[0] == (y).arg1[0]) &&
              ( (x).arg1[1] == (y).arg1[1] ) &&
              ( (x).arg1[2] == (y).arg1[2] ) &&
              ( (x).arg2[0] == (y).arg2[0] ) &&
              ( (x).arg2[1] == (y).arg2[1] ) &&
              ( (x).arg2[2] == (y).arg2[2] ));
    }

    struct  TriangleArgsNM {
      Real arg1[3];
    };
    inline bool
    operator== (const TriangleArgsNM & x, const TriangleArgsNM & y)
    {
      return (((x).arg1[0] == (y).arg1[0]) &&
              ( (x).arg1[1] == (y).arg1[1] ) &&
              ( (x).arg1[2] == (y).arg1[2] ));
    }

    struct BubbleArgsCM {
      Real arg1;
      Complex arg2[2];
    };
    inline bool operator== (const BubbleArgsCM & x, const BubbleArgsCM & y)
    {
      return (((x).arg1 == (y).arg1) &&
              ( (x).arg2[0] == (y).arg2[0] ) &&
              ( (x).arg2[1] == (y).arg2[1] ));
    }

    struct  BubbleArgsRM {
      Real arg1;
      Real arg2[2];
    };
    inline bool operator== (const BubbleArgsRM & x, const BubbleArgsRM & y)
    {
      return (((x).arg1 == (y).arg1) &&
              ( (x).arg2[0] == (y).arg2[0] ) &&
              ( (x).arg2[1] == (y).arg2[1] ));
    }

    struct BubbleArgsNM {
      Real arg1;
    };
    inline bool operator== (const BubbleArgsNM & x, const BubbleArgsNM & y)
    {
      return (((x).arg1 == (y).arg1));
    }

    struct TadpoleArgsCM {
      Complex arg2;
    };
    inline bool operator== (const TadpoleArgsCM & x, const TadpoleArgsCM & y)
    {
      return (( (x).arg2 == (y).arg2 ));
    }

    struct  TadpoleArgsRM {
      Real arg2;
    };
    inline bool operator== (const TadpoleArgsRM & x, const TadpoleArgsRM & y)
    {
      return (( (x).arg2 == (y).arg2 ));
    }

  } // namespace detail


  struct IntegralCache {

    typedef quadninja::detail::MIsResult MIsResult;
    typedef quadninja::detail::MIsRank2BubbleResult MIsRank2BubbleResult;

    HashTable<quadninja::detail::BoxArgsCM,MIsResult> ht_4cm;
    HashTable<quadninja::detail::BoxArgsRM,MIsResult> ht_4rm;
    HashTable<quadninja::detail::TriangleArgsCM,MIsResult> ht_3cm;
    HashTable<quadninja::detail::TriangleArgsRM,MIsResult> ht_3rm;
    HashTable<quadninja::detail::BubbleArgsCM,MIsRank2BubbleResult> ht_2cm;
    HashTable<quadninja::detail::BubbleArgsRM,MIsRank2BubbleResult> ht_2rm;
    HashTable<quadninja::detail::TadpoleArgsCM,MIsResult> ht_1cm;
    HashTable<quadninja::detail::TadpoleArgsRM,MIsResult> ht_1rm;
    HashTable<quadninja::detail::BoxArgsNM,MIsResult> ht_4nm;
    HashTable<quadninja::detail::TriangleArgsNM,MIsResult> ht_3nm;
    HashTable<quadninja::detail::BubbleArgsNM,MIsRank2BubbleResult> ht_2nm;

  };

} // namespace quadninja

#endif // QUADNINJA_INTEGRAL_CACHE_HH

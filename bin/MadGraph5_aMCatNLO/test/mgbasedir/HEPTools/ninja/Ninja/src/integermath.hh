#ifndef __NINJA_INTEGER_MATH_HH__
#define __NINJA_INTEGER_MATH_HH__

namespace ninja_integer_math{
  inline int combinations5(int n){ return n*(n-1)*(n-2)*(n-3)*(n-4)/120; }
  inline int combinations4(int n){ return n*(n-1)*(n-2)*(n-3)/24; }
  inline int combinations3(int n){ return n*(n-1)*(n-2)/6; }
  inline int combinations2(int n){ return n*(n-1)/2; }
}

#endif // __NINJA_INTEGER_MATH_HH__

// RAII for s_matrix object, used internally by Ninja.  If the
// S-matrix was not allocated by the user, this wrapper does the
// necessary allocations and initializations from the (internal)
// kinematics, clearing then the memory in the destructor.  No action
// is done if the S-Matrix was already allocated.

#ifndef S_MAT_WRAPPER_HH
#define S_MAT_WRAPPER_HH

namespace ninja {
  namespace s_matrix_wrapper {

    class WrapSMatrix {

    public:
      WrapSMatrix(SMatrix & s_mat, int n, const RealMomentum pi[])
        : s_mat_(s_mat), was_null_(s_mat.isNull())
      {
        if (was_null_) {
          s_mat.allocate(n);
          s_mat.fillFromKinematics(pi);
        }
      }

      ~WrapSMatrix()
      {
        if (was_null_) {
          s_mat_.clear();
        }
      }

    private:
      SMatrix & s_mat_;
      bool was_null_;
    };

  } // namespace s_matrix_wrapper
} // namespace ninja

#endif // S_MAT_WRAPPER_HH

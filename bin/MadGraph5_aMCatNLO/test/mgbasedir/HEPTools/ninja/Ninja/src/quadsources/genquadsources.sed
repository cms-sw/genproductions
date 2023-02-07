
# generic substitutions
s;#[ ]*include[ ]*<ninja/;#include <quadninja/;g
s/namespace [ ]*ninja/namespace quadninja/g
s/namespace [ ]*samurai/namespace quadsamurai/g
s/ninja::/quadninja::/g
s/samurai::/quadsamurai::/g
s/ NINJA_/ QUADNINJA_/g
s/ninjavholo_/quadninjavholo_/g
s/ninja_tensor_evaluate/quadninja_tensor_evaluate/g
s;//quadninja//;;g
s;!!quadninja!!;;g

# internal header files
s;# *include *<ninja_implem.cxx>;#include <quadsources/ninja_implem.cxx>;g
s;# *include *<polys_implem.cxx>;#include <quadsources/polys_implem.cxx>;g
s;# *include *<basis.hh>;#include <quadsources/basis.hh>;g
s;# *include *<cuts.hh>;#include <quadsources/cuts.hh>;g
s;# *include *<cuts_utils.hh>;#include <quadsources/cuts_utils.hh>;g
s;# *include *<Xcuts.hh>;#include <quadsources/Xcuts.hh>;g
s;# *include *<cuts_utils-inl.hh>;#include <quadsources/cuts_utils-inl.hh>;g
s;# *include *<coefficient_level_subs.hh>;#include <quadsources/coefficient_level_subs.hh>;g
s;# *include *<integermath.hh>;#include <quadsources/integermath.hh>;g
s;# *include *<tmp_utils.hh>;#include <quadsources/tmp_utils.hh>;g
s;# *include *<ninja_scoped_array.hh>;#include <quadsources/ninja_scoped_array.hh>;g
s;# *include *<cuts_vector.hh>;#include <quadsources/cuts_vector.hh>;g
s;# *include *<integral_library_wrapper.hh>;#include <quadsources/integral_library_wrapper.hh>;g
s;# *include *<s_mat_wrapper.hh>;#include <quadsources/s_mat_wrapper.hh>;g
s;# *include *<avholo_decls.hh>;#include <quadsources/avholo_decls.hh>;g
s;# *include *<ninja_hash_table.hh>;#include <quadsources/ninja_hash_table.hh>;g
s;# *include *<integral_cache.hh>;#include <quadsources/integral_cache.hh>;g
s;# *include *<gosam_interface.hh>;#include <quadsources/gosam_interface.hh>;g

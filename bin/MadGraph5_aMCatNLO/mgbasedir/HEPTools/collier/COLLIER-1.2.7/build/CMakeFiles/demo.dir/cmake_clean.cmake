FILE(REMOVE_RECURSE
  "CMakeFiles/demo.dir/demos/demo.f90.o"
  "../demos/demo.pdb"
  "../demos/demo"
)

# Per-language clean rules from dependency scanning.
FOREACH(lang Fortran)
  INCLUDE(CMakeFiles/demo.dir/cmake_clean_${lang}.cmake OPTIONAL)
ENDFOREACH(lang)

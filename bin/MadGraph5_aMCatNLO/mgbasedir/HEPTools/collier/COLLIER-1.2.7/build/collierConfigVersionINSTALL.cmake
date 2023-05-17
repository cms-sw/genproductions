# File: collierConfigVersion.cmake.in
# Author: Jean-Nicolas Lang
# Description: CMake configure file for setting the collier package version
# Last Modified: March 02, 2018

set(PACKAGE_VERSION "1.2.5")
 
# Check whether the requested PACKAGE_FIND_VERSION is compatible
if("${PACKAGE_VERSION}" VERSION_LESS "${PACKAGE_FIND_VERSION}")
  set(PACKAGE_VERSION_COMPATIBLE FALSE)
else()
  set(PACKAGE_VERSION_COMPATIBLE TRUE)
  if ("${PACKAGE_VERSION}" VERSION_EQUAL "${PACKAGE_FIND_VERSION}")
    set(PACKAGE_VERSION_EXACT TRUE)
  endif()
endif()

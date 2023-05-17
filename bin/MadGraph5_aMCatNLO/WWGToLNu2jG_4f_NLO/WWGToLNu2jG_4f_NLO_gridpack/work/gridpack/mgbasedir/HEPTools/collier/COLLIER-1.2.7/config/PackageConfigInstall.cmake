# File: PackageConfigInstall.cmake
# Author: Jean-Nicolas Lang
# Description: Sets up package config files and uninstall target
# Last Modified: March 02, 2018

macro (PACKAGE_CONFIG_INSTALL PNAME PVERSION)

  set(PACKAGE_VERSION ${PVERSION})
  set(LIB_NAME "${PNAME}")
  string(TOUPPER ${LIB_NAME} LIB_NAME_UPPER)

#----------------------#
#  Local config files  #
#----------------------#

  set(LIB_DIR ${LIB_LOCAL_DIR})
  set(INCLUDE_DIR ${INCLUDE_LOCAL_DIR})
  configure_file(${SYSCONFIG_LOCAL_DIR}/PackageConfig.cmake.in
                 "${LIB_LOCAL_DIR}/${LIB_NAME}Config.cmake" @ONLY)
  configure_file(${SYSCONFIG_LOCAL_DIR}/PackageConfigVersion.cmake.in
                 "${LIB_LOCAL_DIR}/${LIB_NAME}ConfigVersion.cmake" @ONLY)

#-------------------------#
#  Config install script  #
#-------------------------#

  set(LIB_DIR ${LIB_INSTALL_DIR})
  set(INCLUDE_DIR ${INCLUDE_INSTALL_DIR})
  configure_file(${SYSCONFIG_LOCAL_DIR}/PackageConfig.cmake.in
                 "${CMAKE_BINARY_DIR}/${LIB_NAME}ConfigINSTALL.cmake" @ONLY)
  configure_file(${SYSCONFIG_LOCAL_DIR}/PackageConfigVersion.cmake.in
                 "${CMAKE_BINARY_DIR}/${LIB_NAME}ConfigVersionINSTALL.cmake" @ONLY)

  install(FILES ${CMAKE_BINARY_DIR}/${LIB_NAME}ConfigINSTALL.cmake
          RENAME  ${LIB_NAME}Config.cmake
          DESTINATION ${SYSCONFIG_INSTALL_DIR})
  install(FILES ${CMAKE_BINARY_DIR}/${LIB_NAME}ConfigVersionINSTALL.cmake
          RENAME  ${LIB_NAME}ConfigVersion.cmake
          DESTINATION ${SYSCONFIG_INSTALL_DIR})

#------------------#
#  Install script  #
#------------------#

  install(TARGETS ${LIB_NAME}
          LIBRARY DESTINATION ${LIB_INSTALL_DIR}
          ARCHIVE DESTINATION ${LIB_INSTALL_DIR}
          PUBLIC_HEADER DESTINATION ${INCLUDE_INSTALL_DIR}
          )

#--------------------#
#  Uninstall script  #
#--------------------#

  CONFIGURE_FILE(
    "${SYSCONFIG_LOCAL_DIR}/CMakeUninstall.cmake.in"
    "${CMAKE_BINARY_DIR}/CMakeUninstall.cmake"
    IMMEDIATE @ONLY)

  ADD_CUSTOM_TARGET(uninstall
    "${CMAKE_COMMAND}" -P "${CMAKE_BINARY_DIR}/CMakeUninstall.cmake") 
endmacro ()

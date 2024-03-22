#!/bin/bash
errfile="${SCRAM_INTwork}/build_error"
rm -f ${errfile}
if [ ! -f ${SCRAM_INTwork}/Makefile ] ; then
  cp ${SCRAM_CONFIGDIR}/SCRAM/GMake/Makefile ${SCRAM_INTwork}/Makefile
  touch -t 198001010100 ${SCRAM_INTwork}/Makefile
fi
(${SCRAM_GMAKE_PATH}gmake -r -f ${SCRAM_INTwork}/Makefile "$@" && [ ! -e ${errfile} ]) || (err=$?; echo "gmake: *** [There are compilation/build errors. Please see the detail log above.] Error $err" && exit $err)

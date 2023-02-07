#! /usr/bin/env python
from __future__ import absolute_import
from __future__ import print_function
import re
import sys
import os
import logging
import six
from six import StringIO

pjoin = os.path.join

_zlib_path     = None
_ma5_path      = None
_mg5_path      = None
_delphes3_path = None
_fastjet_path  = None
_logging_level = logging.INFO
MA5_further_install = True
veto_root      = False
args = sys.argv[1:]

help_msg = \
""" 
Call this python script with the following mandatory arguments:
   --zlib=<zlib_path>               : Path to the zlib library to employ.
   --ma5_path=<ma5_install_path>    : Path to the already untarred MA5 installation
   --mg5_path=<mg5_root_dir>        : Path of MG5 installation
Optional dependency:
   --delphes3=<delphes3_path>       : Path to the Delphes3 installation to be used in MA5.
   --fastjet=<fastjet_dir>          : Path to fastjet
#   --no_MA5_further_install         : Prevent MA5 to install further tools
   --no_root_in_MA5                 : Veto the use of ROOT in MA5
   --logging=<int>                  : Specify MA5 logger level
   --with-XXX[=PATH]                : argument passed to MA5 further installer
   --veto-XXX                       : argument passed to MA5 further installer

"""

def failed_installation():
    error=StringIO()
    import traceback
    traceback.print_exc(file=error)
    print('MadAnalysis5 error was:')
    print('-'*60)
    print(error.getvalue()[:-1])
    print('-'*60)
    print("INSTALLATION STATUS: FAILED")
    sys.exit(1)

ma5_further_options = []
for arg in args:
    try:
        option, value = arg.split('=')
    except ValueError:
        option = arg
        value = None
    if option == '--zlib':
        _zlib_path=value
    elif option == '--ma5_path':
        _ma5_path=value
    elif option == '--mg5_path':
        _mg5_path=value
    elif option == '--delphes3':
        _delphes3_path=value
    elif option == '--logging':
        _logging_level=int(value)
    elif option == '--fastjet':
        if value is None:
            value = True
        ma5_further_options.append(('--with-fastjet',value)) 
    elif option == '--no_root_in_MA5':
        ma5_further_options.append(('--veto-root',True)) 
    elif option == '--no_MA5_further_install':
        MA5_further_install = False
    elif option.startswith(('--with','--veto')):
        if value is None:
            value = True
        ma5_further_options.append((option,value)) 
    else:
        print("Option '%s' not reckognized."%option)
        print(help_msg)
        failed_installation()
        sys.exit(1)

if _zlib_path is None or _ma5_path is None:
        print(help_msg)
        failed_installation()
        sys.exit(1)
else:
    ma5_further_options.append(('--with-zlib', _zlib_path))    


# put additional default
if not any(o[0] in ('--with-fastjet', '--veto-fastjet') for o in ma5_further_options):
    ma5_further_options.append(('--with-fastjet', True))    
if not any(o[0] in ('--with-delphesMA5tune', '--veto-delphesMA5tune','--with-padforMA5tune') for o in ma5_further_options):
    ma5_further_options.append(('--veto-delphesMA5tune', True))
if not any(o[0] in ('--with-delphes', '--veto-delphes','--with-pad') for o in ma5_further_options):
    ma5_further_options.append(('--veto-delphes', True))

# Make sure MA5 is accessible
sys.path.insert(0, _ma5_path)

# Make sure 'MA5_BASE' is available in the environnment variables
if 'MA5_BASE' not in os.environ:
    os.environ['MA5_BASE']=_ma5_path

# Now try to import it.
print(">> Attempting to start MA5 and check the availability of its dependencies...")
try:
    from madanalysis.interpreter.ma5_interpreter import MA5Interpreter
    MA5_interpreter = MA5Interpreter(_ma5_path,LoggerLevel=_logging_level,no_compilation=True)
except Exception as e:
    print("Error: Could not import/start MadAnalysis5 python module: %s."%str(e))
    failed_installation()
    sys.exit(1)

if not MA5_further_install:
    ma5_further_options.extend([('--veto-delphes',True),
                           ('--veto-delphesMA5tune',True)])


    # Now trigger the final compilation of SampleAnalyzer
    #    try:
    #        if not MA5_interpreter.compile():
    #            raise Exception('FailedCompilation')
    #    except Exception as e:
    #        print "Error: MadAnalysis5 failed to compile SampleAnalyzer:\n %s"%str(e)
    #        failed_installation()
    #    print "INSTALLATION STATUS: SUCCESS"
    #    sys.exit(0)

if _delphes3_path is not None:
    print(">> Delphes dependency specified but MadAnalysis5 needs to install its own version. Installing it now...")

#calling the MA5 to specify dependencies
opts= dict([key[2:], value] for key,value in ma5_further_options)

if six.PY3:
    opts['PADForSFS'] = True

try:
    MA5_interpreter.further_install(opts)
except Exception as e:
    print("Error: MadAnalysis5 failed to install some of its dependencies.\n %s"%str(e))
    failed_installation()

# Now trigger the final compilation of SampleAnalyzer
try:
    if not MA5_interpreter.compile():
        raise Exception('FailedCompilation')
except Exception as e:
    print("Error: MadAnalysis5 failed to compile SampleAnalyzer:\n %s"%str(e))
    failed_installation()

print(">> Successful installation of MadAnalysis5.")

# Write a file to indicate the status of the installation
print("INSTALLATION STATUS: SUCCESS")

################################################################################
#
# Copyright (c) 2009 The MadGraph5_aMC@NLO Development team and Contributors
#
# This file is a part of the MadGraph5_aMC@NLO project, an application which 
# automatically generates Feynman diagrams and matrix elements for arbitrary
# high-energy processes in the Standard Model and beyond.
#
# It is subject to the MadGraph5_aMC@NLO license which should accompany this 
# distribution.
#
# For more information, visit madgraph.phys.ucl.ac.be and amcatnlo.web.cern.ch
#
################################################################################

"""A set of functions performing routine administrative I/O tasks."""

import contextlib
import logging
import os
import re
import signal
import subprocess
import sys
import StringIO
import sys
import optparse
import time
import shutil
import traceback
import gzip as ziplib

try:
    # Use in MadGraph
    import madgraph
except Exception, error:
    # Use in MadEvent
    import internal
    from internal import MadGraph5Error, InvalidCmd
    import internal.files as files
    MADEVENT = True    
else:
    from madgraph import MadGraph5Error, InvalidCmd
    import madgraph.iolibs.files as files
    MADEVENT = False


    
logger = logging.getLogger('cmdprint.ext_program')
logger_stderr = logging.getLogger('madevent.misc')
pjoin = os.path.join

#===============================================================================
# Return a warning (if applicable) on the consistency of the current Pythia8
# and MG5_aMC version specified. It is placed here because it should be accessible
# from both madgraph5_interface and madevent_interface
#===============================================================================
def mg5amc_py8_interface_consistency_warning(options):
    """ Check the consistency of the mg5amc_py8_interface installed with
    the current MG5 and Pythia8 versions. """

    return None
    # All this is only relevant is Pythia8 is interfaced to MG5
    if not options['pythia8_path']:
        return None
    
    if not options['mg5amc_py8_interface_path']:
        return \
"""
A Pythia8 path is specified via the option 'pythia8_path' but no path for option
'mg5amc_py8_interface_path' is specified. This means that Pythia8 cannot be used
leading order simulations with MadEvent.
Consider installing the MG5_aMC-PY8 interface with the following command:
 MG5_aMC>install mg5amc_py8_interface
"""
    
    # Retrieve all the on-install and current versions  
    MG5_version_on_install = open(pjoin(options['mg5amc_py8_interface_path'],
                       'MG5AMC_VERSION_ON_INSTALL')).read().replace('\n','')
    if MG5_version_on_install == 'UNSPECIFIED':
        MG5_version_on_install = None
    PY8_version_on_install = open(pjoin(options['mg5amc_py8_interface_path'],
                          'PYTHIA8_VERSION_ON_INSTALL')).read().replace('\n','')
    MG5_curr_version = get_pkg_info()['version']
    try:
        p = subprocess.Popen(['./get_pythia8_version.py',options['pythia8_path']],
                         stdout=subprocess.PIPE, stderr=subprocess.PIPE, 
                                   cwd=options['mg5amc_py8_interface_path'])
        (out, err) = p.communicate()
        out = out.replace('\n','')
        PY8_curr_version = out
        # In order to test that the version is correctly formed, we try to cast
        # it to a float
        float(out)
    except:
        PY8_curr_version = None

    if not MG5_version_on_install is None and not MG5_curr_version is None:
        if MG5_version_on_install != MG5_curr_version:
            return \
"""
The current version of MG5_aMC (v%s) is different than the one active when
installing the 'mg5amc_py8_interface_path' (which was MG5aMC v%s). 
Please consider refreshing the installation of this interface with the command:
 MG5_aMC>install mg5amc_py8_interface
"""%(MG5_curr_version, MG5_version_on_install)

    if not PY8_version_on_install is None and not PY8_curr_version is None:
        if PY8_version_on_install != PY8_curr_version:
            return \
"""
The current version of Pythia8 (v%s) is different than the one active when
installing the 'mg5amc_py8_interface' tool (which was Pythia8 v%s). 
Please consider refreshing the installation of this interface with the command:
 MG5_aMC>install mg5amc_py8_interface
"""%(PY8_curr_version,PY8_version_on_install)

    return None
   


#===============================================================================
# parse_info_str
#===============================================================================
def parse_info_str(fsock):
    """Parse a newline separated list of "param=value" as a dictionnary
    """

    info_dict = {}
    pattern = re.compile("(?P<name>\w*)\s*=\s*(?P<value>.*)",
                         re.IGNORECASE | re.VERBOSE)
    for entry in fsock:
        entry = entry.strip()
        if len(entry) == 0: continue
        m = pattern.match(entry)
        if m is not None:
            info_dict[m.group('name')] = m.group('value')
        else:
            raise IOError, "String %s is not a valid info string" % entry

    return info_dict


def glob(name, path=''):
    """call to glob.glob with automatic security on path"""
    import glob as glob_module
    path = re.sub('(?P<name>\?|\*|\[|\])', '[\g<name>]', path)
    return glob_module.glob(pjoin(path, name))

#===============================================================================
# mute_logger (designed to be a decorator)
#===============================================================================
def mute_logger(names=['madgraph','ALOHA','cmdprint','madevent'], levels=[50,50,50,50]):
    """change the logger level and restore those at their initial value at the
    end of the function decorated."""
    def control_logger(f):
        def restore_old_levels(names, levels):
            for name, level in zip(names, levels):
                log_module = logging.getLogger(name)
                log_module.setLevel(level)            
        
        def f_with_no_logger(self, *args, **opt):
            old_levels = []
            for name, level in zip(names, levels):
                log_module = logging.getLogger(name)
                old_levels.append(log_module.level)
                log_module.setLevel(level)
            try:
                out = f(self, *args, **opt)
                restore_old_levels(names, old_levels)
                return out
            except:
                restore_old_levels(names, old_levels)
                raise
            
        return f_with_no_logger
    return control_logger

#===============================================================================
# get_pkg_info
#===============================================================================
def get_pkg_info(info_str=None):
    """Returns the current version information of the MadGraph5_aMC@NLO package, 
    as written in the VERSION text file. If the file cannot be found, 
    a dictionary with empty values is returned. As an option, an info
    string can be passed to be read instead of the file content.
    """

    if info_str:
        info_dict = parse_info_str(StringIO.StringIO(info_str))

    elif MADEVENT:
        info_dict ={}
        info_dict['version'] = open(pjoin(internal.__path__[0],'..','..','MGMEVersion.txt')).read().strip()
        info_dict['date'] = '20xx-xx-xx'                        
    else:
        info_dict = files.read_from_file(os.path.join(madgraph.__path__[0],
                                                  "VERSION"),
                                                  parse_info_str, 
                                                  print_error=False)
        
    return info_dict

#===============================================================================
# get_time_info
#===============================================================================
def get_time_info():
    """Returns the present time info for use in MG5 command history header.
    """

    creation_time = time.asctime() 
    time_info = {'time': creation_time,
                 'fill': ' ' * (26 - len(creation_time))}

    return time_info

#===============================================================================
# Find the subdirectory which includes the files ending with a given extension 
#===============================================================================
def find_includes_path(start_path, extension):
    """Browse the subdirectories of the path 'start_path' and returns the first
    one found which contains at least one file ending with the string extension
    given in argument."""
    
    subdirs=[pjoin(start_path,dir) for dir in os.listdir(start_path)]
    for subdir in subdirs:
        if os.path.isfile(subdir):
            if os.path.basename(subdir).endswith(extension):
                return start_path
        elif os.path.isdir(subdir):
            path = find_includes_path(subdir, extension)
            if path:
                return path
    return None

#===============================================================================
# Given the path of a ninja installation, this function determines if it 
# supports quadruple precision or not. 
#===============================================================================
def get_ninja_quad_prec_support(ninja_lib_path):
    """ Get whether ninja supports quad prec in different ways"""
    
    # First try with the ninja-config executable if present
    ninja_config = os.path.abspath(pjoin(
                                 ninja_lib_path,os.pardir,'bin','ninja-config'))
    if os.path.exists(ninja_config):
        try:    
            p = Popen([ninja_config, '-quadsupport'], stdout=subprocess.PIPE, 
                                                         stderr=subprocess.PIPE)
            output, error = p.communicate()
            return 'TRUE' in output.upper()
        except Exception:
            pass
    
    # If no ninja-config is present, then simply use the presence of
    # 'quadninja' in the include
    return False

#===============================================================================
# find a executable
#===============================================================================
def which(program):
    def is_exe(fpath):
        return os.path.exists(fpath) and os.access(\
                                               os.path.realpath(fpath), os.X_OK)

    if not program:
        return None

    fpath, fname = os.path.split(program)
    if fpath:
        if is_exe(program):
            return program
    else:
        for path in os.environ["PATH"].split(os.pathsep):
            exe_file = os.path.join(path, program)
            if is_exe(exe_file):
                return exe_file
    return None

def has_f2py():
    has_f2py = False
    if which('f2py'):
        has_f2py = True
    elif sys.version_info[1] == 6:
        if which('f2py-2.6'):
            has_f2py = True
        elif which('f2py2.6'):
            has_f2py = True                 
    else:
        if which('f2py-2.7'):
            has_f2py = True 
        elif which('f2py2.7'):
            has_f2py = True  
    return has_f2py       
        
#===============================================================================
#  Activate dependencies if possible. Mainly for tests
#===============================================================================

def deactivate_dependence(dependency, cmd=None, log = None):
    """ Make sure to turn off some dependency of MG5aMC. """
    
    def tell(msg):
        if log == 'stdout':
            print msg
        elif callable(log):
            log(msg)
    

    if dependency in ['pjfry','golem','samurai','ninja']:
        if cmd.options[dependency] not in ['None',None,'']:
            tell("Deactivating MG5_aMC dependency '%s'"%dependency)
            cmd.options[dependency] = None

def activate_dependence(dependency, cmd=None, log = None, MG5dir=None):
    """ Checks whether the specfieid MG dependency can be activated if it was
    not turned off in MG5 options."""
    
    def tell(msg):
        if log == 'stdout':
            print msg
        elif callable(log):
            log(msg)

    if cmd is None:
        cmd = MGCmd.MasterCmd()

    if dependency=='pjfry':
        if cmd.options['pjfry'] in ['None',None,''] or \
         (cmd.options['pjfry'] == 'auto' and which_lib('libpjfry.a') is None) or\
          which_lib(pjoin(cmd.options['pjfry'],'libpjfry.a')) is None:
            tell("Installing PJFry...")
            cmd.do_install('PJFry')

    if dependency=='golem':
        if cmd.options['golem'] in ['None',None,''] or\
         (cmd.options['golem'] == 'auto' and which_lib('libgolem.a') is None) or\
         which_lib(pjoin(cmd.options['golem'],'libgolem.a')) is None:
            tell("Installing Golem95...")
            cmd.do_install('Golem95')
    
    if dependency=='samurai':
        raise MadGraph5Error, 'Samurai cannot yet be automatically installed.' 

    if dependency=='ninja':
        if cmd.options['ninja'] in ['None',None,''] or\
         (cmd.options['ninja'] == './HEPTools/lib' and not MG5dir is None and\
         which_lib(pjoin(MG5dir,cmd.options['ninja'],'libninja.a')) is None):
            tell("Installing ninja...")
            cmd.do_install('ninja')
 
#===============================================================================
# find a library
#===============================================================================
def which_lib(lib):
    def is_lib(fpath):
        return os.path.exists(fpath) and os.access(fpath, os.R_OK)

    if not lib:
        return None

    fpath, fname = os.path.split(lib)
    if fpath:
        if is_lib(lib):
            return lib
    else:
        locations = sum([os.environ[env_path].split(os.pathsep) for env_path in
           ["DYLD_LIBRARY_PATH","LD_LIBRARY_PATH","LIBRARY_PATH","PATH"] 
                                                  if env_path in os.environ],[])
        for path in locations:
            lib_file = os.path.join(path, lib)
            if is_lib(lib_file):
                return lib_file
    return None

#===============================================================================
# Return Nice display for a random variable
#===============================================================================
def nice_representation(var, nb_space=0):
    """ Return nice information on the current variable """
    
    #check which data to put:
    info = [('type',type(var)),('str', var)]
    if hasattr(var, 'func_doc'):
        info.append( ('DOC', var.func_doc) )
    if hasattr(var, '__doc__'):
        info.append( ('DOC', var.__doc__) )
    if hasattr(var, '__dict__'):
        info.append( ('ATTRIBUTE', var.__dict__.keys() ))
    
    spaces = ' ' * nb_space

    outstr=''
    for name, value in info:
        outstr += '%s%3s : %s\n' % (spaces,name, value)

    return outstr

#
# Decorator for re-running a crashing function automatically.
#
wait_once = False
def multiple_try(nb_try=5, sleep=20):

    def deco_retry(f):
        def deco_f_retry(*args, **opt):
            for i in range(nb_try):
                try:
                    return f(*args, **opt)
                except KeyboardInterrupt:
                    raise
                except Exception, error:
                    global wait_once
                    if not wait_once:
                        text = """Start waiting for update. (more info in debug mode)"""
                        logger.info(text)
                        logger_stderr.debug('fail to do %s function with %s args. %s try on a max of %s (%s waiting time)' %
                                 (str(f), ', '.join([str(a) for a in args]), i+1, nb_try, sleep * (i+1)))
                        logger_stderr.debug('error is %s' % str(error))
                        if __debug__: logger_stderr.debug('and occurred at :'+traceback.format_exc())
                    wait_once = True
                    time.sleep(sleep * (i+1))

            if __debug__:
                raise
            raise error.__class__, '[Fail %i times] \n %s ' % (i+1, error)
        return deco_f_retry
    return deco_retry

#===============================================================================
# helper for scan. providing a nice formatted string for the scan name
#===============================================================================
def get_scan_name(first, last):
    """return a name of the type xxxx[A-B]yyy
        where xxx and yyy are the common part between the two names.
    """
    
    # find the common string at the beginning     
    base = [first[i] for i in range(len(first)) if first[:i+1] == last[:i+1]]
    # remove digit even if in common
    while base and base[0].isdigit():
        base = base[1:] 
    # find the common string at the end 
    end = [first[-(i+1)] for i in range(len(first)) if first[-(i+1):] == last[-(i+1):]]
    # remove digit even if in common    
    while end and end[-1].isdigit():
        end = end[:-1] 
    end.reverse()
    #convert to string
    base, end = ''.join(base), ''.join(end)
    if end:
        name = "%s[%s-%s]%s" % (base, first[len(base):-len(end)], last[len(base):-len(end)],end)
    else:
        name = "%s[%s-%s]%s" % (base, first[len(base):], last[len(base):],end)
    return name

#===============================================================================
# Compiler which returns smart output error in case of trouble
#===============================================================================
def compile(arg=[], cwd=None, mode='fortran', job_specs = True, nb_core=1 ,**opt):
    """compile a given directory"""

    if 'nocompile' in opt:
        if opt['nocompile'] == True:
            if not arg:
                return
            if cwd:
                executable = pjoin(cwd, arg[0])
            else:
                executable = arg[0]
            if os.path.exists(executable):
                return
        del opt['nocompile']

    cmd = ['make']
    try:
        if nb_core > 1:
            cmd.append('-j%s' % nb_core)
        cmd += arg
        p = subprocess.Popen(cmd, stdout=subprocess.PIPE, 
                             stderr=subprocess.STDOUT, cwd=cwd, **opt)
        (out, err) = p.communicate()
    except OSError, error:
        if cwd and not os.path.exists(cwd):
            raise OSError, 'Directory %s doesn\'t exists. Impossible to run make' % cwd
        else:
            error_text = "Impossible to compile %s directory\n" % cwd
            error_text += "Trying to launch make command returns:\n"
            error_text += "    " + str(error) + "\n"
            error_text += "In general this means that your computer is not able to compile."
            if sys.platform == "darwin":
                error_text += "Note that MacOSX doesn\'t have gmake/gfortan install by default.\n"
                error_text += "Xcode3 contains those required programs"
            raise MadGraph5Error, error_text

    if p.returncode:
        # Check that makefile exists
        if not cwd:
            cwd = os.getcwd()
        all_file = [f.lower() for f in os.listdir(cwd)]
        if 'makefile' not in all_file:
            raise OSError, 'no makefile present in %s' % os.path.realpath(cwd)

        if mode == 'fortran' and  not (which('g77') or which('gfortran')):
            error_msg = 'A fortran compiler (g77 or gfortran) is required to create this output.\n'
            error_msg += 'Please install g77 or gfortran on your computer and retry.'
            raise MadGraph5Error, error_msg
        elif mode == 'cpp' and not which('g++'):            
            error_msg ='A C++ compiler (g++) is required to create this output.\n'
            error_msg += 'Please install g++ (which is part of the gcc package)  on your computer and retry.'
            raise MadGraph5Error, error_msg

        # Check if this is due to the need of gfortran 4.6 for quadruple precision
        if any(tag.upper() in out.upper() for tag in ['real(kind=16)','real*16',
            'complex*32']) and mode == 'fortran' and not \
                             ''.join(get_gfortran_version().split('.')) >= '46':
            if not which('gfortran'):
                raise MadGraph5Error, 'The fortran compiler gfortran v4.6 or later '+\
                  'is required to compile %s.\nPlease install it and retry.'%cwd
            else:
                logger_stderr.error('ERROR, you could not compile %s because'%cwd+\
             ' your version of gfortran is older than 4.6. MadGraph5_aMC@NLO will carry on,'+\
                              ' but will not be able to compile an executable.')
                return p.returncode
        # Other reason
        error_text = 'A compilation Error occurs '
        if cwd:
            error_text += 'when trying to compile %s.\n' % cwd
        error_text += 'The compilation fails with the following output message:\n'
        error_text += '    '+out.replace('\n','\n    ')+'\n'
        error_text += 'Please try to fix this compilations issue and retry.\n'
        error_text += 'Help might be found at https://answers.launchpad.net/mg5amcnlo.\n'
        error_text += 'If you think that this is a bug, you can report this at https://bugs.launchpad.net/mg5amcnlo'
        raise MadGraph5Error, error_text
    return p.returncode

def get_gfortran_version(compiler='gfortran'):
    """ Returns the gfortran version as a string.
        Returns '0' if it failed."""
    try:    
        p = Popen([compiler, '-dumpversion'], stdout=subprocess.PIPE, 
                    stderr=subprocess.PIPE)
        output, error = p.communicate()
        version_finder=re.compile(r"(?P<version>(\d.)*\d)")
        version = version_finder.search(output).group('version')
        return version
    except Exception:
        return '0'

def mod_compilator(directory, new='gfortran', current=None, compiler_type='gfortran'):
    #define global regular expression
    if type(directory)!=list:
        directory=[directory]

    #search file
    file_to_change=find_makefile_in_dir(directory)
    if compiler_type == 'gfortran':
        comp_re = re.compile('^(\s*)FC\s*=\s*(.+)\s*$')
        var = 'FC'
    elif compiler_type == 'cpp':
        comp_re = re.compile('^(\s*)CXX\s*=\s*(.+)\s*$')
        var = 'CXX'
    else:
        MadGraph5Error, 'Unknown compiler type: %s' % compiler_type

    mod = False
    for name in file_to_change:
        lines = open(name,'r').read().split('\n')
        for iline, line in enumerate(lines):
            result = comp_re.match(line)
            if result:
                if new != result.group(2) and '$' not in result.group(2):
                    mod = True
                    lines[iline] = result.group(1) + var + "=" + new
            elif compiler_type == 'gfortran' and line.startswith('DEFAULT_F_COMPILER'):
                lines[iline] = "DEFAULT_F_COMPILER = %s" % new
            elif compiler_type == 'cpp' and line.startswith('DEFAULT_CPP_COMPILER'):    
                lines[iline] = "DEFAULT_CPP_COMPILER = %s" % new
                
        if mod:
            open(name,'w').write('\n'.join(lines))
            # reset it to change the next file
            mod = False

#===============================================================================
# mute_logger (designed to work as with statement)
#===============================================================================
class MuteLogger(object):
    """mute_logger (designed to work as with statement),
       files allow to redirect the output of the log to a given file.
    """

    def __init__(self, names, levels, files=None, **opt):
        assert isinstance(names, list)
        assert isinstance(names, list)
        
        self.names = names
        self.levels = levels
        if isinstance(files, list):
            self.files = files
        else:
            self.files = [files] * len(names)
        self.logger_saved_info = {}
        self.opts = opt

    def __enter__(self):
        old_levels = []
        for name, level, path in zip(self.names, self.levels, self.files):
            if path:
                self.setup_logFile_for_logger(path, name, **self.opts)
            log_module = logging.getLogger(name)
            old_levels.append(log_module.level)
            log_module = logging.getLogger(name)
            log_module.setLevel(level)
        self.levels = old_levels
        
    def __exit__(self, ctype, value, traceback ):
        for name, level, path, level in zip(self.names, self.levels, self.files, self.levels):
            if 'keep' in self.opts and not self.opts['keep']:
                self.restore_logFile_for_logger(name, level, path=path)
            else:
                self.restore_logFile_for_logger(name, level)
            
            log_module = logging.getLogger(name)
            log_module.setLevel(level)         
        
    def setup_logFile_for_logger(self, path, full_logname, **opts):
        """ Setup the logger by redirecting them all to logfiles in tmp """
        
        logs = full_logname.split('.')
        lognames = [ '.'.join(logs[:(len(logs)-i)]) for i in\
                                            range(len(full_logname.split('.')))]
        for logname in lognames:
            try:
                os.remove(path)
            except Exception, error:
                pass
            my_logger = logging.getLogger(logname)
            hdlr = logging.FileHandler(path)            
            # I assume below that the orders of the handlers in my_logger.handlers
            # remains the same after having added/removed the FileHandler
            self.logger_saved_info[logname] = [hdlr, my_logger.handlers]
            #for h in my_logger.handlers:
            #    h.setLevel(logging.CRITICAL)
            for old_hdlr in list(my_logger.handlers):
                my_logger.removeHandler(old_hdlr)
            my_logger.addHandler(hdlr)
            #my_logger.setLevel(level)
            my_logger.debug('Log of %s' % logname)

    def restore_logFile_for_logger(self, full_logname, level, path=None, **opts):
        """ Setup the logger by redirecting them all to logfiles in tmp """
        
        logs = full_logname.split('.')
        lognames = [ '.'.join(logs[:(len(logs)-i)]) for i in\
                                            range(len(full_logname.split('.')))]
        for logname in lognames:
            if path:
                try:
                    os.remove(path)
                except Exception, error:
                    pass
            my_logger = logging.getLogger(logname)
            if logname in self.logger_saved_info:
                my_logger.removeHandler(self.logger_saved_info[logname][0])
                for old_hdlr in self.logger_saved_info[logname][1]:
                    my_logger.addHandler(old_hdlr)
            else:
                my_logger.setLevel(level)
        
            #for i, h in enumerate(my_logger.handlers):
            #    h.setLevel(cls.logger_saved_info[logname][2][i])

nb_open =0
@contextlib.contextmanager
def stdchannel_redirected(stdchannel, dest_filename):
    """                                                                                                                                                                                                     
    A context manager to temporarily redirect stdout or stderr                                                                                                                                              
                                                                                                                                                                                                            
    e.g.:                                                                                                                                                                                                   
                                                                                                                                                                                                            
                                                                                                                                                                                                            
    with stdchannel_redirected(sys.stderr, os.devnull):                                                                                                                                                     
        if compiler.has_function('clock_gettime', libraries=['rt']):                                                                                                                                        
            libraries.append('rt')                                                                                                                                                                          
    """

    try:
        oldstdchannel = os.dup(stdchannel.fileno())
        dest_file = open(dest_filename, 'w')
        os.dup2(dest_file.fileno(), stdchannel.fileno())
        yield
    finally:
        if oldstdchannel is not None:
            os.dup2(oldstdchannel, stdchannel.fileno())
            os.close(oldstdchannel)
        if dest_file is not None:
            dest_file.close()
        
def get_open_fds():
    '''
    return the number of open file descriptors for current process

    .. warning: will only work on UNIX-like os-es.
    '''
    import subprocess
    import os

    pid = os.getpid()
    procs = subprocess.check_output( 
        [ "lsof", '-w', '-Ff', "-p", str( pid ) ] )
    nprocs = filter( 
            lambda s: s and s[ 0 ] == 'f' and s[1: ].isdigit(),
            procs.split( '\n' ) )
        
    return nprocs

def detect_if_cpp_compiler_is_clang(cpp_compiler):
    """ Detects whether the specified C++ compiler is clang."""
    
    try:
        p = Popen([cpp_compiler, '--version'], stdout=subprocess.PIPE, 
                    stderr=subprocess.PIPE)
        output, error = p.communicate()
    except Exception, error:
        # Cannot probe the compiler, assume not clang then
        return False
    return 'LLVM' in output

def detect_cpp_std_lib_dependence(cpp_compiler):
    """ Detects if the specified c++ compiler will normally link against the C++
    standard library -lc++ or -libstdc++."""

    is_clang = detect_if_cpp_compiler_is_clang(cpp_compiler)
    if is_clang:
        try:
            import platform
            v, _,_ = platform.mac_ver()
            if not v:
                # We will not attempt to support clang elsewhere than on macs, so
                # we venture a guess here.
                return '-lc++'
            else:
                v = float(v.rsplit('.')[1])
                if v >= 9:
                   return '-lc++'
                else:
                   return '-lstdc++'
        except:
            return '-lstdc++'
    return '-lstdc++'

def detect_current_compiler(path, compiler_type='fortran'):
    """find the current compiler for the current directory"""
    
#    comp = re.compile("^\s*FC\s*=\s*(\w+)\s*")
#   The regular expression below allows for compiler definition with absolute path
    if compiler_type == 'fortran':
        comp = re.compile("^\s*FC\s*=\s*([\w\/\\.\-]+)\s*")
    elif compiler_type == 'cpp':
        comp = re.compile("^\s*CXX\s*=\s*([\w\/\\.\-]+)\s*")
    else:
        MadGraph5Error, 'Unknown compiler type: %s' % compiler_type

    for line in open(path):
        if comp.search(line):
            compiler = comp.search(line).groups()[0]
            return compiler
        elif compiler_type == 'fortran' and line.startswith('DEFAULT_F_COMPILER'):
            return line.split('=')[1].strip()
        elif compiler_type == 'cpp' and line.startswith('DEFAULT_CPP_COMPILER'):
            return line.split('=')[1].strip()

def find_makefile_in_dir(directory):
    """ return a list of all file starting with makefile in the given directory"""

    out=[]
    #list mode
    if type(directory)==list:
        for name in directory:
            out+=find_makefile_in_dir(name)
        return out

    #single mode
    for name in os.listdir(directory):
        if os.path.isdir(directory+'/'+name):
            out+=find_makefile_in_dir(directory+'/'+name)
        elif os.path.isfile(directory+'/'+name) and name.lower().startswith('makefile'):
            out.append(directory+'/'+name)
        elif os.path.isfile(directory+'/'+name) and name.lower().startswith('make_opt'):
            out.append(directory+'/'+name)
    return out

def rm_old_compile_file():

    # remove all the .o files
    os.path.walk('.', rm_file_extension, '.o')
    
    # remove related libraries
    libraries = ['libblocks.a', 'libgeneric_mw.a', 'libMWPS.a', 'libtools.a', 'libdhelas3.a',
                 'libdsample.a', 'libgeneric.a', 'libmodel.a', 'libpdf.a', 'libdhelas3.so', 'libTF.a', 
                 'libdsample.so', 'libgeneric.so', 'libmodel.so', 'libpdf.so']
    lib_pos='./lib'
    [os.remove(os.path.join(lib_pos, lib)) for lib in libraries \
                                 if os.path.exists(os.path.join(lib_pos, lib))]


def format_time(n_secs):
    m, s = divmod(n_secs, 60)
    h, m = divmod(m, 60)
    d, h = divmod(h, 24)
    if d > 0:
        return "%d day%s,%dh%02dm%02ds" % (d,'' if d<=1 else 's',h, m, s)
    elif h > 0:
        return "%dh%02dm%02ds" % (h, m, s)
    elif m > 0:
        return "%dm%02ds" % (m, s)                
    else:
        return "%d second%s" % (s, '' if s<=1 else 's')   

def rm_file_extension( ext, dirname, names):

    [os.remove(os.path.join(dirname, name)) for name in names if name.endswith(ext)]



def multiple_replacer(*key_values):
    replace_dict = dict(key_values)
    replacement_function = lambda match: replace_dict[match.group(0)]
    pattern = re.compile("|".join([re.escape(k) for k, v in key_values]), re.M)
    return lambda string: pattern.sub(replacement_function, string)

def multiple_replace(string, *key_values):
    return multiple_replacer(*key_values)(string)

# Control
def check_system_error(value=1):
    def deco_check(f):
        def deco_f(arg, *args, **opt):
            try:
                return f(arg, *args, **opt)
            except OSError, error:
                logger.debug('try to recover from %s' % error)
                if isinstance(arg, list):
                    prog =  arg[0]
                else:
                    prog = arg[0]
                
                # Permission denied
                if error.errno == 13:     
                    if os.path.exists(prog):
                        os.system('chmod +x %s' % prog)
                    elif 'cwd' in opt and opt['cwd'] and \
                                       os.path.isfile(pjoin(opt['cwd'],arg[0])):
                        os.system('chmod +x %s' % pjoin(opt['cwd'],arg[0]))
                    return f(arg, *args, **opt)
                # NO such file or directory
                elif error.errno == 2:
                    # raise a more meaningfull error message
                    raise Exception, '%s fails with no such file or directory' \
                                                                           % arg            
                else:
                    raise
        return deco_f
    return deco_check


@check_system_error()
def call(arg, *args, **opt):
    """nice way to call an external program with nice error treatment"""
    try:
        return subprocess.call(arg, *args, **opt)
    except OSError:
        arg[0] = './%s' % arg[0]
        return subprocess.call(arg, *args, **opt)
        
@check_system_error()
def Popen(arg, *args, **opt):
    """nice way to call an external program with nice error treatment"""
    return subprocess.Popen(arg, *args, **opt)

@multiple_try()
def mult_try_open(filepath, *args, **opt):
    """try to open a file with multiple try to ensure that filesystem is sync"""  
    return open(filepath, *args, ** opt)


################################################################################
# TAIL FUNCTION
################################################################################
def tail(f, n, offset=None):
    """Reads a n lines from f with an offset of offset lines.  The return
    value is a tuple in the form ``lines``.
    """
    avg_line_length = 74
    to_read = n + (offset or 0)

    while 1:
        try:
            f.seek(-(avg_line_length * to_read), 2)
        except IOError:
            # woops.  apparently file is smaller than what we want
            # to step back, go to the beginning instead
            f.seek(0)
        pos = f.tell()
        lines = f.read().splitlines()
        if len(lines) >= to_read or pos == 0:
            return lines[-to_read:offset and -offset or None]
        avg_line_length *= 1.3
        avg_line_length = int(avg_line_length)

################################################################################
# LAST LINE FUNCTION
################################################################################
def get_last_line(fsock):
    """return the last line of a file"""
    
    return tail(fsock, 1)[0]
    
class BackRead(file):
    """read a file returning the lines in reverse order for each call of readline()
This actually just reads blocks (4096 bytes by default) of data from the end of
the file and returns last line in an internal buffer."""


    def readline(self):
        """ readline in a backward way """
        
        while len(self.data) == 1 and ((self.blkcount * self.blksize) < self.size):
          self.blkcount = self.blkcount + 1
          line = self.data[0]
          try:
            self.seek(-self.blksize * self.blkcount, 2) # read from end of file
            self.data = (self.read(self.blksize) + line).split('\n')
          except IOError:  # can't seek before the beginning of the file
            self.seek(0)
            data = self.read(self.size - (self.blksize * (self.blkcount-1))) + line
            self.data = data.split('\n')
    
        if len(self.data) == 0:
          return ""
    
        line = self.data.pop()
        return line + '\n'

    def __init__(self, filepos, blksize=4096):
        """initialize the internal structures"""

        # get the file size
        self.size = os.stat(filepos)[6]
        # how big of a block to read from the file...
        self.blksize = blksize
        # how many blocks we've read
        self.blkcount = 1
        file.__init__(self, filepos, 'rb')
        # if the file is smaller than the blocksize, read a block,
        # otherwise, read the whole thing...
        if self.size > self.blksize:
          self.seek(-self.blksize * self.blkcount, 2) # read from end of file
        self.data = self.read(self.blksize).split('\n')
        # strip the last item if it's empty...  a byproduct of the last line having
        # a newline at the end of it
        if not self.data[-1]:
          self.data.pop()
        
    def next(self):
        line = self.readline()
        if line:
            return line
        else:
            raise StopIteration


def write_PS_input(filePath, PS):
    """ Write out in file filePath the PS point to be read by the MadLoop."""
    try:
        PSfile = open(filePath, 'w')
        # Add a newline in the end as the implementation fortran 'read'
        # command on some OS is problematic if it ends directly with the
        # floating point number read.

        PSfile.write('\n'.join([' '.join(['%.16E'%pi for pi in p]) \
                                                             for p in PS])+'\n')
        PSfile.close()
    except Exception:
        raise MadGraph5Error, 'Could not write out the PS point to file %s.'\
                                                                  %str(filePath)

def format_timer(running_time):
    """ return a nicely string representing the time elapsed."""
    if running_time < 2e-2:
        running_time = running_time = 'current time: %02dh%02d' % (time.localtime().tm_hour, time.localtime().tm_min) 
    elif running_time < 10:
        running_time = ' %.2gs ' % running_time
    elif 60 > running_time >= 10:
        running_time = ' %.3gs ' % running_time
    elif 3600 > running_time >= 60:
        running_time = ' %im %is ' % (running_time // 60, int(running_time % 60))
    else:
        running_time = ' %ih %im ' % (running_time // 3600, (running_time//60 % 60))
    return running_time
    

#===============================================================================
# TMP_directory (designed to work as with statement)
#===============================================================================
class TMP_directory(object):
    """create a temporary directory and ensure this one to be cleaned.
    """

    def __init__(self, suffix='', prefix='tmp', dir=None):
        self.nb_try_remove = 0
        import tempfile   
        self.path = tempfile.mkdtemp(suffix, prefix, dir)

    
    def __exit__(self, ctype, value, traceback ):
        #True only for debugging:
        if False and isinstance(value, Exception):
            sprint("Directory %s not cleaned. This directory can be removed manually" % self.path)
            return False
        try:
            shutil.rmtree(self.path)
        except OSError:
            self.nb_try_remove += 1
            if self.nb_try_remove < 3:
                time.sleep(10)
                self.__exit__(ctype, value, traceback)
            else:
                logger.warning("Directory %s not completely cleaned. This directory can be removed manually" % self.path)
        
    def __enter__(self):
        return self.path
    
class TMP_variable(object):
    """create a temporary directory and ensure this one to be cleaned.
    """

    def __init__(self, cls, attribute, value):
        
        self.old_value = getattr(cls, attribute)
        self.cls = cls
        self.attribute = attribute
        setattr(self.cls, self.attribute, value)
    
    def __exit__(self, ctype, value, traceback ):

        setattr(self.cls, self.attribute, self.old_value)
        
    def __enter__(self):
        return self.old_value 
    
#
# GUNZIP/GZIP
#
def gunzip(path, keep=False, stdout=None):
    """ a standard replacement for os.system('gunzip -f %s.gz ' % event_path)"""

    if not path.endswith(".gz"):
        if os.path.exists("%s.gz" % path):
            path = "%s.gz" % path
        else:
            raise Exception, "%(path)s does not finish by .gz and the file %(path)s.gz does not exists" %\
                              {"path": path}         

    
    #for large file (>1G) it is faster and safer to use a separate thread
    if os.path.getsize(path) > 1e8:
        if stdout:
            os.system('gunzip -c %s > %s' % (path, stdout))
        else:
            os.system('gunzip  %s' % path) 
        return 0
    
    if not stdout:
        stdout = path[:-3]
    try:
        gfile = ziplib.open(path, "r")
    except IOError:
        raise
    else:    
        try:    
            open(stdout,'w').write(gfile.read())
        except IOError:
            # this means that the file is actually not gzip
            if stdout == path:
                return
            else:
                files.cp(path, stdout)
            
    if not keep:
        os.remove(path)
    return 0

def gzip(path, stdout=None, error=True, forceexternal=False):
    """ a standard replacement for os.system('gzip %s ' % path)"""
 
    #for large file (>1G) it is faster and safer to use a separate thread
    if os.path.getsize(path) > 1e9 or forceexternal:
        call(['gzip', '-f', path])
        if stdout:
            if not stdout.endswith(".gz"):
                stdout = "%s.gz" % stdout
            shutil.move('%s.gz' % path, stdout)
        return
    
    if not stdout:
        stdout = "%s.gz" % path
    elif not stdout.endswith(".gz"):
        stdout = "%s.gz" % stdout

    try:
        ziplib.open(stdout,"w").write(open(path).read())
    except OverflowError:
        gzip(path, stdout, error=error, forceexternal=True)
    except Exception:
        if error:
            raise
    else:
        os.remove(path)
    
#
# Global function to open supported file types
#
class open_file(object):
    """ a convinient class to open a file """
    
    web_browser = None
    eps_viewer = None
    text_editor = None 
    configured = False
    
    def __init__(self, filename):
        """open a file"""
        
        # Check that the class is correctly configure
        if not self.configured:
            self.configure()
        
        try:
            extension = filename.rsplit('.',1)[1]
        except IndexError:
            extension = ''   
    
    
        # dispatch method
        if extension in ['html','htm','php']:
            self.open_program(self.web_browser, filename, background=True)
        elif extension in ['ps','eps']:
            self.open_program(self.eps_viewer, filename, background=True)
        else:
            self.open_program(self.text_editor,filename, mac_check=False)
            # mac_check to False avoid to use open cmd in mac
    
    @classmethod
    def configure(cls, configuration=None):
        """ configure the way to open the file """
         
        cls.configured = True
        
        # start like this is a configuration for mac
        cls.configure_mac(configuration)
        if sys.platform == 'darwin':
            return # done for MAC
        
        # on Mac some default (eps/web) might be kept on None. This is not
        #suitable for LINUX which doesn't have open command.
        
        # first for eps_viewer
        if not cls.eps_viewer:
           cls.eps_viewer = cls.find_valid(['evince','gv', 'ggv'], 'eps viewer') 
            
        # Second for web browser
        if not cls.web_browser:
            cls.web_browser = cls.find_valid(
                                    ['firefox', 'chrome', 'safari','opera'], 
                                    'web browser')

    @classmethod
    def configure_mac(cls, configuration=None):
        """ configure the way to open a file for mac """
    
        if configuration is None:
            configuration = {'text_editor': None,
                             'eps_viewer':None,
                             'web_browser':None}
        
        for key in configuration:
            if key == 'text_editor':
                # Treat text editor ONLY text base editor !!
                if configuration[key]:
                    program = configuration[key].split()[0]                    
                    if not which(program):
                        logger.warning('Specified text editor %s not valid.' % \
                                                             configuration[key])
                    else:
                        # All is good
                        cls.text_editor = configuration[key]
                        continue
                #Need to find a valid default
                if os.environ.has_key('EDITOR'):
                    cls.text_editor = os.environ['EDITOR']
                else:
                    cls.text_editor = cls.find_valid(
                                        ['vi', 'emacs', 'vim', 'gedit', 'nano'],
                                         'text editor')
              
            elif key == 'eps_viewer':
                if configuration[key]:
                    cls.eps_viewer = configuration[key]
                    continue
                # else keep None. For Mac this will use the open command.
            elif key == 'web_browser':
                if configuration[key]:
                    cls.web_browser = configuration[key]
                    continue
                # else keep None. For Mac this will use the open command.

    @staticmethod
    def find_valid(possibility, program='program'):
        """find a valid shell program in the list"""
        
        for p in possibility:
            if which(p):
                logger.info('Using default %s \"%s\". ' % (program, p) + \
                             'Set another one in ./input/mg5_configuration.txt')
                return p
        
        logger.info('No valid %s found. ' % program + \
                                   'Please set in ./input/mg5_configuration.txt')
        return None
        
        
    def open_program(self, program, file_path, mac_check=True, background=False):
        """ open a file with a given program """
        
        if mac_check==True and sys.platform == 'darwin':
            return self.open_mac_program(program, file_path)
        
        # Shell program only                                                                                                                                                                 
        if program:
            arguments = program.split() # allow argument in program definition
            arguments.append(file_path)
        
            if not background:
                subprocess.call(arguments)
            else:
                import thread
                thread.start_new_thread(subprocess.call,(arguments,))
        else:
            logger.warning('Not able to open file %s since no program configured.' % file_path + \
                                'Please set one in ./input/mg5_configuration.txt')

    def open_mac_program(self, program, file_path):
        """ open a text with the text editor """
        
        if not program:
            # Ask to mac manager
            os.system('open %s' % file_path)
        elif which(program):
            # shell program
            arguments = program.split() # Allow argument in program definition
            arguments.append(file_path)
            subprocess.call(arguments)
        else:
            # not shell program
            os.system('open -a %s %s' % (program, file_path))

def get_HEPTools_location_setter(HEPToolsDir,type):
    """ Checks whether mg5dir/HEPTools/<type> (which is 'lib', 'bin' or 'include')
    is in the environment paths of the user. If not, it returns a preamble that
    sets it before calling the exectuable, for example:
       <preamble> ./my_exe
    with <preamble> -> DYLD_LIBRARY_PATH='blabla;$DYLD_LIBRARY_PATH'"""
    
    assert(type in ['bin','include','lib'])
    
    target_env_var = 'PATH' if type in ['bin','include'] else \
          ('DYLD_LIBRARY_PATH' if sys.platform=='darwin' else 'LD_LIBRARY_PATH')
    
    target_path = os.path.abspath(pjoin(HEPToolsDir,type))
    
    if target_env_var not in os.environ or \
                target_path not in os.environ[target_env_var].split(os.pathsep):
        return "%s='%s;$%s' "%(target_env_var,target_path,target_env_var)
    else:
        return ''

def get_shell_type():
    """ Try and guess what shell type does the user use."""
    try:
        if os.environ['SHELL'].endswith('bash'):
            return 'bash'
        elif os.environ['SHELL'].endswith('tcsh'):
            return 'tcsh'
        else:
            # If unknown, return None
            return None 
    except KeyError:
        return None

def is_executable(path):
    """ check if a path is executable"""
    try: 
        return os.access(path, os.X_OK)
    except Exception:
        return False        
    
class OptionParser(optparse.OptionParser):
    """Option Peaser which raise an error instead as calling exit"""
    
    def exit(self, status=0, msg=None):
        if msg:
            raise InvalidCmd, msg
        else:
            raise InvalidCmd

def sprint(*args, **opt):
    """Returns the current line number in our program."""
    
    if not __debug__:
        return
    
    import inspect
    if opt.has_key('log'):
        log = opt['log']
    else:
        log = logging.getLogger('madgraph')
    if opt.has_key('level'):
        level = opt['level']
    else:
        level = logging.getLogger('madgraph').level
        #print  "madgraph level",level
        #if level == 20:
        #    level = 10 #avoid info level
        #print "use", level
    lineno  =  inspect.currentframe().f_back.f_lineno
    fargs =  inspect.getframeinfo(inspect.currentframe().f_back)
    filename, lineno = fargs[:2]
    #file = inspect.currentframe().f_back.co_filename
    #print type(file)
    try:
        source = inspect.getsourcelines(inspect.currentframe().f_back)
        line = source[0][lineno-source[1]]
        line = re.findall(r"misc\.sprint\(\s*(.*)\)\s*($|#)", line)[0][0]
        if line.startswith("'") and line.endswith("'") and line.count(",") ==0:
            line= ''
        elif line.startswith("\"") and line.endswith("\"") and line.count(",") ==0:
            line= ''
        elif line.startswith(("\"","'")) and len(args)==1 and "%" in line:
            line= ''        
    except Exception:
        line=''

    if line:
        intro = ' %s = \033[0m' % line
    else:
        intro = ''
    

    log.log(level, ' '.join([intro]+[str(a) for a in args]) + \
                   ' \033[1;30m[%s at line %s]\033[0m' % (os.path.basename(filename), lineno))
    return 

################################################################################
# function to check if two float are approximatively equal
################################################################################
def equal(a,b,sig_fig=6, zero_limit=True):
    """function to check if two float are approximatively equal"""
    import math

    if not a or not b:
        if zero_limit:
            power = sig_fig + 1
        else:
            return a == b  
    else:
        power = sig_fig - int(math.log10(abs(a))) + 1

    return ( a==b or abs(int(a*10**power) - int(b*10**power)) < 10)

################################################################################
# class to change directory with the "with statement"
# Exemple:
# with chdir(path) as path:
#     pass
################################################################################
class chdir:
    def __init__(self, newPath):
        self.newPath = newPath

    def __enter__(self):
        self.savedPath = os.getcwd()
        os.chdir(self.newPath)

    def __exit__(self, etype, value, traceback):
        os.chdir(self.savedPath)

################################################################################
# Timeout FUNCTION
################################################################################

def timeout(func, args=(), kwargs={}, timeout_duration=1, default=None):
    '''This function will spwan a thread and run the given function using the args, kwargs and 
    return the given default value if the timeout_duration is exceeded 
    ''' 
    import threading
    class InterruptableThread(threading.Thread):
        def __init__(self):
            threading.Thread.__init__(self)
            self.result = default
        def run(self):
            try:
                self.result = func(*args, **kwargs)
            except Exception,error:
                print error
                self.result = default
    it = InterruptableThread()
    it.start()
    it.join(timeout_duration)
    return it.result


################################################################################
# TAIL FUNCTION
################################################################################
class digest:

    def test_all(self):
        try:
            return self.test_hashlib()
        except Exception:
            pass
        try:
            return self.test_md5()
        except Exception:
            pass
        try:
            return self.test_zlib()
        except Exception:
            pass
                
    def test_hashlib(self):
        import hashlib
        def digest(text):
            """using mg5 for the hash"""
            t = hashlib.md5()
            t.update(text)
            return t.hexdigest()
        return digest
    
    def test_md5(self):
        import md5
        def digest(text):
            """using mg5 for the hash"""
            t = md5.md5()
            t.update(text)
            return t.hexdigest()
        return digest
    
    def test_zlib(self):
        import zlib
        def digest(text):
            return zlib.adler32(text)
    
digest = digest().test_all()

#===============================================================================
# Helper class for timing and RAM flashing of subprocesses.
#===============================================================================
class ProcessTimer:
  def __init__(self,*args,**opts):
    self.cmd_args = args
    self.cmd_opts = opts
    self.execution_state = False

  def execute(self):
    self.max_vms_memory = 0
    self.max_rss_memory = 0

    self.t1 = None
    self.t0 = time.time()
    self.p = subprocess.Popen(*self.cmd_args,**self.cmd_opts)
    self.execution_state = True

  def poll(self):
    if not self.check_execution_state():
      return False

    self.t1 = time.time()
    # I redirect stderr to void, because from MacOX snow leopard onward, this
    # ps -p command writes a million times the following stupid warning
    # dyld: DYLD_ environment variables being ignored because main executable (/bin/ps) is setuid or setgid
    flash = subprocess.Popen("ps -p %i -o rss"%self.p.pid,
                  shell=True,stdout=subprocess.PIPE,stderr=open(os.devnull,"w"))
    stdout_list = flash.communicate()[0].split('\n')
    rss_memory = int(stdout_list[1])
    # for now we ignore vms
    vms_memory = 0

    # This is the neat version using psutil
#    try:
#      pp = psutil.Process(self.p.pid)
#
#      # obtain a list of the subprocess and all its descendants
#      descendants = list(pp.get_children(recursive=True))
#      descendants = descendants + [pp]
#
#      rss_memory = 0
#      vms_memory = 0
#
#      # calculate and sum up the memory of the subprocess and all its descendants 
#      for descendant in descendants:
#        try:
#          mem_info = descendant.get_memory_info()
#
#          rss_memory += mem_info[0]
#          vms_memory += mem_info[1]
#        except psutil.error.NoSuchProcess:
#          # sometimes a subprocess descendant will have terminated between the time
#          # we obtain a list of descendants, and the time we actually poll this
#          # descendant's memory usage.
#          pass
#
#    except psutil.error.NoSuchProcess:
#      return self.check_execution_state()

    self.max_vms_memory = max(self.max_vms_memory,vms_memory)
    self.max_rss_memory = max(self.max_rss_memory,rss_memory)

    return self.check_execution_state()

  def is_running(self):
    # Version with psutil
#    return psutil.pid_exists(self.p.pid) and self.p.poll() == None
    return self.p.poll() == None

  def check_execution_state(self):
    if not self.execution_state:
      return False
    if self.is_running():
      return True
    self.executation_state = False
    self.t1 = time.time()
    return False

  def close(self,kill=False):

    if self.p.poll() == None:
        if kill:
            self.p.kill()
        else:
            self.p.terminate()

    # Again a neater handling with psutil
#    try:
#      pp = psutil.Process(self.p.pid)
#      if kill:
#        pp.kill()
#      else:
#        pp.terminate()
#    except psutil.error.NoSuchProcess:
#      pass


try:
    import Foundation
    import objc
    NSUserNotification = objc.lookUpClass('NSUserNotification')
    NSUserNotificationCenter = objc.lookUpClass('NSUserNotificationCenter')

    def apple_notify(subtitle, info_text, userInfo={}):
        try:
            notification = NSUserNotification.alloc().init()
            notification.setTitle_('MadGraph5_aMC@NLO')
            notification.setSubtitle_(subtitle)
            notification.setInformativeText_(info_text)
            notification.setUserInfo_(userInfo)
            NSUserNotificationCenter.defaultUserNotificationCenter().scheduleNotification_(notification)
        except:
            pass
except:
    def apple_notify(subtitle, info_text, userInfo={}):
        return

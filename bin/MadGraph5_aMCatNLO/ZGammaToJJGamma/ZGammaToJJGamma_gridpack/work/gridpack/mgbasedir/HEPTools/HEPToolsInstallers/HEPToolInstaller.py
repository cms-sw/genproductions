#! /usr/bin/env python

__author__ = 'Valentin Hirschi'
__email__  = "valentin.hirschi[at]gmail[dot]com"

import sys
import os
import subprocess
import shutil
import tarfile
import tempfile
import glob
import logging
import re
try:
    from distutils.version import LooseVersion
except ModuleNotFoundError:
    try:
        from packaging.version import parse, Version
        LooseVersion = parse
    except ModuleNotFoundError:
        from looseversion import LooseVersion

logger = logging
pjoin = os.path.join

_lib_extensions = ['a']
if sys.platform == "darwin":
   _lib_extensions.append('dylib')
else:
   _lib_extensions.append('so')


import sys
if sys.version_info[0] < 3:
   PY3 = False
else:
   PY3 = True
   
_standard_lib_dir_names = ['lib','lib64']

_HepTools = {'hepmc':
               {'install_mode':'Default',
                # Force to install this dependency if not found locally
                'allow_system_wide': False,
                'version':       '2.06.09', #'2.07-beta00',
                'www':'http://madgraph.phys.ucl.ac.be/Downloads',
                'tarball':      ['online','%(www)s/hepmc%(version)s.tgz'],
                'mandatory_dependencies': [],
                'optional_dependencies' : [],
                'libraries' : ['libHepMC.%(libextension)s'],
                'install_path':  '%(prefix)s/hepmc/'},
             'hepmc3':
               {'install_mode':'Default',
                # Force to install this dependency if not found locally
                'allow_system_wide': False,
                'version':       '3.2.6',
                'www': 'http://hepmc.web.cern.ch/hepmc/releases',
                'tarball':      ['online','%(www)s/HepMC3-%(version)s.tar.gz'],
                'mandatory_dependencies': [],
                'optional_dependencies' : [],
                'libraries' : ['libHepMC3.%(libextension)s'],
                'install_path':  '%(prefix)s/hepmc3/'},
             'boost':
               {'install_mode':'Default',
                'version':       '1.74.0',
                'www': 'http://sourceforge.net/projects/boost/files/boost/1.74.0', 
#                'tarball':      ['online','%(www)s/boost_1_74_0.tar.gz'],
                'tarball': ['online', 'madgraph.phys.ucl.ac.be/Downloads/boost_1_74_0.tar.gz'],
                'mandatory_dependencies': [],
                'optional_dependencies' : [],
                'libraries' : ['libboost_system-mt.%(libextension)s','libboost_system.%(libextension)s'],
                'include_path' : ['/opt/local/include', '/usr/local/include', '/opt/include', '/usr/include'], 
                'install_path':  '%(prefix)s/boost/'},
             'yoda':{'install_mode':'Default',
                'version':       '1.9.10',
                'www': 'https://yoda.hepforge.org/',
                'tarball':      ['online', '%(www)s/downloads/YODA-%(version)s.tar.gz'],
               # Specify a different tarball for mac 
                 'MG5_version_constraints' : [
                     ( lambda MG5version: sys.platform == "darwin",
                        ['online','%(www)s/downloads/YODA-1.9.0.tar.gz'] ),
                         #https://bazaar.launchpad.net/~ma5dev/madanalysis5/v1.9_beta/tarball'] ),
                 ],
                     'mandatory_dependencies': [],
                'optional_dependencies' : [],
                'libraries' : ['libYODA.%(libextension)s'],
                'include_path' : [],
                'install_path':  '%(prefix)s/yoda/'},
             'fastjet':{'install_mode':'Default',
                'version':       '3.4.2',
                'www': 'https://fastjet.fr',
                'tarball':      ['online', '%(www)s/repo/fastjet-%(version)s.tar.gz'],
                'mandatory_dependencies': [],
                'optional_dependencies' : [],
                'libraries' : ['libfastjet.%(libextension)s'],
                'include_path' : [],
                'install_path':  '%(prefix)s/fastjet/'},
             'fjcontrib':{'install_mode':'Default',
                'version':       '1.045',
                'www': 'http://fastjet.hepforge.org/',
                'tarball':      ['online', '%(www)s/contrib/downloads/fjcontrib-%(version)s.tar.gz'],
                'mandatory_dependencies': ['fastjet'],
                'optional_dependencies' : [],
                'libraries' : ['libRecursiveTools.%(libextension)s'],
                'include_path' : [],
                'install_path':  '%(prefix)s/fastjet/'},
             'rivet':{'install_mode':'Default',
                'version':       '3.1.10',
                'www': 'https://rivet.hepforge.org/',
                'tarball':      ['online', '%(www)s/downloads/Rivet-%(version)s.tar.gz'],
                'mandatory_dependencies': ['hepmc', 'yoda', 'fastjet','fjcontrib'],
                'optional_dependencies' : [],
                'libraries' : ['libRivet.%(libextension)s'],
                'include_path' : [],
                'install_path':  '%(prefix)s/rivet/'},
             'contur':{'install_mode':'Default',
                'version':       '2.0.2',
                 'www': 'https://gitlab.com/hepcedar/contur/-/archive',
                'tarball':      ['online', '%(www)s/contur-%(version)s/contur-contur-%(version)s.tar.gz'],
                'mandatory_dependencies': ['rivet', 'yoda'],
                'optional_dependencies' : [],
                'libraries' : [],
                'include_path' : [],
                'install_path':  '%(prefix)s/contur/'},                          
             'gsl':{'install_mode':'Default',
                'version':       '2.4',
                'www': 'ftp://ftp.gnu.org/gnu/gsl/',
                'tarball':      ['online', '%(www)s/gsl-%(version)s.tar.gz'],
                'mandatory_dependencies': [],
                'optional_dependencies' : [],
                'libraries' : ['libgsl.%(libextension)s'],
                'include_path' : [],
                'install_path':  '%(prefix)s/GSL/'},
             'fitsio':{'install_mode':'Default',
                'version':       '3450',
                'www': 'http://heasarc.gsfc.nasa.gov/FTP/software/fitsio/c',
                'format_version': lambda x: x.replace('.',''),
                'tarball':      ['online', '%(www)s/cfitsio%(version)s.tar.gz'],
                'mandatory_dependencies': [],
                'optional_dependencies' : [],
                'libraries' : ['libcfitsio.%(libextension)s'],
                'include_path' : [],
                'install_path':  '%(prefix)s/fitsio/'},
             'dragon_data':{'install_mode':'Default',
                'version':       '1.0',
                'www': 'http://madgraph.phys.ucl.ac.be/Downloads/maddm',
                'tarball':      ['online', '%(www)s/dragon_input_from_galprop.zip'],
                'mandatory_dependencies': ['dragon', 'gsl', 'fitsio'],
                'optional_dependencies' : [],
                'libraries' : [],
                'include_path' : [],
                'install_path':  '%(prefix)s/dragon/'},
             'dragon':{'install_mode':'Default',
                'version':       '1.0',
                'www': 'https://github.com/cosmicrays/DRAGON/archive',
                'tarball':      ['online', '%(www)s/master.zip'],
                'mandatory_dependencies': ['gsl', 'fitsio'],
                'optional_dependencies' : [],
                'libraries' : ['libDRAGON.a'],
                'include_path' : [],
                'install_path':  '%(prefix)s/dragon/'},
             'rosetta':
               {'install_mode':'Default',
                'version':       '1.0',
                'www': 'http://www.hepforge.org/archive/rosetta/',
                'tarball':      ['online', '%(www)s/Rosetta-2.1.tgz'],
                'mandatory_dependencies': [],
                'optional_dependencies' : [],
                'libraries' : [],
                'include_path' : [],
                'install_path':  '%(prefix)s/rosetta/'},
             'pythia8':
               {'install_mode':'Default',
                'version':       '8311',
                'www': 'https://pythia.org/download/pythia83',
# Official version
                'tarball':      ['online','%(www)s/pythia%(version)s.tgz'],
# Development version
#                'tarball':      ['online','http://slac.stanford.edu/~prestel/pythia8217alpha.tar.gz'],                
                # We put zlib mandatory because we are not going to unzip .lhe files in MG5_aMC when passing them to PY8.
                'mandatory_dependencies': ['hepmc','zlib'],
                # Dependency lhapdf, without version specification means this installer will try linking against the most
                # recent version
                'optional_dependencies' : ['lhapdf6'],
                'libraries' : ['libpythia8.%(libextension)s'],
                'install_path':  '%(prefix)s/pythia8/'},
             'lhapdf6':
               {'install_mode':'Default',
                'version':       '6.5.4',
                'www': 'https://lhapdf.hepforge.org/downloads',#?f=LHAPDF-6.1.6.tar.gz',
                #'tarball': ['online', 'http://madgraph.phys.ucl.ac.be/Downloads/LHAPDF-%(version)s.tar.gz'],
                'tarball': ['online','%(www)s/LHAPDF-%(version)s.tar.gz'],
                'mandatory_dependencies': [],
                'optional_dependencies' : [],
                'libraries' : ['libLHAPDF.%(libextension)s'],
                'install_path':  '%(prefix)s/lhapdf6/' if not PY3 else '%(prefix)s/lhapdf6_py3/'},             
             'lhapdf61':
               {'install_mode':'Default',
                'version':       '6.1.6',
                'www': 'https://lhapdf.hepforge.org/downloads',#?f=LHAPDF-6.1.6.tar.gz',
                'tarball':      ['online','%(www)s/LHAPDF-%(version)s.tar.gz'],
                'mandatory_dependencies': ['boost'],
                'optional_dependencies' : [],
                'libraries' : ['libLHAPDF.%(libextension)s'],
                'install_path':  '%(prefix)s/lhapdf6/' if not PY3 else '%(prefix)s/lhapdf6_py3/'},
             'lhapdf5':
               {'install_mode':'Default',
                'version':       '5.9.1',
                'www': 'https://lhapdf.hepforge.org/downloads/old/',#?f=LHAPDF-6.1.6.tar.gz',
                'tarball':      ['online','%(www)s/lhapdf-%(version)s.tar.gz'],
                'mandatory_dependencies': [],
                'optional_dependencies' : [],
                'libraries' : ['libLHAPDF.%(libextension)s'],
                'install_path':  '%(prefix)s/lhapdf5/'},
             'zlib':
               {'install_mode':'Default',
                # Force to install this dependency if not found locally
                'allow_system_wide': False,
                'version':       '1.2.10',
                # Zlib doesn't offer a sticky link and often updates its version. So we must host 1.2.10 ourselves to be stable
                # 'tarball':      ['online','http://zlib.net/zlib-%(version)s.tar.gz'],
                # Not ideal since the MG server can be down quite often.
                #'tarball':      ['online','http://madgraph.phys.ucl.ac.be/Downloads/zlib-1.2.10.tar.gz'],
                # This legacy versions webpage should be more stable
                'www' : ['http://www.zlib.net/fossils','http://madgraph.phys.ucl.ac.be/Downloads/', 'http://madgraph.physics.illinois.edu/Downloads'],
                'tarball':      ['online','%(www)s/zlib-1.2.10.tar.gz'],
                'mandatory_dependencies': [],
                'optional_dependencies' : [],
                'libraries' : ['libz.%(libextension)s','libz.1.%(libextension)s',
                               'libz.1.2.8.%(libextension)s'],
                'mandatory_files' : ['%s/include/zlib.h'%os.path.pardir,
                                     '%s/include/zconf.h'%os.path.pardir],
                'install_path':  '%(prefix)s/zlib/'},
              'mg5amc_py8_interface':
               {'install_mode':'Default',
                'version':       '1.0',
                'www': '',
#                'tarball':      ['online','http://madgraph.phys.ucl.ac.be/Downloads/MG5aMC_PY8_interface.tar.gz'],
                'tarball':      ['online','TO_BE_DEFINED_BY_INSTALLER'],
                'mandatory_dependencies': ['pythia8'],
                'optional_dependencies' : [],
                'libraries' : ['MG5aMC_PY8_interface'],
                'install_path':  '%(prefix)s/MG5aMC_PY8_interface/'},
               'ninja':
               {'install_mode':'Default',
                'version':       '1.1 (not semantic)',
                'www' : 'https://ninja.hepforge.org/downloads/',
                'mandatory_dependencies': ['oneloop'],
                'tarball':      ['online','http://madgraph.phys.ucl.ac.be/Downloads/ninja-1.1.0.tar.gz'],
                #'tarball':      ['online','%(www)s/ninja-1.1.0.tar.gz'],
                'optional_dependencies' : [],
                'libraries' : ['libninja.%(libextension)s'],
                'install_path':  '%(prefix)s/ninja/'},
               'oneloop':
               {'install_mode':'Default',
                # Force to install this dependency if not found locally
                'allow_system_wide': False,
                'version':       '3.6',
                'www': 'http://helac-phegas.web.cern.ch/helac-phegas/tar-files',
                'tarball':      ['online','%(www)s/OneLOop-%(version)s.tgz'],
                'mandatory_dependencies': [],
                'optional_dependencies' : [],
                'libraries' : ['libavh_olo.a'],
                'install_path':  '%(prefix)s/oneloop/'},
               'collier':
               {'install_mode':'Default',
                'version':       '1.2.1',
                'www' : 'http://collier.hepforge.org/',
                'tarball':      ['online','%(www)s/collier-latest.tar.gz'],
                'mandatory_dependencies': ['cmake'],
                'optional_dependencies' : [],
                'libraries' : ['libcollier.a'],
                'install_path':  '%(prefix)s/collier/'},
               'madanalysis5':
               {'install_mode':'Default',
                'version':       '1.10.9_beta',
#               WARNING USING BETA VERSION OF 1.10.9                
                'www': 'http://madanalysis.irmp.ucl.ac.be/raw-attachment/wiki/MA5SandBox',
                'tarball':      ['online','https://github.com/MadAnalysis/madanalysis5/archive/refs/tags/v%(version)s.tar.gz'],
                # Specify a different tarball for MG version before 2.6.1
                'MG5_version_constraints' : [
                    ( lambda MG5version: MG5version < LooseVersion("2.6.1"),
                       ['online','%(www)s/ma5_v_1_6_21.tgz'] ),
                    ( lambda MG5version: PY3,
                       ['online','https://github.com/MadAnalysis/madanalysis5/archive/refs/tags/v%(version)s.tar.gz']),
                        #https://bazaar.launchpad.net/~ma5dev/madanalysis5/v1.9_beta/tarball'] ),
                ],
                'mandatory_dependencies': ['zlib'],
                'optional_dependencies' : [],
                'libraries' : [],
                'install_path':  '%(prefix)s/madanalysis5/',
                'MA5_further_install':True,
                'use_root_if_available':True,
               },
               'cmake':
               {'install_mode':'Default',
                'version':       '3.6.0',
                'www': 'http://cmake.org/files/v3.6',
                'tarball':      ['online','%(www)s/cmake-3.6.0-rc2.tar.gz'],
                'mandatory_dependencies': [],
                'optional_dependencies' : [],
                'libraries' : ['cmake'],
                'install_path':  '%(prefix)s/cmake/',
               },
               'ginac_mg5_interface':
               {'install_mode':'Default',
                'version':       '0.1',
                'tarball':      ['local','%(mg5_path)s/vendor/ginac_mg5_interface'],
                'mandatory_dependencies': ['ginac','cln'],
                'optional_dependencies' : [],
                'libraries' : [''],
                'install_path':  '%(prefix)s/ginac_mg5_interface/',
               },
             'ginac':
                 {'install_mode': 'Default',
                  'version': '1.7.5',
                  'www' : 'https://ginac.de',
                  'tarball': ['local', ''],
                  'mandatory_dependencies': ['cln'],
                  'optional_dependencies': [],
                  'libraries': [''],
                  'install_path': '%(prefix)s/ginac/',
                  },
             'cln':
                 {'install_mode': 'Default',
                  'version': '1.3.4',
                  'www': 'https://www.ginac.de/CLN',
                  'tarball': ['local', ''],
                  'mandatory_dependencies': [],
                  'optional_dependencies': [],
                  'libraries': [''],
                  'install_path': '%(prefix)s/cln/',
                  },
             'nklo_tools':
                 {
                'install_mode':'Default',
                'version':       '0.1',
                'tarball':      ['local','%(mg5_path)s/vendor/NkLO_tools'],
                'mandatory_dependencies': ['ginac','cln'],
                'optional_dependencies' : [],
                'libraries' : [''],
                'install_path':  '%(prefix)s/NkLO_tools/',
                 },
             'emela':
                {
                'install_mode':'Default',
                'version':       '1.0',
                'www': 'https://github.com/gstagnit/eMELA/',
                'tarball':      ['online','%(www)s/archive/refs/tags/v%(version)s.tar.gz'],
                   'mandatory_dependencies': ['lhapdf6','cmake', 'boost'],
                'optional_dependencies' : [],
                'libraries' : ['libeMELA.so'],
                'install_path':  '%(prefix)s/EMELA/',
                },
            'cudacpp':
               {'install_mode': 'Default',
                'version': 'TEST_cudacpp_for%(_mg5_version)s_latest',
                'www': 'http://madgraph.phys.ucl.ac.be/Downloads/cudacpp/info.dat',
                'tarball': ['online','MG5_specific'],
                ###'www': 'https://github.com/valassi/madgraph4gpu/releases',
                ###'tarball': ['online','%(www)s/download/%(version)s/cudacpp.tar.gz'],
                'mandatory_dependencies': [],
                'optional_dependencies' : [],
                'libraries' : [''],
                'install_path':  '%(mg5_path)s/PLUGIN/'},
            }

# Set default for advanced options
for tool in _HepTools:
    if 'allow_system_wide' not in _HepTools[tool]:
        _HepTools[tool]['allow_system_wide'] = True

_non_installable_dependencies = {
               'delphes':        { 'active' : None, 'path'   : None}, #None -> not specified by the user.
               'delphesMA5tune': { 'active' : None, 'path'   : None},
               'root':           { 'active' : None , 'path'   : None},
#               'fastjet':        { 'active' : None , 'path'   : None},
               'pad':            { 'active' : None, 'path'   : None},
               'padforMA5tune':  { 'active' : None, 'path'   : None},
             }

_cwd             = os.getcwd()
_installers_path = os.path.abspath(os.path.dirname(os.path.realpath( __file__ )))
_cpp             = 'g++'
_gfortran        = 'gfortran'
_prefix          = pjoin(_cwd,'HEPTools')
_overwrite_existing_installation = False
# MG5 path, can be used for the installation of mg5amc_py8_interface
_mg5_path        = None
_mg5_version     = None
_cpp_standard_lib= '-lstdc++'
_keep_source     = False
_keep_existing_installation = False

# List of tools for which the tarballs have been specified
_tarballs_specified = []

_force_local_server = False

logger_level = [a[10:] for a in sys.argv if '--logging=' in a]
if not logger_level:
   logger_level = 20
else:
   logger_level = logger_level[-1]
   if logger_level.isdigit():
      logger_level = int(logger_level)
   else:
      try:
         logger_level = getattr(logging,logger_level)
      except AttributeError:
         print("Logging level %s not reckognized."%logger_level)
         sys.exit(9)
logging.basicConfig(format='%(message)s',level=logger_level)         

if "__main__" == __name__:

   if 'help' in sys.argv or '--help' in sys.argv:
      sys.argv[1] = 'help'
   elif len(sys.argv)>1 and sys.argv[1].lower() not in _HepTools.keys():
      logger.warning("HEPToolInstaller does not support the installation of %s" , sys.argv[1])
      sys.argv[1] = 'help'

   if len(sys.argv)<2 or sys.argv[1]=='help': 
      print( """
./HEPToolInstaller <target> <options>
     Possible values and meaning for the various <options> are:
           
     |           option           |    value           |                          meaning
     -============================-====================-===================================================
     | <target>                   | Any <TOOL>         | Which HEPTool to install
     | --prefix=<path>            | install root path  | Specify where to install target and dependencies
     | --force                    | -                  | Overwrite existing installation if necessary
     | --update                   | -                  | Update the installation with different options.
     | --fortran_compiler=<path>  | path to gfortran   | Specify fortran compiler
     | --cpp_compiler=<path>      | path to g++        | Specify C++ compiler
     | --cpp_standard_lib=<lib>   | -lc++ or -lstdc++  | Specify which C++ standard library the compiler links to
     | --mg5_path=<path>          | path to MG5        | Specify what is the MG5 distribution invoking this
     | --with_<DEP>[=<DepMode>]   | <path>             | Use the specified path for dependency DEP
     |                            | Default, ON, -     | Link against DEP if present otherwise install it
     |                            | OFF                | Do not link against dependency DEP
     | --veto_<DEP>               | -                  | Identical to --with_<DEP>=OFF
     | --<TOOL>_tarball=<path>    | A path to .tar.gz  | Path of the tarball to be used if DEP is installed
     | --keep_source              | -                  | If specified, the installer will keep the source file
     | --logging=<int>            | integer | string   | Logging level for the installer. 10 is DEBUG
     | --version=<str>            | string             | Try to force a specific version of the tool (best try)
     -============================-====================-===================================================
     <TOOL> can be any of the following :\n        %s\n
     <DEP>  can be <TOOL> but also      :\n        %s\n
Example of usage:
    ./HEPToolInstaller.py pythia8 --prefix=~/MyTools --with_lhapdf6=OFF --pythia8_tarball=~/MyTarball.tar.gz
""" % (', '.join(_HepTools.keys()),', '.join(_non_installable_dependencies.keys())))
      sys.exit(9)

   target_tool = sys.argv[1].lower()


# Make sure to set the install location of all other tools than the target to 'default'. Meaning that they
# will be installed if not found.
   for tool in _HepTools:
      if tool==target_tool:
         continue
      _HepTools[tool]['install_path']='default'


# For compatibility reason, map some names
_dependency_names_map = {'delphes3':'delphes'}

available_options = ['--prefix','--fortran_compiler','--cpp_compiler','--gfortran_compiler','--update',
                     '--force','--keep_source','--mg5_path','--cpp_standard_lib','--no_MA5_further_install',
                     '--no_root_in_MA5']+\
                    ['--%s_tarball'%tool for tool in _HepTools.keys()]+\
                    ['--with_%s'%tool for tool in list(_HepTools.keys())+list(_non_installable_dependencies.keys())+
                                                                           list(_dependency_names_map.keys())]+\
                    ['--veto_%s'%tool for tool in list(_HepTools.keys())+list(_non_installable_dependencies.keys())]+\
                    ['--logging','--source', '--version']

if '__main__' == __name__:
   # Recall input command for logfiles
   logger.debug("Installer HEPToolInstaller.py is now processing the following command:")
   logger.debug("   %s"%' '.join(sys.argv))

def with_option_parser(with_option):
    if with_option=='ON':
        return 'Default'
    if with_option=='OFF':
        return False 
    else:
        return with_option


def adapt_tarball_paths_according_to_MG5_version(MG5_version):
    """ Adapt paths of certain dependencies depending on MG5aMC version."""

    for tool_name, tool_options in _HepTools.items():
        # Do not modify tarball path if specified by user
        if tool_name in _tarballs_specified:
            continue
        if 'MG5_version_constraints' in tool_options:
            for constraint, tarball_specifier in tool_options['MG5_version_constraints']:
                if constraint(MG5_version):
                    # Overwrite tarball path with the first constraint that fits
                    tool_options['tarball'] = tarball_specifier
                    break
        # install lhapdf62 for newer version of MG5aMC
        if tool_name in ['lhapdf6', 'lhapdf'] and MG5_version and MG5_version < LooseVersion("2.6.1"):
            for key in tool_options:
                _HepTools['lhapdf6'][key] = _HepTools['lhapdf61'][key]
        if  'MG5_specific' == _HepTools[tool_name]['tarball'][1]:
            import urllib.request as request
            import six
            try:
                data = six.moves.urllib.request.urlopen(_HepTools[tool_name]['www'])
            except Exception as error:
                _HepTools[tool_name]['tarball'] = ['online', str(error)]
                continue
            struct = {}
            for line in data:
                line = line.decode()
                version, html = line.split(maxsplit=1)
                major, medium, minor = [int(i) for i in version.split('.')[:3]]
                struct[(major, medium, minor)] = html

            # Find compatible tarball
            try:
                html = find_compatible_tarball(struct, MG5_version)
            except Exception as error:
                html = str(error)
            _HepTools[tool_name]['tarball'] = ['online', html]


def find_compatible_tarball(database, MG5_version):
    """ struct is a database of the form (major, medium, minor) -> something (typically html link)
        this function 
        The rule is to find the version A.B.C with a lower (or equal) numbering 
        present in the datastructure
        Only version with A.B.C version are handle (pure digit)
        A.B.C.D are working but all .D are just ignored below and therefore treated as A.B.C
    """

    MG5_major, MG5_medium, MG5_minor = [int(i) for i in str(MG5_version).split('.')[:3]] 
    for major in range(MG5_major, -1, -1):
        # find the rang of allowed value for the second index
        if major == MG5_major:
            max_medium = MG5_medium
        else:
            max_medium = max([v2 for (v1, v2, v3) in database if v1 == major], default=-1)

        for medium in range(max_medium, -1, -1):
            # find the rang of allowed value for the third index
            if major == MG5_major and medium == MG5_medium:
                max_minor = MG5_minor
            else:
                max_minor = max([v3 for (v1, v2, v3) in database if v1 == major and v2 == medium], default=-1)

            for minor in range(max_minor, -1, -1):
                if (major, medium, minor) in database:
                    return database[(major, medium, minor)].strip() 
                
    raise Exception("No compatible data detected")          

if '__main__' == __name__:
    _version = None
    # Now parse the options
    for user_option in sys.argv[2:]:
        try:
            option, value = user_option.split('=')
        except:
            option = user_option
            value  = None
        if option not in available_options:
            logger.error("HEPToolsInstaller.py: option '%s' not recognized." , option)
            sys.exit(9)
        if option=='--force':
            _overwrite_existing_installation = True
        if option=='--update':
            supported_tools = ['madanalysis5']
            if target_tool not in supported_tools:
                logger.error("HEPToolsInstaller.py supports the option '--update' only for the following tools:")
                logger.error("  %s"%supported_tools)
                sys.exit(9)
            _keep_existing_installation = True
        if option=='--keep_source':
            _keep_source = True
        if option=='--prefix':
            if not os.path.isdir(value):
                logger.info("Creating root directory '%s'.", os.path.abspath(value))
                os.mkdir(os.path.abspath(value))
            _prefix = os.path.abspath(value)
        elif option=='--fortran_compiler':
            _gfortran = value
        elif option=='--cpp_compiler':
            _cpp = value
        elif option=='--no_MA5_further_install':
            _HepTools['madanalysis5']['MA5_further_install']=False
        elif option=='--no_root_in_MA5':
            _HepTools['madanalysis5']['use_root_if_available']=False        
        elif option=='--cpp_standard_lib':
            if value not in ['-lc++','-lstdc++']:
                logger.error( "ERROR: Option '--cpp_standard_lib' must be either '-lc++' or '-libstdc++', not '%s'.", value)
                sys.exit(9)
            _cpp_standard_lib = value
        elif option=='--mg5_path':
            _mg5_path = value
            # Try to gather MG5_version
            try:
                _mg5_version = None
                for line in open(pjoin(_mg5_path,'VERSION'),'r').read().split('\n'):
                    if line.startswith('version ='):
                        out = re.findall(r'version\s*=\s*([\.\d]*)', line)
                        if out[0].endswith('.'):
                            out[0] = out[0][:-1]
                        _mg5_version = LooseVersion(out[0])
                        break
            except:
                raise
                _mg5_version = None
        elif option.startswith('--with_'):
            dependency_name = _dependency_names_map[option[7:]] if option[7:] in _dependency_names_map else option[7:]
            
            value           = with_option_parser(value)
            if value and not os.path.isdir(value):
                value = os.path.dirname(value)
            if value and os.path.basename(value) == "bin":
                value = os.path.dirname(value)
                
            if dependency_name in _HepTools:
                _HepTools[dependency_name]['install_path'] = value
            else:
                # Special treatment for dependencies that cannot be directly installed in this installer
                _non_installable_dependencies[dependency_name]['path']   = value if (value not in ['Default',False, None]) else None 
                _non_installable_dependencies[dependency_name]['active'] = (value!=False)
        elif option.startswith('--veto_'):
            dependency_name = option[7:]
            if dependency_name in _HepTools:
                _HepTools[dependency_name]['install_path'] = None 
            else:
                # Special treatment for dependencies that cannot be directly installed in this installer
                _non_installable_dependencies[dependency_name]['path']   = None 
                _non_installable_dependencies[dependency_name]['active'] = False

        elif option.endswith('_tarball'):
            access_mode = 'online' if '//' in value else 'local'
            if access_mode=='local':
                value = os.path.abspath(value)
            _HepTools[option[2:-8]]['tarball'] = [access_mode, value]
            # Flag the tarball of this tool as specified
            _tarballs_specified.append(option[2:-8])       
        elif option.startswith('--version='):
           _version = value


    # Adapt paths according to MG5 version specified
    if _mg5_version:
        adapt_tarball_paths_according_to_MG5_version(_mg5_version)

    # force code version if request by the user
    if _version:
       if 'format_version' in _HepTools[target_tool]:
          _version = _HepTools[target_tool]['format_version'](_version)
       if '%(version)s' in _HepTools[target_tool]['tarball'][1]:
          _HepTools[target_tool]['tarball'][1]=_HepTools[target_tool]['tarball'][1]%{'version':_version}
       else:
          raise Exception( 'fail to specify version for this tools.')

    # Apply substitutions if necessary:

    for tool in _HepTools:
        if _HepTools[tool]['install_path']:
           _HepTools[tool]['install_path']=_HepTools[tool]['install_path']%\
                                           {'prefix':_prefix, 'mg5_path': '' if _mg5_path is None else _mg5_path }
        if _HepTools[tool]['tarball'][0]=='local':
           _HepTools[tool]['tarball'][1]=_HepTools[tool]['tarball'][1]%\
                                           {'prefix':_prefix, 'mg5_path': '' if _mg5_path is None else _mg5_path }

        if _HepTools[tool]['tarball'][0]=='online':
           version = _HepTools[tool]['version']
           if _mg5_version and '%(_mg5_version)s' in version: version = version % {'_mg5_version' : str(_mg5_version)}
           if 'format_version' in _HepTools[tool]:
              version = _HepTools[tool]['format_version'](version)
           if not _force_local_server:
              www = _HepTools[tool]['www']
           else: 
              www = _force_local_server
           if not isinstance(www, list):
              _HepTools[tool]['tarball'][1]=_HepTools[tool]['tarball'][1]%{'version':version, 'www':www}
           else:
              _HepTools[tool]['tarball'][1]=[_HepTools[tool]['tarball'][1]%{'version':version, 'www':w} for w in www]            
        new_libs = []
        for lib in _HepTools[tool]['libraries']:
            for libext in _lib_extensions:
                if lib%{'libextension':libext} not in new_libs:
                    new_libs.append(lib%{'libextension':libext})
        _HepTools[tool]['libraries'] = new_libs


    # Make sure it is not already installed, but if the directory is empty, then remove it
    if os.path.isdir(pjoin(_prefix,target_tool)):
        if os.listdir(pjoin(_prefix,target_tool)) in [[],['%s_install.log'%target_tool]]:
            shutil.rmtree(pjoin(_prefix,target_tool))
            _keep_existing_installation = False
        else:
            if not _keep_existing_installation:
               if not _overwrite_existing_installation:
                  logger.warning( "The specified path '%s' already contains an installation of tool '%s'.", _prefix, target_tool)
                  logger.warning( "Rerun the HEPToolInstaller.py script again with the option '--force' if you want to overwrite it.")
                  sys.exit(66)
               else:
                  logger.info("Removing existing installation of tool '%s' in '%s'.", target_tool, _prefix)
                  shutil.rmtree(pjoin(_prefix,target_tool))
    else:
       _keep_existing_installation = False

# TMP_directory (designed to work as with statement) and go to it
class TMP_directory(object):
    """create a temporary directory, goes to it, and ensure this one to be cleaned.
    """

    def __init__(self, suffix='', prefix='tmp', dir=None, keep_source=False, tool=None):
        self.nb_try_remove = 0
        self.tool = tool 
        if not keep_source or self.tool is None:
            self.keep_source = False
            import tempfile 
            self.path = tempfile.mkdtemp(suffix, prefix, dir)
        else:
            self.keep_source = True
            basename= os.path.basename(os.path.abspath(_HepTools[self.tool]['install_path']))
            self.path = os.path.abspath(pjoin(_HepTools[self.tool]['install_path'],os.pardir,basename+'_src'))
            if os.path.isdir(self.path):
                logger.info("Found existing directory '%s'. This installer will overwrite it.", self.path)
                shutil.rmtree(self.path)     
            os.mkdir(self.path)                

        self.orig_path = os.getcwd()
        os.chdir(os.path.abspath(self.path))
    
    def __exit__(self, ctype, value, traceback ):
        os.chdir(self.orig_path)
        if self.keep_source:
            logger.info("The source directory of tool '%s' is kept at:\n  %s", self.tool, self.path)
            return
        #True only for debugging:
        elif False and isinstance(value, Exception):
            logger.info("The source directory '%s' was not cleaned. This directory can be removed manually.", self.path)
            return
        try:
            shutil.rmtree(self.path)
        except OSError:
            import time
            self.nb_try_remove += 1
            if self.nb_try_remove < 3:
                time.sleep(10)
                self.__exit__(ctype, value, traceback)
            else:
                logger.warning("Directory %s not completely cleaned. This directory can be removed manually" % self.path)
        
    def __enter__(self):
        return self.path

def test_cpp_compiler(options):
    """ Try to compile a dummy c++ program to test whether the compiler support the options specified
    in argument."""

    support_it = False
    try:
        tmp_dir = tempfile.mkdtemp()
        open(pjoin(tmp_dir,'test_cpp_compiler.cc'),'w').write(
"""#include <iostream>

int main()
{
  std::cout << "Hello World!";
}
""")
        cpp_tester = [_cpp,]+options+['test_cpp_compiler.cc','-o','test_cpp_compiler']
        p = subprocess.Popen(cpp_tester, stdout=subprocess.PIPE, stderr=subprocess.PIPE, cwd=tmp_dir)
        output, error = p.communicate()
        support_it = (p.returncode == 0)
        # clean-up of the temporary file
        shutil.rmtree(tmp_dir)
    except Exception:
        try:
            shutil.rmtree(tmp_dir)
        except:
            pass
        pass
    return support_it

#==================================================================================================
# Now define the installation function
#==================================================================================================
def install_zlib(tmp_path):
    """Installation operations for zlib"""
    zlib_log = open(pjoin(_HepTools['zlib']['install_path'],"zlib_install.log"), "w")
    p = subprocess.call([pjoin(_installers_path,'installZLIB.sh'),
                     _HepTools['zlib']['install_path'],
                     _HepTools['zlib']['version'],
                     _HepTools['zlib']['tarball'][1]], 
                    stdout=zlib_log,
                    stderr=zlib_log)
    zlib_log.close()
    return p 

def install_hepmc(tmp_path):
    """Installation operations for hepmc""" 
    hepmc_log = open(pjoin(_HepTools['hepmc']['install_path'],"hepmc_install.log"), "w")
    p= subprocess.call([pjoin(_installers_path,'installHEPMC2.sh'),
                     _HepTools['hepmc']['install_path'],
                     _HepTools['hepmc']['version'],
                     _HepTools['hepmc']['tarball'][1]],
                    stdout=hepmc_log,
                    stderr=hepmc_log)
    hepmc_log.close()
    return p 

def install_hepmc3(tmp_path):
    """Installation operations for hepmc3"""
    hepmc3_log = open(pjoin(_HepTools['hepmc3']['install_path'],"hepmc3_install.log"), "w")
    p = subprocess.call([pjoin(_installers_path,'installHEPMC3.sh'),
                     _HepTools['hepmc3']['install_path'],
                     _HepTools['hepmc3']['version'],
                     _HepTools['hepmc3']['tarball'][1]],
                    stdout=hepmc3_log,
                    stderr=hepmc3_log)
    hepmc3_log.close()
    return p 

def install_boost(tmp_path):
    """Installation operations for boost"""
    boost_log = open(pjoin(_HepTools['boost']['install_path'],"boost_install.log"), "w")
    p = subprocess.call([pjoin(_installers_path,'installBOOST.sh'),
                     _HepTools['boost']['install_path'],
                     _HepTools['boost']['version'],
                     _HepTools['boost']['tarball'][1]],
                    stdout=boost_log,
                    stderr=boost_log)
    boost_log.close()
    return p 

def install_cmake(tmp_path):
    """Installation operations for boost"""
    cmake_log = open(pjoin(_HepTools['cmake']['install_path'],"cmake_install.log"), "w")
    configuration_options = []
    if sys.platform == "darwin":
        # The cmake version of curl shipped with cmake does not work on modern macs.
        configuration_options.append('--system-curl')
    p= subprocess.call([pjoin(_installers_path,'installCMAKE.sh'),
                     _HepTools['cmake']['install_path'],
                     _HepTools['cmake']['tarball'][1],
                     ' '.join(configuration_options)],
                    stdout=cmake_log,
                    stderr=cmake_log)
    cmake_log.close()
    return p 

def install_oneloop(tmp_path):
    """Installation operations for OneLOop"""
    oneloop_log = open(pjoin(_HepTools['oneloop']['install_path'],"oneloop_install.log"), "w")
    import os, sys
    my_env = os.environ.copy()
    my_env["HEP_PYTHON"] = sys.executable
    p = subprocess.call([pjoin(_installers_path,'installOneLOop.sh'),
                      _HepTools['oneloop']['install_path'],
                     _HepTools['oneloop']['version'],
                     _HepTools['oneloop']['tarball'][1]],
                    stdout=oneloop_log,
                    stderr=oneloop_log,
                    env=my_env)
    oneloop_log.close()
    return p 

def install_collier(tmp_path):
    """Installation operat ons for COLLIER"""
    collier_log = open(pjoin(_HepTools['collier']['install_path'],"collier_install.log"), "w")    
   # Try to obtain collier version from the name of the directory in its tarball
    for path in [m.name for m in tarfile.open(_HepTools['collier']['tarball'][1],'r:gz').getmembers()]:
        match = re.compile("^COLLIER-(?P<main_version>\d+)\.(?P<subversion>\d+)$").match(os.path.basename(path))
        if not match is None:
            collier_version = "%s.%s"%(match.group('main_version'),match.group('subversion'))
            if _HepTools['collier']['version'] != collier_version:
                logger.info("HEPToolsInstaller is correcting COLLIER version to be %s"%collier_version)
                _HepTools['collier']['version'] = collier_version
            break
    
    p = subprocess.call([pjoin(_installers_path,'installCOLLIER.sh'),
                      _HepTools['collier']['install_path'],
                     _HepTools['collier']['version'],
                     _HepTools['collier']['tarball'][1],
                     _HepTools['cmake']['install_path']],
                    stdout=collier_log,
                    stderr=collier_log)
    collier_log.close()
    return p 

def install_emela(tmp_path):
    """Installation operat ons for COLLIER"""
    log = open(pjoin(_HepTools['emela']['install_path'],"emela_install.log"), "w")
    my_env = os.environ.copy()
    boost_dir = [p for p in os.listdir(_HepTools['boost']['install_path']) if p.startswith('boost') and not p.endswith('.log')][0]


    p = subprocess.call([pjoin(_installers_path,'installEMELA.sh'),
                      _HepTools['emela']['install_path'],
                     _HepTools['emela']['version'],
                     _HepTools['emela']['tarball'][1],
                     _HepTools['cmake']['install_path'],
                     os.path.join(_HepTools['lhapdf6']['install_path'], 'lhapdf6_py3'),
                     os.path.join(_HepTools['boost']['install_path'],boost_dir),
                     ],
                    stdout=log,
                    stderr=log,
                    env=my_env)
    log.close()
    return p

def install_ninja(tmp_path):
    """Installation operations for Ninja"""

    # Test whether the c++ compiler supports the -stdlib=libstdc++ option.
    # Typically clang++ supports it but not g++ which links to it by default anyway.
    cxx_flags = ['-O2']
    for flag in ['-fcx-fortran-rules','-fno-exceptions','-fno-rtti']:
        if test_cpp_compiler([flag]):
            cxx_flags.append(flag)

    ninja_log = open(pjoin(_HepTools['ninja']['install_path'],"ninja_install.log"), "w")
    p = subprocess.call([pjoin(_installers_path,'installNinja.sh'),
                     _HepTools['ninja']['install_path'],
                     _HepTools['ninja']['tarball'][1],
                     _HepTools['oneloop']['install_path'],
                     ' '.join(cxx_flags),_cpp_standard_lib], 
                    stdout=ninja_log,
                    stderr=ninja_log)
    ninja_log.close()
    return p 

def install_lhapdf6(tmp_path):
    """Installation operations for lhapdf6"""
    
    lhapdf6_log = open(pjoin(_HepTools['lhapdf6']['install_path'],"lhapdf6_install.log"), "w")
    cxx_flags = '-O2'
    #for flag in ['-static-libstdc++']:
    #   if test_cpp_compiler([flag]):
    #        cxx_flags = flag
    cxx_flags += ' -std=c++11 '
    my_env = os.environ.copy()
    my_env["PYTHON"] = sys.executable
    p = subprocess.call([pjoin(_installers_path,'installLHAPDF6.sh'),
                     _HepTools['boost']['install_path'],
                     _HepTools['lhapdf6']['install_path'],
                     _HepTools['lhapdf6']['version'],
                     _HepTools['lhapdf6']['tarball'][1],
                     ".".join([str(i) for i in sys.version_info[:2]]),
                     cxx_flags,],
                    stdout=lhapdf6_log,
                    stderr=lhapdf6_log,
                    env=my_env)
    lhapdf6_log.close()
    return p 

def install_yoda(tmp_path):
    """Installation operations for lhapdf6"""
    
    log = open(pjoin(_HepTools['yoda']['install_path'],"yoda_install.log"), "w")
    cxx_flags = '-O'
    for flag in ['-static-libstdc++']:
       if test_cpp_compiler([flag]):
            cxx_flags = flag
    cxx_flags += ' -std=c++11 '
    my_env = os.environ.copy()
    my_env["PYTHON"] = sys.executable
    p = subprocess.call([pjoin(_installers_path,'installYODA.sh'),
                     _HepTools['yoda']['install_path'],
                     _HepTools['yoda']['version'],
                     _HepTools['yoda']['tarball'][1],
                     cxx_flags],
                    stdout=log,
                    stderr=log,
                    env=my_env)
    print('install done')
    log.close()
    return p 

def install_rivet(tmp_path):
    """Installation operations for lhapdf6"""
    
    log = open(pjoin(_HepTools['rivet']['install_path'],"rivet_install.log"), "w")
    cxx_flags = ' ' #FIXME ' -O ' needs to be added back, see https://github.com/mg5amcnlo/HEPToolsInstallers/issues/3
    for flag in ['-static-libstdc++']:
       if test_cpp_compiler([flag]):
            cxx_flags = flag
    cxx_flags += ' -std=c++14 '
    my_env = os.environ.copy()
    my_env["PYTHON"] = sys.executable
    p = subprocess.call([pjoin(_installers_path,'installRIVET.sh'),
                     _HepTools['rivet']['install_path'],
                     _HepTools['rivet']['version'],
                     _HepTools['rivet']['tarball'][1],
                     _HepTools['yoda']['install_path'],
                     _HepTools['hepmc']['install_path'],
                     _HepTools['fastjet']['install_path'],
                     cxx_flags],
                    stdout=log,
                    stderr=log,
                    env=my_env)
    print('install done')
    log.close()
    return p 

def install_contur(tmp_path):
    """Installation operations for contur"""
    
    log = open(pjoin(_HepTools['contur']['install_path'],"contur_install.log"), "w")
    cxx_flags = '-O'
    for flag in ['-static-libstdc++']:
       if test_cpp_compiler([flag]):
            cxx_flags = flag
    cxx_flags += ' -std=c++14 '
    my_env = os.environ.copy()
    my_env["PYTHON"] = sys.executable
    p = subprocess.call(' '.join([pjoin(_installers_path,'installcontur.sh'),
                     _HepTools['contur']['install_path'],
                     _HepTools['contur']['version'],
                     _HepTools['contur']['tarball'][1],
                     _HepTools['rivet']['install_path'],
                     _HepTools['yoda']['install_path'],                     
                     _HepTools['hepmc']['install_path'],
                     ".".join([str(i) for i in sys.version_info[:2]]),
                     cxx_flags]),
                    stdout=log,
                    stderr=log,
                    env=my_env,
                    shell=True)
    print('install done')
    log.close()  
    return p   
    
    

def install_lhapdf5(tmp_path):
    """Installation operations for lhapdf5"""
    lhapdf5_log = open(pjoin(_HepTools['lhapdf5']['install_path'],"lhapdf5_install.log"), "w")
    p = subprocess.call([pjoin(_installers_path,'installLHAPDF5.sh'),
                     _HepTools['lhapdf5']['install_path'],
                     _HepTools['lhapdf5']['version'],
                     _HepTools['lhapdf5']['tarball'][1]],
                    stdout=lhapdf5_log,
                    stderr=lhapdf5_log)
    lhapdf5_log.close()
    return p 

def install_gsl(tmp_path):
    """Installation operations for lhapdf5"""
    gsl_log = open(pjoin(_HepTools['gsl']['install_path'],"gsl_install.log"), "w")
    p = subprocess.call([pjoin(_installers_path,'installgsl.sh'),
                     _HepTools['gsl']['install_path'],
                     _HepTools['gsl']['version'],
                     _HepTools['gsl']['tarball'][1]],
                    stdout=gsl_log,
                    stderr=gsl_log)
    gsl_log.close()
    return p 


def install_fitsio(tmp_path):
    """Installation operations for lhapdf5"""
    log = open(pjoin(_HepTools['fitsio']['install_path'],"fitsio_install.log"), "w")
    p = subprocess.call([pjoin(_installers_path,'installfitsio.sh'),
                     _HepTools['fitsio']['install_path'],
                     _HepTools['fitsio']['version'],
                     _HepTools['fitsio']['tarball'][1]],
                    stdout=log,
                    stderr=log)
    log.close()
    return p 

def install_fastjet(tmp_path):
    """Installation operations for lhapdf5"""
    log = open(pjoin(_HepTools['fastjet']['install_path'],"fastjet_install.log"), "w")
    p = subprocess.call([pjoin(_installers_path,'installfastjet.sh'),
                     _HepTools['fastjet']['install_path'],
                     _HepTools['fastjet']['version'],
                     _HepTools['fastjet']['tarball'][1]],
                    stdout=log,
                    stderr=log)
    log.close()
    return p 


def install_fjcontrib(tmp_path):
    """Installation operations for lhapdf5"""
    _HepTools['fjcontrib']['install_path'] = _HepTools['fastjet']['install_path']
    log = open(pjoin(_HepTools['fjcontrib']['install_path'],"fjcontrib_install.log"), "w")
    p = subprocess.call([pjoin(_installers_path,'installfjcontrib.sh'),
                     _HepTools['fastjet']['install_path'],
                     _HepTools['fjcontrib']['version'],
                     _HepTools['fjcontrib']['tarball'][1]],
                    stdout=log,
                    stderr=log)
    log.close()
    return p 

def install_dragon(tmp_path):
    """Installation operations for lhapdf5"""
    log = open(pjoin(_HepTools['dragon']['install_path'],"dragon_install.log"), "w")
    p = subprocess.call([pjoin(_installers_path,'installdragon.sh'),
                     _HepTools['dragon']['install_path'],
                     _HepTools['dragon']['version'],
                     _HepTools['dragon']['tarball'][1],
                    _HepTools['gsl']['install_path'],
                    _HepTools['fitsio']['install_path']],
                    stdout=log,
                    stderr=log)
    log.close()
    return p 

def install_dragon_data(tmp_path):
    """Installation operations for lhapdf5"""
    log = open(pjoin(_HepTools['dragon_data']['install_path'],"dragon_data_install.log"), "w")
    p = subprocess.call([pjoin(_installers_path,'installdragondata.sh'),
                     _HepTools['dragon_data']['install_path'],
                     _HepTools['dragon_data']['tarball'][1]
                     ],
                    stdout=log,
                    stderr=log)
    log.close()
    return p 

def install_cudacpp(tmp_path):
    tarball = _HepTools['cudacpp']['tarball'][1]
    install_path = _HepTools['cudacpp']['install_path']
    # Unpack the tarball
    shutil.unpack_archive(tarball, install_path)
    return False

def install_mg5amc_py8_interface(tmp_path):
    """ Installation operations for the mg5amc_py8_interface"""
    
    # Extract the tarball
    tar = tarfile.open(_HepTools['mg5amc_py8_interface']['tarball'][1],)
    tar.extractall(path=_HepTools['mg5amc_py8_interface']['install_path'])
    tar.close()

    # Setup the options: the pythia8 path is mandatory. The MG5 on is optimal an only necessary
    # so as to indicate which version of MG5 was present on install.
    options = [_HepTools['pythia8']['install_path']]
    if not _mg5_path is None:
        options.append(_mg5_path)

    # Run the installation script
    mg5amc_py8_interface_log = open(pjoin(_HepTools['mg5amc_py8_interface']['install_path'],"mg5amc_py8_interface_install.log"), "w")
    p = subprocess.call([sys.executable, pjoin(_HepTools['mg5amc_py8_interface']['install_path'],'compile.py')]+options, 
                    stdout=mg5amc_py8_interface_log,
                    stderr=mg5amc_py8_interface_log)
    mg5amc_py8_interface_log.close()
    return p 
    

def install_rosetta(tmp_path):
    """ Installation operations for the rosetta"""
    
    # Extract the tarball
    tar = tarfile.open(_HepTools['rosetta']['tarball'][1],)
    tar.extractall(path=_HepTools['rosetta']['install_path'])
    tar.close()


def install_madanalysis5(tmp_path):
    """ Installation operations for madanalysis5"""

#
#       raise Exception('MadAnalysis5 still request Python2')
       
    if not _keep_existing_installation:
       # Extract the tarball
       tar = tarfile.open(_HepTools['madanalysis5']['tarball'][1],)
       tar.extractall(path=_HepTools['madanalysis5']['install_path'])
       tar.close()
       
    if sys.version_info[0]!=2:
#       print('\033[0;31m Note that Madanalysis5 for Python3 is still in ALPHA stage.\033[0m')
       #subprocess.call('mv ~ma5dev/madanalysis5/v1.9_beta madanalysis5', shell=True, cwd=_HepTools['madanalysis5']['install_path'])
       print(os.listdir(_HepTools['madanalysis5']['install_path']))
       print('mv madanalysis5-%s madanalysis5' % _HepTools['madanalysis5']['version'])
       subprocess.call('mv madanalysis5-%s madanalysis5' % _HepTools['madanalysis5']['version']
                       , shell=True, cwd=_HepTools['madanalysis5']['install_path'])
       ff = open(os.path.join(_HepTools['madanalysis5']['install_path'],'madanalysis5', 'version.txt'),'w')
       ff.write('MA5 version 1.9.60\n')
       from datetime import date
       today = date.today()
       ff.write('Date %s\n' % today)
       ff.close()
       
    options = []
    
    options.append('--ma5_path=%s'%pjoin(_HepTools['madanalysis5']['install_path'],'madanalysis5'))
    options.append('--mg5_path=%s'%_mg5_path)
    options.append('--zlib=%s'%_HepTools['zlib']['install_path'])
    _delphes_path = _non_installable_dependencies['delphes']['path']
    if not _delphes_path is None:
        if not os.path.isdir(_delphes_path):
            _delphes_path = os.path.dirname(_delphes_path)
        options.append('--delphes3=%s'%os.path.normpath(pjoin(_mg5_path,_delphes_path)))
    _fastjet_path = _HepTools['fastjet']['install_path']
    if _fastjet_path and os.path.exists(_fastjet_path):
        if not os.path.isdir(_fastjet_path):
            _fastjet_path = os.path.dirname(_fastjet_path)
        options.append('--fastjet=%s'%os.path.normpath(pjoin(_mg5_path,_fastjet_path)))
    if not _HepTools['madanalysis5']['MA5_further_install']:
        options.append('--no_MA5_further_install')
    if not _HepTools['madanalysis5']['use_root_if_available']:
        options.append('--no_root_in_MA5')
    options.append('--logging=%s'%str(logger_level))    

    # Specify dependencies non-installable in the HEPToolsInstaller
    for dep in ['delphes','delphesMA5tune','root','pad','padforMA5tune']:
       if _non_installable_dependencies[dep]['active'] is not None:
          if _non_installable_dependencies[dep]['active']:
             options.append('--with-%s'%dep)            
             if not _non_installable_dependencies[dep]['path'] is None:
                options[-1] += '=%s'%_non_installable_dependencies[dep]['path']
          else:
             options.append('--veto-%s'%dep)

    # Specify dependencies installable in the HEPToolsInstaller
    for dep in ['zlib']:
        if _HepTools[dep]['install_path']:
            options.append('--with-%s=%s'%(dep,_HepTools[dep]['install_path']))
        else:
            options.append('--veto-%s'%dep)

    # Run the installation script
    madanalysis5_interface_log = open(pjoin(_HepTools['madanalysis5']['install_path'],"madanalysis5_install.log"), "w")
    logger.debug('> Calling MadAnalysis5 installer python script with:\n     %s'%' '.join([pjoin(_installers_path,'installMadAnalysis5.py')]+options))
    p = subprocess.call([sys.executable, pjoin(_installers_path,'installMadAnalysis5.py')]+options, 
                    stdout=madanalysis5_interface_log,
                    stderr=madanalysis5_interface_log)
    madanalysis5_interface_log.close()
    return p 

def install_pythia8(tmp_path):
    """Installation operations for pythia8"""
    
    # Setup optional dependencies
    optional_dependences = []
    for dep in _HepTools['pythia8']['optional_dependencies']:
        if dep=='lhapdf6':
            optional_dependences.append('--with-lhapdf6=%s'%_HepTools['lhapdf6']['install_path'])
            optional_dependences.append('--with-lhapdf6-plugin=LHAPDF6.h')
#            optional_dependences.append('--with-boost=%s'%_HepTools['boost']['install_path'])
        if dep=='lhapdf5':
            optional_dependences.append('--with-lhapdf5=%s'%_HepTools['lhapdf5']['install_path'])

    # Check whether hepmc was hacked to support named weights
    hepmc_named_weight_support = False
    try:
        tmp_dir = tempfile.mkdtemp()
        shutil.copyfile(pjoin(_installers_path,'test-hepmc2hack.cc'),
                        pjoin(tmp_dir,'test-hepmc2hack.cc'))
        hepmc_tester = [_cpp, 'test-hepmc2hack.cc','-o','test-hepmchack',
                       '-I%s'%pjoin(_HepTools['hepmc']['install_path'],'include'),
                       '-L%s'%pjoin(_HepTools['hepmc']['install_path'],'lib'),
                       '-lHepMC']
        p = subprocess.Popen(hepmc_tester, stdout=subprocess.PIPE, stderr=subprocess.PIPE, cwd=tmp_dir)
        output, error = p.communicate()
        hepmc_named_weight_support = (p.returncode == 0)
        # clean-up of the temporary file
        shutil.rmtree(tmp_dir)
    except Exception:
        try:
            shutil.rmtree(tmp_dir)
        except:
            pass
        pass
    cxx_common = ['-ldl','-fPIC',_cpp_standard_lib, '-std=c++11', '-O2', '-pthread']
    if hepmc_named_weight_support:
        cxx_common.append('-DHEPMC2HACK')
        logger.debug("The version of HepMC supports the writing of named weights: %s ", _HepTools['hepmc']['install_path'])
    else:
        logger.warning( r"|| /!\/!\/!\ ")
        logger.warning("|| Note that he following version of HEPMC\n||   %s\n|| was "%_HepTools['hepmc']['install_path']+\
              "detected as *not* supporting named weights. The MG5aMC-PY8 interface will have to write "+\
              "a separate hepmc file\n|| for each extra weight potentially specified for studying systematic"+\
              " uncertainties (PDF, scale, merging, etc...)")
        logger.warning("We recommend that you change to the version of hepmc to the one that can be installed with this tool")
        logger.warning(r"|| /!\/!\/!\ ")
    
    # Join the --cxx-common detected
    cxx_common = "'%s'"%(' '.join(cxx_common))
    # Join the optional dependencies detected
    optional_dependences = ' '.join(optional_dependences)

    # Now run the installation
    pythia_log = open(pjoin(_HepTools['pythia8']['install_path'],"pythia8_install.log"), "w")
    logger.info("mg path %s", _mg5_path)
    p = subprocess.call([pjoin(_installers_path,'installPYTHIA8.sh'),
                     _HepTools['pythia8']['install_path'],
                     _HepTools['pythia8']['tarball'][1],
                     _HepTools['hepmc']['install_path'],
                     _HepTools['zlib']['install_path'],
                     str(_mg5_path),
                     cxx_common,
                     optional_dependences], 
                    stdout=pythia_log,
                    stderr=pythia_log)
    pythia_log.close()
    return p 

def install_ginac_mg5_interface(tmp_path):
    """Installation operations for ginac_mg5_interface"""
    # Test if the interface is available
    if not os.path.isdir(_HepTools['ginac_mg5_interface']['tarball'][1]):
        logger.warning("""This version of MG5 does not include/need the GINAC/MG5 interface""")
    else:
        ginac_mg5_interface_log = open(pjoin(_HepTools['ginac_mg5_interface']['install_path'],"ginac_mg5_interface_install.log"), "w")
        p = subprocess.call([pjoin(_installers_path,'installGINAC_MG5_INTERFACE.sh'),
                     _HepTools['ginac_mg5_interface']['install_path'],
                     _HepTools['ginac_mg5_interface']['tarball'][1],
                     _cpp,
                     _prefix
                     ],
                    stdout=ginac_mg5_interface_log,
                    stderr=ginac_mg5_interface_log)
        ginac_mg5_interface_log.close()
        return p 
    
def install_ginac(tmp_path):
    """For now only check if GINAC is available in the system"""
    # Test if the interface is available
    if not os.path.isdir(_HepTools['ginac_mg5_interface']['tarball'][1]):
        logger.warning("""This version of MG5 does not include/need the GINAC/MG5 interface. No GINAC for you""")
    else:
        if which_lib('libginac.a') != None:
            logger.warning("""GINAC was not installed in MG5. For the moment it uses your system install.""")
        else:
            logger.error("""GINAC is not installed in your system. For the moment it needs to be installed externally""")
            sys.exit(1)

def install_cln(tmp_path):
    """For now only check if CLN is available in the system"""
    # Test if the interface is available
    if not os.path.isdir(_HepTools['ginac_mg5_interface']['tarball'][1]):
        logger.warning("""This version of MG5 does not include/need the GINAC/MG5 interface. No CLN for you""")
    else:
        if which_lib('libcln.a') != None:
            logger.warning("""CLN was not installed in MG5. For the moment it uses your system install""")
        else:
            logger.error("""CLN is not installed in your system. For the moment it needs to be installed externally""")
            sys.exit(1)

def install_nklo_tools(tmp_path):
    """Installation operations for NkLO_tools"""
    # Test if the interface is available

    if not os.path.isdir(_HepTools['nklo_tools']['tarball'][1]):
        logger.warning("""This version of MG5 does not include/need NkLO_tools""")
    else:
        import shutil
        if os.path.exists(_HepTools['nklo_tools']['install_path']):
            shutil.rmtree(_HepTools['nklo_tools']['install_path'])
        shutil.copytree(_HepTools['nklo_tools']['tarball'][1],_HepTools['nklo_tools']['install_path'])
        NkLO_tools_log = open(pjoin(_HepTools['nklo_tools']['install_path'],"NkLO_tools_interface_install.log"), "w")
        p = subprocess.call([pjoin(_installers_path,'installNKLO_TOOLS.sh'),
                     _HepTools['nklo_tools']['install_path']
                     ],
                    stdout=NkLO_tools_log,
                    stderr=NkLO_tools_log)
        NkLO_tools_log.close()
        return p 
    
#==================================================================================================
def finalize_installation(tool):
    """ Finalize the installation of the tool specified by copying all its libraries and executable
    files inside the corresponding lib and bin directories of HEPTools """
    
    # Create the necessary directories if they do not exist yet
    if not os.path.exists(pjoin(_prefix,'bin')):
        os.mkdir(pjoin(_prefix,'bin'))
    if not os.path.exists(pjoin(_prefix,'include')):
        os.mkdir(pjoin(_prefix,'include'))
    if not os.path.exists(pjoin(_prefix,'lib')):
        os.mkdir(pjoin(_prefix,'lib'))

    # List of all executables
    all_bin      = glob.glob(pjoin(_HepTools[tool]['install_path'],'bin','*'))
    all_include  = glob.glob(pjoin(_HepTools[tool]['install_path'],'include','*'))
    all_lib = []
    for libdir in _standard_lib_dir_names:
        all_lib.extend(glob.glob(pjoin(_HepTools[tool]['install_path'],libdir,'*')))

    # Pick the special location of library for oneloop
    if tool=='oneloop':
        all_bin      = []
        all_include  = glob.glob(pjoin(_HepTools[tool]['install_path'],'*.mod'))
        all_lib      = [pjoin(_HepTools[tool]['install_path'],'libavh_olo.a')]

    # Pick the special location of library for COLLIER
    if tool=='collier':
        all_bin      = []
        all_lib      = [pjoin(_HepTools[tool]['install_path'],'libcollier.a')]

    # Pick special executable for mg5amc_py8_interface
    if tool=='mg5amc_py8_interface':
        all_bin      += [pjoin(_HepTools[tool]['install_path'],'MG5aMC_PY8_interface')]

    # Force static linking for Ninja
    if tool in ['ninja','pythia8']:
        all_lib = [lib for lib in all_lib if not any(lib.endswith(ext) for ext in ['.so','.la','.dylib'])]

    
    for path in all_bin:
        if os.path.islink(pjoin(_prefix,'bin',os.path.basename(path))):
            os.remove(pjoin(_prefix,'bin',os.path.basename(path)))
        os.symlink(os.path.relpath(path,pjoin(_prefix,'bin')),
                               pjoin(_prefix,'bin',os.path.basename(path)))
    for path in all_include:
        if os.path.islink(pjoin(_prefix,'include',os.path.basename(path))):
            os.remove(pjoin(_prefix,'include',os.path.basename(path)))
        os.symlink(os.path.relpath(path,pjoin(_prefix,'include')),
                               pjoin(_prefix,'include',os.path.basename(path)))
    for path in all_lib: 
        if os.path.islink(pjoin(_prefix,'lib',os.path.basename(path))):
            os.remove(pjoin(_prefix,'lib',os.path.basename(path)))
        os.symlink(os.path.relpath(path,pjoin(_prefix,'lib')),
                               pjoin(_prefix,'lib',os.path.basename(path)))

#==================================================================================================

def get_data(links):
    """ Pulls up a tarball from the web """

    if not isinstance(links, list):
       links = [links]

    for link in links:
       if sys.platform == "darwin":
          program = ["curl","-OL"]
       else:
          program = ["wget",'--no-check-certificate']
       logger.info("Fetching data with command:\n  %s %s", ' '.join(program), link)
       # Here shell=True is necessary. It is safe however since program and link are not
       returncode = subprocess.call(program+[link])
       if not returncode:
          return pjoin(os.getcwd(),os.path.basename(link))
       else:
           raise Exception

# find a library in common paths 
def which_lib(lib, tool=None):
    def is_lib(fpath):
        return os.path.isfile(fpath) and os.access(fpath, os.R_OK)

    if not lib:
        return None

    fpath, fname = os.path.split(lib)
    if fpath:
        if is_lib(lib):
            return lib
    else:
        locations = sum([os.environ[env_path].split(os.pathsep) for env_path in
                   ["LIBRARY_PATH","DYLD_LIBRARY_PATH","LD_LIBRARY_PATH","PATH"] 
                                                  if env_path in os.environ],[])

        # Automatically look for the corresponding lib directories of the bin ones
        additional_locations = []
        for loc in locations:
            if os.path.basename(loc)=='bin':
                potential_other_lib_path = pjoin(os.path.dirname(loc),'lib')
                if os.path.isdir(potential_other_lib_path) and \
                   potential_other_lib_path not in locations:
                    additional_locations.append(potential_other_lib_path)
        locations.extend(additional_locations)

        # Add the default UNIX locations, but only after all the others have been searched for
        locations.extend([os.path.join(os.path.sep,'usr','lib'),os.path.join(os.path.sep,'usr','local','lib')])

        for path in locations:
            lib_file = os.path.join(path, lib)
            if is_lib(lib_file):

                # Check for the presence of other mandatory files
                if tool in _HepTools and 'mandatory_files' in _HepTools[tool]:
                    if any(not os.path.isfile(pjoin(path,mandatory_file)) for mandatory_file 
                           in _HepTools[tool]['mandatory_files']):
                        logger.debug("Mandatory files '%s' for dependency '%s' could not be found."%
                                     (', '.join(_HepTools[tool]['mandatory_files']),tool))
                        # If mandatory file not found, continue looking for the library
                        continue

                # Check lhapdf version:
                if lib.lower().startswith('lhapdf'):
                    try:
                        proc = subprocess.Popen(
                          [pjoin(path,os.path.pardir,'bin','lhapdf-config'),'--version'], 
                          stdout=subprocess.PIPE)
                        if int(proc.stdout.read()[0])!=int(lib[-1:]):
                            raise
                    except:
                        continue
                return lib_file
    return None

# Find a dependency
def find_dependency(tool):
    """ Check if a tool is already installed or needs to be."""
    if not _HepTools[tool]['install_path']:
        return None
    elif _HepTools[tool]['install_path'].lower() != 'default':
        return _HepTools[tool]['install_path']
    else:
        # Make sure it has not been installed locally in the _prefix location already
        if any( (os.path.isfile(pjoin(_prefix,tool,'lib',lib)) or \
                  os.path.isfile(pjoin(_prefix,tool,'bin',lib))) for lib in \
                                           _HepTools[tool]['libraries']):
            return pjoin(_prefix,tool)
        # For dependencies for which using a system-wide installation is forbidden,
        # force the installation
        if not _HepTools[tool]['allow_system_wide']:
            return 'TO_INSTALL'
        # Treat the default case which is "install dependency if not found
        # otherwise use the one found".
        lib_found = None
        for lib in _HepTools[tool]['libraries']:
            lib_search = which_lib(lib, tool=tool)
            if not lib_search is None:
                lib_found = lib_search
                break
        if lib_found is None:
           # search via include file
           if 'include_path' in _HepTools[tool]:
              for d in _HepTools[tool]['include_path']:
                 if not os.path.isdir(d):
                    continue
                 if any(p.startswith(tool) for p in os.listdir(d)):
                    for p in os.listdir(d):
                       if p.startswith(tool):
                          p = os.path.realpath(pjoin(d,p))
                          break
                    else:
                       continue
                    # found the include path
                    incfound = p
                    if 'lib' in os.listdir(pjoin(incfound, os.path.pardir)):
                       return os.path.abspath(pjoin(incfound, os.path.pardir))
                    elif 'lib' in os.listdir(pjoin(incfound, os.path.pardir, os.path.pardir)):
                       return os.path.abspath(pjoin(incfound, os.path.pardir,os.path.pardir))
                    else:
                       return os.path.abspath(incfound)
                 else:
                    continue
              return 'TO_INSTALL'
           return 'TO_INSTALL'
        else:
            # Return the root folder which is typically the install dir
            if os.path.basename(os.path.dirname(lib_found)) in ['lib','bin']:
                return os.path.abspath(pjoin(os.path.dirname(lib_found),os.path.pardir))
            else:
                return os.path.abspath(os.path.dirname(lib_found))

# Find lhapdf dependency
def find_lhapdf_dependency(tool):
    """ Decides which version of LHAPDF to use."""

    # First check the status for each version
    status_lhapdf5 = find_dependency('lhapdf5')
    status_lhapdf6 = find_dependency('lhapdf6')

    # Both have been vetoed
    if status_lhapdf5 is None and status_lhapdf6 is None:
        return None, None

    # If only one of the version is vetoed, install the remaining one:
    if status_lhapdf5 is None:
        return status_lhapdf6, 6
    if status_lhapdf6 is None:
        return status_lhapdf5, 5

    # At this stage the status can only take the values 'TO_INSTALL' or are alreayd installed
    # at a given path.
    # If both need to be installed, then chose LHAPDF 6
    if status_lhapdf5=='TO_INSTALL' and status_lhapdf6=='TO_INSTALL':
        return status_lhapdf6, 6

    # If both are already installed, then chose LHAPDF 6
    if status_lhapdf5 not in ['TO_INSTALL', None] and status_lhapdf6 not in ['TO_INSTALL', None]:
        return status_lhapdf6, 6

    # If only one is installed, then chose the already installed one.
    if status_lhapdf5 not in ['TO_INSTALL', None] and status_lhapdf6 in ['TO_INSTALL', None]:
        return status_lhapdf5, 5
    if status_lhapdf6 not in ['TO_INSTALL', None] and status_lhapdf5 in ['TO_INSTALL', None]:
        return status_lhapdf6, 6

    # All cases should be covered at this point
    logger.warning("Inconsistent LHPADF setup, the installer should have never reached this point.")
    sys.exit(11)

def check_successful_installation(target):
    """ Check whether the installation of target was successful or not. """
    
    # Special check for MadAnalysis installation
    if target=='madanalysis5':
        return 'INSTALLATION STATUS: SUCCESS' in open(pjoin(_HepTools['madanalysis5']['install_path'],'madanalysis5_install.log'),'r').readlines()[-1]
    if target=='rosetta':
       import sys
       sys.path.append(pjoin(_HepTools[target]['install_path']))
       try:
          import Rosetta
       except:
          if sys.version_info[0]!=2 and sys.version_info[1] !=7:
             logger.warning('\033[1;31mRosetta requires python2.7. Impossible to test the installation but the code is downloaded correctly.\033[0m')
             return True
          else:
             return False
       else:
          return True
    if target == 'dragon_data':
       return os.path.exists(pjoin(_HepTools[target]['install_path'],'data'))

    if target == 'contur':
        return True #'You should source data/share/analysis-list' in open(pjoin(_HepTools['contur']['install_path'],'contur_install.log'),'r').read()
    for f in _HepTools[target]['libraries']:
        if any(f.endswith(extension) for extension in _lib_extensions):
            for libdir in _standard_lib_dir_names:
                if os.path.exists(pjoin(_HepTools[target]['install_path'],libdir,f)):
                    return True
            if os.path.exists(pjoin(_HepTools[target]['install_path'],f)):
                return True
        if os.path.exists(pjoin(_HepTools[target]['install_path'],f)):
            return True
    if target == 'emela':
       if os.path.exists(pjoin(_HepTools[target]['install_path'], 'bin', 'eMELA-config')):
            return True
       else:
          print(pjoin(_HepTools[target]['install_path'], 'bin', 'emela-config'))
    return False

def install_with_dependencies(target,is_main_target=False):
    """ Recursive install function for a given tool, taking care of its dependencies"""
    # Make sure to actualize the path if set to default, as the target is now no longer a dependency but what
    # is being installed
    if _HepTools[target]['install_path'] == 'default':
        _HepTools[target]['install_path'] = pjoin(_prefix,target)
        if target == 'lhapdf6':
            _HepTools[target]['install_path'] += '_py3' if PY3 else ''    

    dependency_index = 0
    all_dependencies = _HepTools[target]['mandatory_dependencies']+_HepTools[target]['optional_dependencies']
    while True:
        try:
            dependency = all_dependencies[dependency_index]
        except IndexError:
            break
        else:
            dependency_index +=1
        # Special treatment for lhapdf. 
        if dependency == 'lhapdf':
            path, version = find_lhapdf_dependency(dependency)
            if not version is None:
                try:
                    _HepTools[target]['mandatory_dependencies'][
                       _HepTools[target]['mandatory_dependencies'].index('lhapdf')]='lhapdf%d'%version
                except ValueError:
                    pass
                try:
                    _HepTools[target]['optional_dependencies'][
                       _HepTools[target]['optional_dependencies'].index('lhapdf')]='lhapdf%d'%version
                except ValueError:
                    pass
                dependency = 'lhapdf%d'%version
                if version==6:
                    # We must add boost as a mandatory dependency since lhapdf6 will be used.
                    _HepTools[target]['mandatory_dependencies'].append('boost')
                    all_dependencies.append('boost')
        # Special treatment of oneloop as well. We want it locally.
        elif dependency == 'oneloop':
            if os.path.isfile(pjoin(_prefix,'oneloop','libavh_olo.a')):
                path = pjoin(_prefix,'oneloop')
            else:
                path = 'TO_INSTALL'
        else:
            path = find_dependency(dependency)
        if path is None:
            if dependency in _HepTools[target]['optional_dependencies']:
                logger.info("Optional '%s' dependency '%s' is disabled and will not be available.", target, dependency)
                _HepTools[target]['optional_dependencies'].remove(dependency)
            else:
                logger.critical("Mandatory '%s' dependency '%s' unavailable. Exiting now.", target, dependency)
                sys.exit(9)
        elif path=='TO_INSTALL':
            logger.info("Detected '%s' missing dependency: '%s'. Will install it now.", target, dependency)
            install_with_dependencies(dependency)
        else:
            logger.debug("'%s' dependency '%s' found at:\n  %s", target, dependency, path)
            _HepTools[dependency]['install_path']=path

    with TMP_directory(keep_source=_keep_source, tool=target) as tmp_path:
        # Get the source tarball if online
        if _HepTools[target]['tarball'][0]=='online':
            logger.debug("Downloading '%s' sources...", target)
            try:
                tarball_path = get_data(_HepTools[target]['tarball'][1])
                if not os.path.exists(tarball_path):
                    logger.critical("Could not download data at '%s'."%tarball_path)
                    sys.exit(9)
            except Exception as e:
                logger.critical("Could not download data at '%s' because of:\n%s\n"%(_HepTools[target]['tarball'][1],str(e)))
                sys.exit(9)
            _HepTools[target]['tarball'] = ('local',tarball_path)

        if not os.path.isdir(_HepTools[target]['install_path']):
            os.mkdir(_HepTools[target]['install_path'])
        logger.info("Installing tool '%s'...", target)
        logger.info("    > Follow the installation progress by running the command below in a separate terminal)")
        logger.info("    > tail -f %s", pjoin(_HepTools[target]['install_path'],'%s_install.log'%target))
        return_code = eval('install_%s(tmp_path)' % target)
        print("RETURN CODE AT PYTHON:", return_code)
        if not is_main_target:
            if check_successful_installation(target) and return_code == 0:
                # Successful installation, now copy the installed components directly under HEPTools
                finalize_installation(target)
                logger.info("    > Successful installation of dependency '%s' in '%s'."%(target,_prefix))
                logger.info("    > See installation log at '%s'."%pjoin(_HepTools[target]['install_path'],'%s_install.log'%target))
            else:
                logger.info("    > A problem occured during the installation of dependency '%s'.", target)
                try:
                    logger.info("    > Content of the installation log file '%s':\n\n%s"%(\
                            pjoin(_HepTools[target]['install_path'],'%s_install.log'%target),
                            open(pjoin(_HepTools[target]['install_path'],'%s_install.log'%target),'r').read()))
                except IOError:
                    logger.info("    > No additional information on the installation problem available.")
                logger.info("    > Now aborting installation of tool '%s'."%target_tool)
                sys.exit(9)
        elif return_code != 0:
            logger.info("    > A problem occured during the installation of '%s'.", target)
            logger.info("    > Return code of the installation is %s" , return_code)
            logger.info("    > See installation log at '%s' "%pjoin(_HepTools[target]['install_path'],                '%s_install.log'%target))
            sys.exit(return_code)

if "__main__" == __name__:
    _environ = dict(os.environ)
    try:   
        os.environ["CXX"]     = _cpp
        os.environ["FC"]      = _gfortran
        # Also add the bin directory of the HEPTools install location, as we might need some of the executables installed there like cmake
        os.environ["PATH"]    = pjoin(_prefix,'bin')+os.pathsep+os.environ["PATH"]
        install_with_dependencies(target_tool,is_main_target=True)
    except ZeroDivisionError as e:
        os.environ.clear()
        os.environ.update(_environ)
        logger.critical("The following error occured during the installation of '%s' (and its dependencies):\n%s"%(target_tool,repr(e)))
        sys.exit(9)

    os.environ.clear()
    os.environ.update(_environ)

    if check_successful_installation(target_tool):
        # Successful installation, now copy the installed components directly under HEPTools
        finalize_installation(target_tool)
        logger.info("Successful installation of '%s' in '%s'."%(target_tool,_prefix))
        logger.debug("See installation log at '%s'."%pjoin(_HepTools[target_tool]['install_path'],'%s_install.log'%target_tool))
        sys.exit(0)
    else:
        logger.warning("A problem occured during the installation of '%s'.", target_tool)
        try:
          logger.warning("Content of the installation log file '%s':\n\n%s"%(\
            pjoin(_HepTools[target_tool]['install_path'],'%s_install.log'%target_tool),
            open(pjoin(_HepTools[target_tool]['install_path'],'%s_install.log'%target_tool),'r').read()))
        except IOError:
          logger.warning("No additional information on the installation problem available.")
        sys.exit(9)

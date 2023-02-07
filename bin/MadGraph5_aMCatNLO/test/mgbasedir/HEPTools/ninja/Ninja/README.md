Ninja
=====

- Ninja - version 1.1.0
- Author: Tiziano Peraro

*Ninja* implements the *Laurent series expansion* method for the
computation of *one-loop integrals*.

It is based on:

- P. Mastrolia, E. Mirabella and T. Peraro,
   "*Integrand reduction of one-loop scattering amplitudes through
   Laurent series expansion*,"
   JHEP 1206 (2012) 095
   [arXiv:1203.0291 [hep-ph]](http://arxiv.org/abs/arXiv:1203.0291).

- T. Peraro,
   "*Ninja: Automated Integrand Reduction via Laurent Expansion for
   One-Loop Amplitudes*,"
   Comput. Phys. Commun. 185 (2014) 2771
   [arXiv:1403.1229 [hep-ph]](http://arxiv.org/abs/1403.1229)


Installing Ninja
----------------

Ninja can be compiled and installed with the commands

    ./configure
    make
    make install

In order to compile and link the built-in interface to the
[OneLoop](http://helac-phegas.web.cern.ch/helac-phegas/OneLOop.html)
library use `--with-avholo[=FLAGS]` during configuration e.g.

    ./configure --with-avholo
    make
    make install

if OneLoop is installed in a standard directory, or

    ./configure --with-avholo='-L/path/to/avh_olo/lib -lavh_olo' \
        FCINCLUDE=-I/path/to/avh_olo/include
    make
    make install

if it is installed in `/path/to/avh_olo/lib` and the corresponding
Fortran90 module `avh_olo` is installed in `/path/to/avh_olo/include`.

In order to compile the built-in interface to the
[LoopTools](http://www.feynarts.de/looptools) library (requires
LoopTools-2.9 or later) use `--with-looptools[=FLAGS]` during
configuration e.g.

    ./configure \
        --with-looptools='fldflags -L/path/to/looptools/lib -looptools' \
        CPPFLAGS=-I/path/to/looptools/include
    make
    make install

where `/path/to/looptools/` is the directory where LooTools is
installed and `fldflags` stands for the additional flags the library
requires for static linking (you can find them e.g. in the file
`bin/fcc` in the main installation directory of LoopTools).  You can
omit the `FLAGS` argument of `--with-looptools` if you do not wish to
compile the examples.

For a full list of available options type

    ./configure --help

See the file `INSTALL` for more information about the installation
procedure.

If you specified the `--prefix` option for the configure scrip, you
should also add the directory where the library is installed to your
`LD_LIBRARY_PATH` (and `DYLD_LIBRARY_PATH` on Mac OS) environment
variable, if not already present.  This is typically a sub-directory
`lib/` or `lib64/` of the one you specified with `--prefix`.

The examples which come with the distribution can be compiled, after
installation, using the command
    
    make examples

either in the top directory or in the examples/ directory.  Examples
using POSIX threads can be compiled (if supported) with

    make thread-examples



Installing the package NinjaNumGen
----------------------------------

The Python package *NinjaNumGen* can be used both as a Python module
and as a script in order to generate the input needed for Ninja.  It
can be installed using the command

    python setup.py install

in the `utils/` directory.  The command
    
    python setup.py --help

will give a list of installation options.  If you specified the
`--prefix` option during installation, update your `PATH` and
`PYTHONPATH` environment variables accordingly.

The package NinjaNumGen requires [Form-4](http://www.nikhef.nl/~form)
or later.

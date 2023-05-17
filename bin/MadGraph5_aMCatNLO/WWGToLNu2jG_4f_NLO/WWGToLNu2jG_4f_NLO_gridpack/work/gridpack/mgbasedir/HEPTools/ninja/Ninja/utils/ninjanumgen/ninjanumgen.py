#! /usr/bin/env python

__all__ = ['NinjaPP', 'DiagramExpansion',
           'ProgrammingLanguage', 'CPP', 'C', 'F77', 'F90']

import subprocess
import os
import sys
import errno
import re
import tokenize
import cStringIO


DEVNULL = open(os.devnull, 'w')

def make_extension(name,newext):
    return os.path.splitext(name)[0] + newext

def silent_delete(path):
    "Delete a file if exists"
    try:
        os.remove(path)
    except OSError as err:
        if err.errno != errno.ENOENT:
            raise



ninjappreg = re.compile('^\s*#\s*ninja_([A-Za-z0-9_]+)\s*(.*)')

class NinjaPP(dict):
    "Ninja Preprocessor"

    def __init__(self,methods = dict()):
        super(NinjaPP,self).__init__(methods)
        self._macros = dict()
        self._macros_reg = None

    def addMacros(self,**kwargs):
        for (name,value) in kwargs.iteritems():
            self._macros[name]=value
        self._macros_reg = \
            re.compile(r'\b({0})\b'.format('|'.join(self._macros.keys())))

    def _subst_macros(self,x):
        return self._macros[x.group()]

    def __call__(self,line,output):
        m = ninjappreg.match(line)
        if m:
            self[m.group(1)](m.group(2),output)
        elif self._macros_reg:
            output.write(self._macros_reg.sub(self._subst_macros,line))
        else:
            output.write(line)



def write_with_float_check(out,wrapper,s):
    try:
        float(s)
        out.write('{0}({1})'.format(wrapper,s))
    except ValueError:
        out.write(s+' ')

class CPPFormatter(object):
    def __init__(self,ofile):
        self._out = ofile
        self._keyw = {'NinjaAbbrType' : self._abbr_def,
                      'ninjanumabbr' : self._abbr,
                      'ninjaC' : self._coeff,
                      'pow' : self._pow,
                      '[' : self._form_square_brackets}
        #              'NinjaFormAbbr' : self._form_abbr_def} 
        self._tokens = None

    def __call__(self,line):
        readl = cStringIO.StringIO(line).readline
        self._tokens = (_[1] for _ in tokenize.generate_tokens(readl))
        for token in self._tokens:
            if token in self._keyw:
                self._keyw[token]()
            else:
                write_with_float_check(self._out,'NINJA_REAL',token)
        self._tokens = None

    def _form_square_brackets(self):
        "Contents od Form square brackets are left as they are"
        for token in self._tokens:
                if token == ']':
                    return
                self._out.write(token)

    def _arrayidx(self):
        in_par = False
        for token in self._tokens:
            if (in_par == False) and (token == '['):
                self._out.write('[%s+' % self._idxshift)
                in_par = True
            elif (token == ']') or (token == ';'):
                self._out.write(token)
                return
            else:
                self._out.write(token)

    def _abbr_def(self):
        self._out.write('NinjaAbbrType ')
        prev_token=None
        for token in self._tokens:
            if (prev_token == '[') and not (token.isspace()):
                dim = int(token)
                dim = dim+1 if dim == 0 else dim
                self._out.write(str(dim))
                prev_token = token
            elif token == ';':
                self._out.write(token)
                self._out.write('  (void)(ninjanumabbr);')
                return
            else:
                self._out.write(token)
                prev_token = token

    def _abbr(self):
        self._out.write('ninjanumabbr')
        self._idxshift = '-1'
        self._arrayidx()

    def _coeff(self):
        self._out.write('ninjaC')
        self._idxshift = '0'
        self._arrayidx()

    # def _form_abbr_def(self):
    #     self._out.write('NinjaFormAbbr')
    #     for token in self._tokens:
    #         if token == ';':
    #             self._out.write(token)
    #             return
    #         else:
    #             self._out.write(token)

    def _pow(self):
        in_arg = False
        in_exp = False
        exp = ''
        balanced_pars = 0
        old_out = None
        argbuffer = cStringIO.StringIO()
        for token in self._tokens:
            if token in self._keyw:
                self._keyw[token]()
            elif token == '(':
                if not in_arg:
                    in_arg = True
                    old_out = self._out
                    self._out = argbuffer
                else:
                    self._out.write('(')
                balanced_pars += 1
            elif token == ')':
                balanced_pars -= 1
                if balanced_pars == 0:
                    argument = self._out.getvalue()
                    self._out = old_out
                    exp = int(exp)
                    if 0 < exp <= 9:
                        self._out.write('powi{0}({1})'.format(exp,argument))
                    elif exp > 9:
                        self._out.write('powi({1},{0})'.format(exp,argument))
                    elif -9 <= exp < 0:
                        self._out.write('(NINJA_REAL(1)/powi{0}({1}))'
                                        .format(-exp,argument))
                    elif exp < -9:
                        self._out.write('(NINJA_REAL(1)/powi({1},{0}))'
                                        .format(-exp,argument))
                    exp = ''
                    return
                else:
                    self._out.write(')')
            elif (balanced_pars == 1) and (token == ','):
                in_exp = True
            elif in_exp:
                exp += token
            elif in_arg:
                write_with_float_check(self._out,'NINJA_REAL',token)



class ProgrammingLanguage(object):
    pass

class ProgrammingLanguageError(Exception):
    def __init__(self, value):
        self.value_ = value
    def __str__(self):
        return repr(self.value_)

class CPP(ProgrammingLanguage):
    string = 'CPP'
    form = 'C'
    extension = 'cc'
    const_int = '  const int {0} = {1};\n'
    formatter = CPPFormatter

    @staticmethod
    def include(includes):
        return ''.join('#include "{0}"\n'.format(_) for _ in includes)


class C(ProgrammingLanguage):
    pass

class F77(ProgrammingLanguage):
    pass

class F90(ProgrammingLanguage):
    pass


# Path with template files
TPATH = os.path.join(os.path.dirname(os.path.realpath(__file__)),'templates')


# QuadNinja stuff
quadninja_regs_ = [(r"#[ ]*include[ ]*<ninja/", "#include <quadninja/"),
                   (r"namespace +ninja", "namespace quadninja"),
                   (r"ninja::", "quadninja::"),
                   (r"(?<=[^A-Za-z0-9])NINJA_", "QUADNINJA_"),
                   (r"ninjavholo_", "quadninjavholo_"),
                   (r"//quadninja//", "")]
    
def quadninja_translate(fname, outfile, cdname, header=None):
    "Translate a ninja file in a quadninja file"
    with open(fname, "r") as infile:
        with open(outfile, "w") as outfile:
            for line in infile:
                outline = line
                if header:
                    outline = outline.replace('"%s"' % header,
                                              '"quad%s"' % header)
                outline = re.sub(r"\b%s\b" % cdname,
                                 "Quad%s" % cdname, outline)
                for reg in quadninja_regs_:
                    outline = re.sub(reg[0],reg[1],outline)
                outfile.write(outline)

def quadninja_translate_hguard(fname, outfile):
    "Prepends QUAD to headerguards"
    content = ''
    with open(fname, "r") as infile:
        content = infile.read()
    with open(outfile, "w") as out:    
        out.write(re.sub(r'# *ifndef +([A-Za-z0-9_]+)\n *# *define +\1',
                         r'#ifndef QUAD\1\n#define QUAD\1',
                         content))

        
class DiagramExpansion(object):
    '''Class for building the Laurent-expansion methods needed by
    Ninja.
    '''
    def __init__(self, infile, outfile,
                 nlegs, rank=None,
                 diagname='Diagram',
                 formexec='form', formflags='', q='Q', mu2='Mu2',
                 cdiagname=None, include = [],
                 header = None,
                 namespace = '0',
                 optimize = 2,
                 inline = [], inline_evaluate = [],
                 inline_mu2expansion = [],
                 inline_t3expansion = [], inline_t2expansion = [],
                 loop_prefix = None,
                 language=CPP,
                 verbose = False):
        '''Constructor of the DiagramExpansion class.


        Positional arguments:

        infile -- name of an input file containing a Form expression
                  of the numerator

        outfile -- name of the output file which will be produced with
                   the definition of the methods

        nlegs -- number of external legs of the diagram


        Keyword arguments:

        rank -- rank of the numerator, if None it is assumed to be
                equal to the number of external legs (default None)

        diagname -- name of the Form expression of the numerator of
                    the diagram [default 'Diagram']

        formexec -- form executable (default 'form')

        formflags -- flags to be passed to the form executable (default '')

        q -- name of the loop momentum in the Form expression (default 'Q')

        mu2 -- name of the \\mu^2 variable in the Form expression
               (default 'Mu2')

        cdiagname -- name of the numerator class in the output, if
                     None it will be the same as the diagname (default
                     None)

        include -- list of names of header files to be included in the
                   output source (default list())

        header -- name of the header file with the Numerator class
                  definition, if None it is assumed to be the same as
                  outfile, but with '.hh' extension

        namespace -- if different from '0', should be the namespace
                     containing the Numerator class definition
                     (default '0')

        optimize -- Form optimization level (default 2)

        inline -- list of code lines to be inlined in the generated
                  source file, inside the definition of every method
                  which computes the numerator and its expansions

        inline_evaluate -- list of code lines to be inlined inside the
                           generated 'evaluate' method

        inline_mu2expansion -- list of code lines to be inlined inside
                               the generated 'mu2Expansion' method

        inline_t3expansion -- list of code lines to be inlined inside
                              the generated 't3Expansion' method

        inline_t2expansion -- list of code lines to be inlined inside
                              the generated 't2Expansion' method

        verbose -- set to True for printing information of the ongoing
                   computations

        '''

        self._dname = diagname
        self._cdname = self._dname if (cdiagname is None) else cdiagname
        self._lang = language 
        if not (language is CPP):
            raise ProgrammingLanguageError("Sorry, only C++ output "
                                           "is available in this version "
                                           "of ninjanumgen.")
        self._include = include
        self._header = header
        self._namespace = namespace
        self._n = int(nlegs)
        self._r = self._n if (rank is None) else int(rank)
        self._formexec = formexec
        self._formflags = formflags.split()
        self._q = q
        self._mu2 = mu2
        self._file = infile
        self._out = outfile
        self._opt = optimize
        self._inline = inline
        self._inline_eval = inline_evaluate
        self._inline_mu2 = inline_mu2expansion
        self._inline_t3 = inline_t3expansion
        self._inline_t2 = inline_t2expansion
        self._verbose = verbose
        self._args = [self._formexec,
                      '-D', 'DIAGNAME='+self._dname,
                      '-D', 'DIAGN='+str(self._n),
                      '-D', 'DIAGRANK='+str(self._r),
                      '-D', 'QVAR='+self._q,
                      '-D', 'MU2VAR='+self._mu2] + self._formflags
        if (loop_prefix):
            self._args += ['-D', 'LOOPPREFIX='+loop_prefix]
        self._done = False
            

    def _performExpansions(self):
        extra_args = ['-D', 'OUTFILE='+self._out+'.ninja_laurent.out',
                      '-D', 'DIAGFILE='+self._file,
                      '-D', 'NINJADIAGID=0',
                      os.path.join(TPATH,'ninja_laurent.frm')]
        subprocess.check_call(self._args + extra_args, stdout=DEVNULL)
        
    def _performFormOptimization(self,idstr):
        extra_args = ['-D', 'OUTFILE=outfile.out',
                      '-D', 'DIAGFILE='+self._file,
                      '-D', 'TEMPPREFIX='+self._out+'.',
                      '-D', 'EXPANSIONFILE=ninja_laurent.out',
                      '-D', 'EXPANSIONID='+idstr,
                      '-D', 'OPTIMIZATIONLEVEL='+str(self._opt),
                      '-D', 'LANGUAGE='+self._lang.form,
                      os.path.join(TPATH,'ninja_opt.frm')]
        subprocess.check_call(self._args + extra_args, stdout=DEVNULL)

    def _performOptimizations(self):
        if self._verbose:
            print "- optimizing evaluate method..."
        self._performFormOptimization('SAMURAINUM')
        if self._verbose:
            print "- optimizing t3expansion method..."
        self._performFormOptimization('31')
        if self._n >= 3:
            self._performFormOptimization('32')
        if self._verbose:
            print "- optimizing t2expansion method..."
        self._performFormOptimization('21')
        self._performFormOptimization('22')
        if (self._n >= 4) and (self._r >= self._n):
            if self._verbose:
                print "- optimizing mu2expansion method..."
            self._performFormOptimization('Mu2')

    def _preprocessor(self):
        "Defines preprocessor operations"
        def include(arg,out):
            out.write(self._lang.include(self._include))
        definition_dict = {'NINJA_NUM' : self._cdname,
                           'NINJA_RANK_MINUS_N' : '(%s)'%str(self._r-self._n),
                           'NINJA_NUM_NAMESPACE' : self._namespace}
        def cpp_definition(arg,out):
            out.write('#define {0} {1}\n'.format(arg,definition_dict[arg]))
        def cpp_include_guards(arg,out):
            if 'ON' in arg:
                out.write('#ifndef {0}_HH\n'.format(self._cdname.upper()))
                out.write('#define {0}_HH\n'.format(self._cdname.upper()))
            elif 'OFF' in arg:
                out.write('#endif // {0}_HH\n'.format(self._cdname.upper()))
        def inline(inlines):
            def inline_func(arg,out):
                out.write(''.join((_ + '\n') for _ in inlines))
            return inline_func
        def mu2_indices(arg,out):
            i = 0
            for idx in range (self._r - self._n,-1,-1):
                name = 'ninjaidxt%s' % str(idx)
                out.write(self._lang.const_int.format(name,i))
                i += 1
        def t3_indices(arg,out):
            i = 0
            for tidx in range (3 + self._r - self._n,-1,-1):
                for muidx in range (0,3 + self._r-self._n-tidx+1,2):
                    name = 'ninjaidxt%smu%s' % (str(tidx), str(muidx))
                    out.write(self._lang.const_int.format(name,i))
                    i += 1
        def t2_indices(arg,out):
            i = 0
            for tidx in range (2+self._r-self._n,-1,-1):
                for xidx in range (0,2+self._r-self._n-tidx+1):
                    for muidx in range (0,2+self._r-self._n-tidx-xidx+1,2):
                        name = 'ninjaidxt%sx%smu%s' \
                            % (str(tidx), str(xidx), str(muidx))
                        out.write(self._lang.const_int.format(name,i))
                        i += 1
        def write_file(name):
            def write(arg,out):
                buff = ''
                formatted_write = self._lang.formatter(out)
                with open(self._out+'.'+name) as f:
                    for l in f:
                        buff += l.lstrip(' \t')
                        if ';' in l:
                            formatted_write(buff)
                            buff = ''
            return write
        def dummy(args,out):
            return None
        pp = NinjaPP()
        pp['includes'] = include
        pp['cpp_definition'] = cpp_definition
        pp['cpp_include_guards'] = cpp_include_guards
        pp['inline'] = inline(self._inline)
        pp['inline_evaluate'] = inline(self._inline_eval)
        pp['inline_t3expansion'] = inline(self._inline_t3)
        pp['inline_t2expansion'] = inline(self._inline_t2)
        pp['inline_mu2expansion'] = inline(self._inline_mu2)
        pp['numerator_evaluate'] = write_file('ninja_optSAMURAINUM.out')
        pp['numerator_t3expansion1'] = write_file('ninja_opt31.out')
        if self._n >= 3:
            pp['numerator_t3expansion2'] = write_file('ninja_opt32.out')
        else:
            pp['numerator_t3expansion2'] = dummy
        pp['numerator_t2expansion1'] = write_file('ninja_opt21.out')
        pp['numerator_t2expansion2'] = write_file('ninja_opt22.out')
        pp['t3expansion_indices'] = t3_indices
        pp['t2expansion_indices'] = t2_indices
        if (self._n >= 4) and (self._r >= self._n):
            pp['numerator_mu2expansion'] = write_file('ninja_optMu2.out')
            pp['mu2expansion_indices'] = mu2_indices
        else:
            pp['numerator_mu2expansion'] = dummy
            pp['mu2expansion_indices'] = dummy
        #pp.addMacros(NINJA_NUM = 'MyNum')
        return pp

    def _cleanup(self):
        cleanup_list = ['ninja_laurent.out',
                        'ninja_optSAMURAINUM.out',
                        'ninja_opt31.out',
                        'ninja_opt32.out',
                        'ninja_opt21.out',
                        'ninja_opt22.out',
                        'ninja_optMu2.out']
        for f in cleanup_list:
            silent_delete(self._out+'.'+f)

    def _writeSource(self):
        if self._verbose:
            print "Form is performing the Laurent expansions..."
        self._performExpansions()
        if self._verbose:
            print "Form is optimizing the expressions:"
        self._performOptimizations()
        if self._verbose:
            print "NinjaNumGen is writing the output source..."
        process = self._preprocessor()
        if (self._lang is CPP) or (self._lang is C):
            if self._header is None:
                self._header = make_extension(self._out,'.hh')
            self._include.append(self._header)
            if not os.path.exists(self._header):
                with open(self._header,'w') as out:
                    fname = os.path.join(TPATH,'ninjanumgen_template.hh')
                    with open(fname) as f:
                        for l in f:
                            process(l,out)
        with open(self._out,'w') as out:
            fname = os.path.join(TPATH,
                                 'ninjanumgen_template.'+self._lang.extension)
            with open(fname) as f:
                for l in f:
                    process(l,out)
        if self._verbose:
            print "NinjaNumGen is done!"

    def writeSource(self):
        "Perform the expansions and writes the source code"
        self._done = False
        try:
            self._writeSource()
        except:
            sys.stderr.write('ERROR IN NINJANUMGEN: Something went wrong!\n')
            sys.stderr.write('ERROR IN NINJANUMGEN: Please check your input'
                             ' and your Form expression.\n')
            raise
        finally:
            self._cleanup()
        self._done = True
        
    def writeQuadSource(self, existing=False):
        "Writes to source for QuadNinja"
        prefix = "quad"
        if not existing and not self._done:
            self.writeSource()
        if self._header is None:
            self._header = make_extension(self._out,'.hh')
        quadninja_translate(self._out, prefix + self._out,
                            self._cdname, self._header)
        if not os.path.exists(prefix + self._header):
            quadninja_translate(self._header, prefix + self._header,
                                self._cdname)
            quadninja_translate_hguard(prefix + self._header,
                                       prefix + self._header)

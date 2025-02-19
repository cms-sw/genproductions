#####################################################
#                                                   #
#  Source file of the Matrix Elements exports for   #
#  the MadSTR MG5aMC plugin.                         #
#                                                   #
#####################################################

import os
import logging
import shutil
import itertools
import copy
from math import fmod
import subprocess
import re

plugin_path = os.path.dirname(os.path.realpath( __file__ ))

from madgraph import MadGraph5Error, InvalidCmd, MG5DIR
import madgraph.iolibs.export_fks as export_fks
import madgraph.iolibs.file_writers as writers
import madgraph.various.misc as misc
import madgraph.various.banner as banner_mod
import madgraph.iolibs.helas_call_writers as helas_call_writers
import madgraph.iolibs.files as files
import madgraph.iolibs.drawing_eps as draw

logger = logging.getLogger('MadSTR_plugin.MEExporter')

pjoin = os.path.join 

class MadSTRExporterError(MadGraph5Error):
    """ Error from the Resummation MEs exporter. """ 

class MadSTRExporter(export_fks.ProcessOptimizedExporterFortranFKS):
    
    # check status of the directory. Remove it if already exists
    check = True 
    # Language type: 'v4' for f77 'cpp' for C++ output
    exporter = 'v4'
    # Output type:
    #[Template/dir/None] copy the Template, just create dir  or do nothing 
    output = 'Template'
    # Decide which type of merging if used [madevent/madweight]
    grouped_mode = False
    # if no grouping on can decide to merge uu~ and u~u anyway:
    sa_symmetry = False

    template_path = pjoin(plugin_path,'MadSTRTemplate')
    
    def __init__(self, *args, **opts):
        """ Possibly define extra instance attribute for this daughter class."""
        return super(MadSTRExporter, self).__init__(*args, **opts)
    
    def read_template_file(self, name):
        """ Read a given template file. In conjunction of the use of class attributes,
        this is to help making the choice of the template file modular."""
        return open(pjoin(self.template_path,name),'r').read()
    
    def copy_fkstemplate(self, *args, **opts):
        """Additional actions needed for setup of Template
        """

        super(MadSTRExporter, self).copy_fkstemplate(*args, **opts)

        # Files or directories to copy from MadSTR templates
        to_copy_from_madstr_templates = \
                       [ pjoin('SubProcesses','transform_os.f'),
                         pjoin('SubProcesses','test_OS_subtr.f'),
                         ]
        
        for path in to_copy_from_madstr_templates:
            if os.path.isfile(pjoin(self.template_path, path)):
                shutil.copy(pjoin(self.template_path, path), pjoin(self.dir_path, path))
            elif os.path.isdir(pjoin(self.template_path, path)):
                shutil.copytree(pjoin(self.template_path, path), pjoin(self.dir_path, path))
            else:
                raise MadSTRExporterError("Template '%s' not found."%pjoin(self.template_path, path))

        self.update_fks_makefile(pjoin(self.dir_path, 'SubProcesses', 'makefile_fks_dir'))
        self.update_run_inc(pjoin(self.dir_path, 'Source', 'run.inc'))
        
        # Write makefile

        # We need to create the correct open_data for the pdf, which 
        # is normally not to be done in SA output.
        self.write_pdf_opendata()


    def generate_directories_fks(self, matrix_elements, *args):
        """ write the files in the P* directories.
        Call the mother and then add the OS infos
        """
        calls = super(MadSTRExporter, self).generate_directories_fks(matrix_elements, *args)


        Pdir = pjoin(self.dir_path, 'SubProcesses', \
                       "P%s" % matrix_elements.get('processes')[0].shell_string())

        # the file with the informations for on-shell subtraction
        filename = pjoin(Pdir, 'osinfo.dat')
        self.write_osinfo_file(matrix_elements, filename)

        # link extra files
        linkfiles = ['transform_os.f', 'test_OS_subtr.f']
        for f in linkfiles:
            files.ln('../%s' % f, cwd=Pdir)
        # Add the os_ids to os_ids.mg
        os_ids = self.get_os_ids_from_me(matrix_elements)
        filename = pjoin(self.dir_path, 'SubProcesses', 'os_ids.mg')
        files.append_to_file(filename,
                             self.write_os_ids,
                             Pdir,
                             os_ids)
        return calls

    
    def write_osinfo_file(self,matrix_element,outfilename):
        """write a .dat file with the on-shell informations
        """
        outfile = open(outfilename, 'w')
        content = ""
        for i, fksinfo in enumerate(matrix_element.get_fks_info_list()):
            content += "F %3d " % (i+1)
            real = matrix_element.real_processes[fksinfo['n_me'] - 1]
            content += " %3d\n" % len(real.os_ids)
            for os_id, dau_pos in zip(real.os_ids, real.os_daughter_pos):
                content += "O "
                content += " ".join(["%3d" % v for v in os_id]) +  "     "
                content += " ".join(["%3d" % (v+1) for v in dau_pos]) +  "\n"
        outfile.write(content)
        outfile.close()


    def get_os_ids_from_me(self, matrix_element):
        """Returns the list of the OS ids (resonant particles"""
        #if hasattr(self, "os_ids"): return self.os_ids
        os_ids = []
        for real in matrix_element.real_processes:
            # append only the mother particle, i.e. the 1st particle in each list of ids
            os_ids += [ids[0] for ids in real.os_ids]
        return set(os_ids)


    def get_os_ids_from_file(self, filepath):
        """Returns the list of the OS ids (resonant particles), reading 
        the os_ids file.
        It has a series of line with the format
        P0_xx_yy: id1 id2 id3"""
        lines = open(filepath).read().split('\n')
        os_ids = []
        for l in lines:
            if not l: continue
            os_ids += [int(v) for v in l.split(':')[1].split()]
        return set(os_ids)



    def draw_feynman_diagrams(self, matrix_element):
        """Create the ps files containing the feynman diagrams for the born process,
        as well as for all the real emission processes"""

        super(MadSTRExporter, self).draw_feynman_diagrams(matrix_element)

        # now we have to draw those for the os subtractions terms
        model = matrix_element.born_matrix_element.get('processes')[0].get('model')
        for n, fksreal in enumerate(matrix_element.real_processes):
            for nos, os_me in enumerate(fksreal.os_matrix_elements):
                suffix = '%d_os_%d' % (n + 1, nos + 1)
                filename = 'matrix_%s.ps' % suffix
                plot = draw.MultiEpsDiagramDrawer(os_me.\
                                        get('base_amplitude').get('diagrams'),
                                        filename,
                                        model=model,
                                        amplitude=True, diagram_type='real')
                plot.draw()



    def write_real_matrix_elements(self, matrix_element, fortran_model):
        """writes the matrix_i.f files which contain the real matrix elements
        and the matrix_i_os_j.f which contain eventual on shell subtraction
        terms""" 
        self.os_ids = self.get_os_ids_from_me(matrix_element)

        for n, fksreal in enumerate(matrix_element.real_processes):
            filename = 'matrix_%d.f' % (n + 1)
            self.write_matrix_element_fks(writers.FortranWriter(filename),
                                          fksreal.matrix_element, n + 1, 
                                          fortran_model, 
                                          os_info = {'diags': fksreal.os_diagrams,
                                                     'ids': fksreal.os_ids,
                                                     'dau_pos': fksreal.os_daughter_pos})

            for nos, os_me in enumerate(fksreal.os_matrix_elements):
                suffix = '%d_os_%d' % (n + 1, nos + 1)
                filename = 'matrix_%s.f' % suffix
                self.write_matrix_element_fks(writers.FortranWriter(filename),
                                            os_me, suffix, fortran_model, 
                                            os_info = {'diags': [], 'ids': fksreal.os_ids, 'dau_pos': []})
                filename = 'wrapper_matrix_%s.f' % suffix
                self.write_os_wrapper(writers.FortranWriter(filename),
                        fksreal.matrix_element, os_me, suffix, fortran_model)


    def write_os_wrapper(self, writer, real_me, os_me, suffix, fortran_model):
        """write the wrapper for the on shell subtraction matrix-elements
        which takes care of reordering the momenta and of knowing which is the 
        mother particle"""
        replace_dict = {}
        replace_dict['suffix'] = suffix

        # find the permutation of the final state legs to map real_me onto os_me. 
        # look only at final state legs (initial state legs are not touched)
        real_ids = [l['id'] for l in real_me.get_base_amplitude()['process']['legs'] if l['state']]
        os_ids = os_me.get_base_amplitude()['process'].get_final_ids_after_decay()
        nexternal,ninitial = real_me.get_nexternal_ninitial()
        permutation = []
        #initial state legs are trivial
        for i in range(ninitial):
            permutation.append(i)
        for os_id in os_ids:
            permutation.append(ninitial + real_ids.index(os_id))
            # don't remove from the list, otherwise the position is
            # not correcly returned, just replace it by an 'x' 
            real_ids[real_ids.index(os_id)] = 'x'
        replace_dict['mom_perm'] = ', '.join([str(pp + 1) for pp in permutation])
        # find decay mother and daughter id's
        mother = [l['id'] \
                for l in os_me.get_base_amplitude()['process']['decay_chains'][0]['legs'] \
                if not l['state']]
        daughters = [l['id'] \
                for l in os_me.get_base_amplitude()['process']['decay_chains'][0]['legs'] \
                if l['state']]
        if not (len(mother) == 1 and len(daughters) == 2):
            raise fks_common.FKSProcessError(
                    'Incorrect number of mother(s) and daughters: %d, %d' % \
                            (len(mother), len(daughters)))
        
        model = os_me.get_base_amplitude()['process']['model']

        replace_dict['mom_external'] = {True: '.true.', False: '.false.'}[\
                    mother[0] in os_ids or \
                    model.get_particle(mother[0]).get_anti_pdg_code() in os_ids]

        # mother and daughter masses and widths
        replace_dict['mom_mass'] = model.get_particle(mother[0])['mass']
        replace_dict['mom_wdth'] = model.get_particle(mother[0])['width']
        replace_dict['dau1_mass'] = model.get_particle(daughters[0])['mass']
        replace_dict['dau2_mass'] = model.get_particle(daughters[1])['mass']
        # position of daughter in the array of momenta (the one of the decayed process)
        # count the ovvurrence of the daughters into the final state:
        counts = [0, 0]
        for idau, dau in enumerate(daughters):
            for idd in os_ids:
                if idd == dau:
                    counts[idau] +=1

        if counts == [1,1]:
            # if daughters are unique, find them in the os_ids list
            replace_dict['idau1'] = os_ids.index(daughters[0]) + ninitial + 1
            replace_dict['idau2'] = os_ids.index(daughters[1]) + ninitial + 1
        else:
            # otherwise, assign the position of the mother and the next one
            real_ids = [l['id'] for l in os_me.get_base_amplitude()['process']['legs'] if l['state']]
            replace_dict['idau1'] = real_ids.index(mother[0]) + ninitial + 1
            replace_dict['idau2'] = real_ids.index(mother[0]) + ninitial + 2

        # find the spectator (needed by the function which put momenta on-shell)
        # by default choose the first final state particle which is not a daughter
        for i in range(ninitial,nexternal):
            if i + 1 not in [replace_dict['idau1'], replace_dict['idau2']]:
                spectator = i + 1
                break
        replace_dict['ispect'] = spectator 
        replace_dict['spect_mass'] = model.get_particle(spectator)['mass']

        # finally write out the file
        file = open(os.path.join(self.template_path, 'os_wrapper_fks.inc')).read()
        file = file % replace_dict
        
        # Write the file
        writer.writelines(file)



    def write_real_me_wrapper(self, writer, matrix_element, fortran_model):
        """writes the wrapper which allows to chose among the different real matrix elements"""

        file = \
"""subroutine smatrix_real(p, wgt)
implicit none
include 'nexternal.inc'
double precision p(0:3, nexternal)
double precision wgt, wgt_re, wgt_os, wgt_os_this
double precision iden_comp
common /to_real_wgts/wgt_re, wgt_os
integer nfksprocess
common/c_nfksprocess/nfksprocess

wgt_re=0d0
wgt_os=0d0

"""
        # subtract here the on shell matrix-elements if any is there
        for n, info in enumerate(matrix_element.get_fks_info_list()):
            os_lines = ''
            iden_re = matrix_element.real_processes[info['n_me'] - 1].matrix_element.get('identical_particle_factor') 
            for i, os_me in \
              enumerate(matrix_element.real_processes[info['n_me'] - 1].os_matrix_elements):
                iden_os = os_me.get('identical_particle_factor') 
                os_lines += '\n iden_comp=dble(%d)/dble(%d)\ncall smatrix_%d_os_%d_wrapper(p, wgt_os_this)\n wgt_os = wgt_os + wgt_os_this*iden_comp' \
                        % (iden_os, iden_re, info['n_me'] , i + 1)

            file += \
"""if (nfksprocess.eq.%(n)d) then
call smatrix_%(n_me)d(p, wgt_re) %(os_lines)s
else""" % {'n': n + 1, 'n_me' : info['n_me'], 'os_lines': os_lines}

        if matrix_element.real_processes:
            file += \
"""
write(*,*) 'ERROR: invalid n in real_matrix :', nfksprocess
stop
endif
wgt = wgt_re - wgt_os
return
end
"""
        else:
            file += \
"""
wgt=0d0
return
end
"""
        # Write the file
        writer.writelines(file)
        return 0


    #===========================================================================
    # write_os_ids
    #===========================================================================
    def write_os_ids(self, writer, folder, os_ids):
        """Append the os_ids to the os_ids file"""

        # Write line to file
        content = ''
        if os_ids:
            content+= '%s: %s\n' % (folder, ' '.join(['%d' % v for v in os_ids])) 
        writer.write(content)

        return True


    def pass_information_from_cmd(self, cmd):
        """pass information from the command interface to the exporter.
           Please do not modify any object of the interface from the exporter.
        """
        return super(MadSTRExporter, self).pass_information_from_cmd(cmd)


    def update_fks_makefile(self, makefile):
        """add extra files related to OS to the standard aMC@NLO makefile
        """
        content = open(makefile).read()

        to_add = '$(patsubst %.f,%.o,$(wildcard wrapper_matrix_*.f)) transform_os.o '
        tag = '\nFILES= '
        content=content.replace(tag, tag + to_add)

        to_add = """\n# Files for testOS
TESTOS= $(FILES) test_OS_subtr.o BinothLHADummy.o cuts.o                \\
      pythia_unlops.o recluster.o

"""
        tag = """# Files for tests"""
        content=content.replace(tag, to_add+tag)

        to_add = """
test_OS_subtr: $(TESTOS)
	$(FC) $(LDFLAGS) -o test_OS_subtr $(TESTOS) $(APPLLIBS) $(LINKLIBS) $(FJLIBS)
	rm test_OS_subtr.o
	strip test_OS_subtr

"""
        tag = """\n
test_soft_col_limits: $(TEST)
	$(FC) $(LDFLAGS) -o test_soft_col_limits $(TEST) $(APPLLIBS) $(LINKLIBS) $(FJLIBS)
	rm test_soft_col_limits.o
	strip test_soft_col_limits
"""
        content=content.replace(tag, tag + to_add)

        out = open(makefile, 'w')
        out.write(content)
        out.close()


    def update_run_inc(self, runinc):
        """add extra files related to OS to the standard aMC@NLO makefile
        """
        content = open(runinc).read()
        to_add = \
"""
C for the OS subtraction
      logical str_include_pdf, str_include_flux
      integer istr
      common /to_os_reshuf/ str_include_pdf, str_include_flux, istr
"""
        content+= to_add
        out = open(runinc, 'w')
        out.write(content)
        out.close()


    #===============================================================================
    # write_matrix_element_fks
    #===============================================================================
    #test written
    def write_matrix_element_fks(self, writer, matrix_element, n, fortran_model, os_info={}):
        """Export a matrix element to a matrix.f file in MG4 madevent format"""
    
        if not matrix_element.get('processes') or \
               not matrix_element.get('diagrams'):
            return 0,0
    
        if not isinstance(writer, writers.FortranWriter):
            raise writers.FortranWriter.FortranWriterError(\
                "writer not FortranWriter")
        # Set lowercase/uppercase Fortran code
        writers.FortranWriter.downcase = False
    
        replace_dict = {}
        replace_dict['N_me'] = str(n)
    
        # Extract version number and date from VERSION file
        info_lines = self.get_mg5_info_lines()
        replace_dict['info_lines'] = info_lines
    
        # Extract process info lines
        process_lines = self.get_process_info_lines(matrix_element)
        replace_dict['process_lines'] = process_lines
    
        # Extract ncomb
        ncomb = matrix_element.get_helicity_combinations()
        replace_dict['ncomb'] = ncomb
    
        # Extract helicity lines
        helicity_lines = self.get_helicity_lines(matrix_element)
        replace_dict['helicity_lines'] = helicity_lines
    
        # Extract IC line
        ic_line = self.get_ic_line(matrix_element)
        replace_dict['ic_line'] = ic_line
    
        # Extract overall denominator
        # Averaging initial state color, spin, and identical FS particles
        den_factor_line = self.get_den_factor_line(matrix_element)
        replace_dict['den_factor_line'] = den_factor_line
    
        # Extract ngraphs
        ngraphs = matrix_element.get_number_of_amplitudes()
        replace_dict['ngraphs'] = ngraphs
    
        # Extract ncolor
        if not matrix_element.get('color_basis'):
            matrix_element.process_color()
        ncolor = max(1, len(matrix_element.get('color_basis')))
        replace_dict['ncolor'] = ncolor
    
        # Extract color data lines
        color_data_lines = self.get_color_data_lines(matrix_element)
        replace_dict['color_data_lines'] = "\n".join(color_data_lines)
    
        # Extract helas calls
        helas_calls = fortran_model.get_matrix_element_calls(\
                    matrix_element)
        replace_dict['helas_calls'] = "\n".join(helas_calls)

        # if there are os_diagrams, these should be set to zero in the ME if
        # diagram removal without interference is done
        if os_info and os_info['diags']:
            # this is for the resonant diagrams in the full real emission ME
            os_diagrams = os_info['diags']
            os_ids = os_info['ids']
            os_dau_pos = os_info['dau_pos']
            # add a copy of the helas calls, in order to store diagrams with
            # non-zero width
            replace_dict['helas_calls'] += \
                self.change_width_in_os_diagrams(matrix_element, replace_dict['helas_calls'], \
                                                 os_diagrams, os_ids, os_dau_pos)
            replace_dict['helas_calls'] += '\n' + \
                    self.get_os_diagrams_lines(matrix_element, os_diagrams, os_ids)

        elif os_info and os_info['ids'] and not os_info['diags']:
            os_ids = os_info['ids']
            # this is for the OS subtraction counterterms, 
            # in this case replace all occurrences of the particle width
            replace_dict['helas_calls'] = \
                self.change_width_in_os_diagrams(matrix_element, replace_dict['helas_calls'], [], os_ids, [])
    
        # Extract nwavefuncs (important to place after get_matrix_element_calls
        # so that 'me_id' is set)
        nwavefuncs = matrix_element.get_number_of_wavefunctions()
        replace_dict['nwavefuncs'] = nwavefuncs
    
        # Extract amp2 lines
        amp2_lines = self.get_amp2_lines(matrix_element)
        replace_dict['amp2_lines'] = '\n'.join(amp2_lines)

        # Set the size of Wavefunction
        if not self.model or any([p.get('spin') in [4,5] for p in self.model.get('particles') if p]):
            replace_dict['wavefunctionsize'] = 20
        else:
            replace_dict['wavefunctionsize'] = 8
    
        # Extract JAMP lines
        # MZ this is version dependent as the function
        # has changed in v 2.9
        
        version = misc.get_pkg_info()['version'].split('.')
        if int(version[0]) == 2 and int(version[1]) < 9:
            jamp_lines = self.get_JAMP_lines(matrix_element)
            nb_tmp_jamp = 1
        elif int(version[0]) == 2 and int(version[1]) >= 9: 
            jamp_lines, nb_tmp_jamp = self.get_JAMP_lines(matrix_element)
        else:
            raise MadSTRExporterError("Wrong version: %s" % '.'.join(version))
    
        replace_dict['jamp_lines'] = '\n'.join(jamp_lines)
        replace_dict['nb_temp_jamp'] = nb_tmp_jamp
    
        realfile = open(os.path.join(self.template_path, 'realmatrix_madstr.inc')).read()

        realfile = realfile % replace_dict
        
        # Write the file
        writer.writelines(realfile)
    
        return len([call for call in helas_calls if call.find('#') != 0]), ncolor


    def get_os_diagrams_lines(self, matrix_element, os_diagrams, os_ids):
        """ add the lines which set the diagrams used with
        diagram-removal techniques to zero or to the value including
        the particle width 
        """
        particle_dict = self.model.get('particle_dict') 

        text = ''
        for diags, ids in zip(os_diagrams, os_ids): 
            text += 'if (%s.gt.(%s+%s)) then\nif (istr.eq.1) then\n' % \
                    tuple([particle_dict[idd].get('mass') for idd in ids])
            for diag in diags:
                for amp in matrix_element['diagrams'][diag]['amplitudes']:
                    text+= 'amp(%d) = dcmplx(0d0,0d0)\n' % amp['number']
            text += 'else\n'
            for diag in diags:
                for amp in matrix_element['diagrams'][diag]['amplitudes']:
                    text+= 'amp(%d) = amp_keep_width(%d)\n' % (amp['number'], amp['number'])
            text += 'endif\n'
            text += 'endif\n'

        return text


    def change_width_in_os_diagrams(self, me, helas_calls, os_diagrams, os_ids, os_dau_pos):
        """change the name of the width used in diagrams with internal resonances, so
        that the width in those diagrams is not set to zero
        """
        
        new_helas_calls = "\nC the following helas calls are like the ones above, but they keep a finite width for the on-shell resonances\n" +\
                        copy.copy(helas_calls)

        if os_diagrams:
            for diags, ids, dau_pos in zip(os_diagrams, os_ids, os_dau_pos):
                part_width = self.model.get('particle_dict')[ids[0]].get('width')
                if part_width + '_keep' not in new_helas_calls:
                    new_helas_calls = new_helas_calls.replace(part_width, part_width + '_keep')
            new_helas_calls = new_helas_calls.upper().replace('AMP(', 'AMP_KEEP_WIDTH(')
        else:
            for ids in os_ids:
                part_width = self.model.get('particle_dict')[ids[0]].get('width')
                if part_width + '_keep' not in new_helas_calls:
                    # Here we replace the width everywhere, as no
                    # IR singularities are there in the resonant ME
                    new_helas_calls = new_helas_calls.replace(part_width, part_width + '_keep')

        return new_helas_calls


    #===========================================================================
    #  get_wfno_for_ext_particles 
    #===========================================================================
    def get_wfs_for_ext_particles(self, helas_calls, nexternal):
        """for each external particle, returns the list of the
        wavefunctions associated to it. Non-trivial for majoranas
        """
        #start with an empty list
        wf_list = [[]] * nexternal 

        for i in range(nexternal):
            pattern=re.compile(r'''\(P\(0,%d\)[^\r\n]*(?P<WF>W\(1,\d+\))''' % (i+1))
            wf_list[i] = [item.group('WF') for item in pattern.finditer(helas_calls)]
        return wf_list

    #===========================================================================
    #  create the run_card 
    #===========================================================================
    def create_run_card(self, processes, history):
        """create the run_card for MadSTR, including the extra variables needed
        to control the OS subtraction also in banner.py"""
 
        run_card = banner_mod.RunCardNLO()
        
        run_card.create_default_for_process(self.proc_characteristic, 
                                            history,
                                            processes)
        
        run_card.write(pjoin(self.dir_path, 'Cards', 'run_card_default.dat'))

        # now edit the run_card with the extra variables
        os_text = \
"""#***********************************************************************
# Parameters relevant for the MasSTR plugin:                           *
# iSTR controls the strategy for the resonance treatment               *
#  istr = 1 -> DR without interferece                                  *
#  istr = 2 -> DR with interferece                                     *
#  istr = 3 -> DS with reshuffling on initial state, standard BW       *
#  istr = 4 -> DS with reshuffling on initial state, running BW        *
#  istr = 5 -> DS with reshuffling on all FS particles, standard BW    *
#  istr = 6 -> DS with reshuffling on all FS particles, running BW     *
#***********************************************************************
  2 = istr ! strategy to be used to remove resonances 
                         ! appearing in real emissions
 True = str_include_pdf ! compensate for PDFs when doing reshuffling
 True = str_include_flux ! compensate for flux when doing reshuffling"""

        run_card_lines = open(pjoin(self.dir_path, 'Cards', 'run_card_default.dat')).read().split('\n')
        # look for the line which contains 'store_rwgt_info', after which we will insert
        # the OS block
        for isplit, line in enumerate(run_card_lines):
            if 'store_rwgt_info' in line: break
        new_run_card_lines = run_card_lines[:isplit+1] + os_text.split('\n') + run_card_lines[isplit+1:]
        new_run_card = open(pjoin(self.dir_path, 'Cards', 'run_card_default.dat'), 'w')
        new_run_card.write('\n'.join(new_run_card_lines))
        new_run_card.close()

        # finally copy the run_card_default to run_card
        files.cp(pjoin(self.dir_path, 'Cards', 'run_card_default.dat'), \
                 pjoin(self.dir_path, 'Cards', 'run_card.dat'))

        # We also need to update banner.py
        banner_text = \
"""        self.add_param('istr', 2)
        self.add_param('str_include_pdf', True)
        self.add_param('str_include_flux', True)"""

        banner_lines = open(pjoin(self.dir_path, 'bin', 'internal', 'banner.py')).read().split('\n')
        for isplit, line in enumerate(banner_lines):
            if 'store_rwgt_info' in line: break
        new_banner_lines = banner_lines[:isplit+1] + banner_text.split('\n') + banner_lines[isplit+1:]
        new_banner = open(pjoin(self.dir_path, 'bin', 'internal', 'banner.py'), 'w')
        new_banner.write('\n'.join(new_banner_lines))
        new_banner.close()


    def finalize(self, matrix_elements, history, mg5options, flaglist):
        """call the mother class, and do a couple of other things relevant 
        to OS subtraction
        """
        super(MadSTRExporter, self).finalize(matrix_elements, history, mg5options, flaglist)

        os_ids = self.get_os_ids_from_file(pjoin(self.dir_path, 'SubProcesses', 'os_ids.mg'))

        # add the widths corresponding to the os_ids to coupl.inc
        particle_dict = self.model.get('particle_dict') 
        # list(set( is needed for particle/antiparticle with the same width
        keep_widths = list(set([particle_dict[idd].get('width') for idd in os_ids]))
        self.update_couplinc(keep_widths,pjoin(self.dir_path, 'Source', 'coupl.inc'))

        # also, add a function that returns these widths to MODEL 
        filename=pjoin(self.dir_path, 'Source', 'MODEL', 'get_mass_width_fcts.f')
        width_particles = [particle_dict[idd] for idd in os_ids]
        self.update_get_mass_width(width_particles, filename)

        # replace the common_run_interface with the one from madstr_plugin
        internal = pjoin(self.dir_path, 'bin', 'internal')
        files.mv(pjoin(internal, 'common_run_interface.py'), \
                 pjoin(internal, 'common_run_interface_MG.py'))
        files.cp(pjoin(plugin_path, 'common_run_interface.py'), internal)

        # finally patch fks_singular so that it won't complain about negative 
        # weights for the real emission
        subprocess.call('patch -p3 < %s' % pjoin(self.template_path, 'fks_singular_patch.txt'), cwd=self.dir_path, shell=True)


    def update_get_mass_width(self, width_particles, filename):
        """update the get_mass_width to return also the vaule
        of the _keep widhts"""

        done_particles = []

        iflines_width = ''
        for i, part in enumerate(width_particles):

            if part in done_particles: continue
            done_particles.append(part)

            if i == 0:
                ifstring = 'if'
            else:
                ifstring = 'else if'
            if part['self_antipart']:
                iflines_width += '%s (id.eq.%d) then\n' % \
                        (ifstring, part.get_pdg_code())
            else:
                iflines_width += '%s (id.eq.%d.or.id.eq.%d) then\n' % \
                        (ifstring, part.get_pdg_code(), part.get_anti_pdg_code())
            iflines_width += 'get_width_os_from_id=abs(%s_keep)\n' % part.get('width')

        if len(width_particles)==0:
            iflines_width = 'if (.True.) then\n'

        outfile = writers.FortranWriter(filename, 'a')

        text = """
        DOUBLE PRECISION FUNCTION GET_WIDTH_OS_FROM_ID(ID)
IMPLICIT NONE
INTEGER ID
INCLUDE 'coupl.inc'

%sELSE
GET_WIDTH_OS_FROM_ID=0d0
ENDIF
RETURN
END
"""     % iflines_width
        
        outfile.writelines(text)
        outfile.close()

        return


    def update_couplinc(self, widths, couplinc):
        """update coupl.inc by adding extra lines for the widths to keep"""

        if not widths:
            # so that the code will compile anyway
            width_list = 'MDL_WDUMMY_KEEP'
        else:
            width_list = ','.join(['%s_keep' % w for w in widths])
        lines = '\n      double precision %s\n      common /keep_widths/%s\n' %(width_list, width_list)
        #outfile = open(couplincpjoin(self.dir_path, 'Source', 'coupl.inc'), 'a')
        outfile = writers.FortranWriter(couplinc, 'a')
        outfile.writelines(lines)
        outfile.close()

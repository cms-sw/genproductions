###################################################
#                                                   #
#  Source file of the interface for the             #
#  MadSTR plugin of MG5aMC.                          #
#                                                   #
#####################################################

import os
import subprocess
import logging
import itertools
import sys
import random
import re
import time
import shutil
import signal
import multiprocessing
import six.moves.cPickle

from madgraph import MadGraph5Error, InvalidCmd, MG5DIR
import madgraph.various.progressbar as pbar
import madgraph.interface.extended_cmd as extended_cmd 
import madgraph.interface.madgraph_interface as madgraph_interface
import madgraph.interface.master_interface as master_interface
import madgraph.interface.loop_interface as loop_interface
import madgraph.various.misc as misc
import madgraph.iolibs.files as files
import madgraph.various.lhe_parser as lhe_parser
from madgraph.interface.loop_interface import CommonLoopInterface
import madgraph.interface.amcatnlo_run_interface as amcatnlo_run
import madgraph.iolibs.export_v4 as export_v4
import madgraph.iolibs.helas_call_writers as helas_call_writers
import madgraph.core.helas_objects as helas_objects

import MadSTR.madstr_fks as madstr_fks
import MadSTR.madstr_exporter as madstr_exporter
import madgraph.fks.fks_helas_objects as fks_helas


plugin_path = os.path.dirname(os.path.realpath( __file__ ))

logger = logging.getLogger('MadSTR_plugin.Interface')

pjoin = os.path.join


def generate_directories_fks_async(i):
    """generates directories in a multi-core way"""
        
    arglist = glob_directories_map[i]
    
    curr_exporter = arglist[0]
    mefile = arglist[1]
    curr_fortran_model = arglist[2]
    ime = arglist[3]
    nme = arglist[4]
    path = arglist[5]
    olpopts = arglist[6]
    
    infile = open(mefile,'rb')
    me = six.moves.cPickle.load(infile)
    infile.close()      

    # here we need to find the OS configuration from the matrix-elements
    os_couplings = []
    os_lorentz = [] 
    for real_me in me.real_processes:
        madstr_fks.find_os_divergences(real_me)
        real_me.os_matrix_elements = [\
            helas_objects.HelasDecayChainProcess(os_amp).combine_decay_chain_processes()[0]
            for os_amp in real_me.os_amplitudes]

        os_couplings.extend(sum([c for osme in real_me.os_matrix_elements for c in osme.get_used_couplings()], []))
        os_lorentz.extend(sum([osme.get_used_lorentz() for osme in real_me.os_matrix_elements], []))

    calls = curr_exporter.generate_directories_fks(me, curr_fortran_model, ime, nme, path, olpopts)
    nexternal = curr_exporter.proc_characteristic['nexternal']
    ninitial = curr_exporter.proc_characteristic['ninitial']
    processes = me.born_matrix_element.get('processes')
    
    #only available after export has been done, so has to be returned from here
    max_loop_vertex_rank = -99
    if me.virt_matrix_element:
        max_loop_vertex_rank = me.virt_matrix_element.get_max_loop_vertex_rank()  
    
    return [calls, curr_exporter.fksdirs, max_loop_vertex_rank, ninitial, nexternal, processes, os_couplings, os_lorentz]



class MadSTRInterfaceError(MadGraph5Error):
    """ Error from the resummation interface. """

class MadSTRInvalidCmd(InvalidCmd):
    """ Invalid command issued to the resummation plugin. """


class MadSTRInterface(master_interface.MasterCmd):
    """ Interface for steering the Resummation tasks. """

    # Change the prompt 
    def preloop(self, *args, **opts):
        """only change the prompt after calling  the mother preloop command"""
        super(MadSTRInterface, self).preloop(*args,**opts)
        # The colored prompt screws up the terminal for some reason.
        #self.prompt = '\033[92mPY8Kernels > \033[0m'
        self.prompt = 'MadSTR > '


    def do_launch(self, line, *args, **opts):
        """Warn the user not to use launch from the MG5_aMC interface, 
        but rather to do the launch from within the output folder
        """
        raise MadSTRInterfaceError(\
                "\nWith MadSTR, the launch command must not be executed from the MG5_aMC shell.\n" + \
                "Rather, the event generation / cross-section computation should be\n" + \
                "launched from within the process directory.")


    def do_add(self, line, *args, **opts):
        """does the usual add command, then, if the output mode is NLO
        on-shell singularities are looked for
        """
        super(MadSTRInterface, self).do_add(line, *args, **opts)
        # check that a NLO generation has been done (if not, just return)
        if not hasattr(self, '_fks_multi_proc') or not self._fks_multi_proc:
            logger.warning('No NLO Process has been generated.\n To use MadSTR, please generate a process with [QCD]')
            return

        if self.options['low_mem_multicore_nlo_generation']: 
            self.n_os = -1
            return

        # now one needs to check for OS resonances
        logger.info('Looking for on-shell singularities in the real emissions...')
        self.n_os = 0
        for born in self._fks_multi_proc['born_processes']:
            for real in born.real_amps:
                self.n_os += madstr_fks.find_os_divergences(real)
        logger.info('Found %d on-shell contributions' % self.n_os)


    def do_output(self, line):
        """output command: if no os divergences are there or if LO run has
        been generated nothing has to be done.
        Otherwise, the diagrams for the on-shell resonances 
        need to be exported too
        """
        if not hasattr(self, '_fks_multi_proc') or not self._fks_multi_proc:
            #MZMZ in these cases we should also switch the interface
            # or we just do no output
            super(MadSTRInterface, self).do_output(line)
            return
        elif self.n_os == 0:
            super(MadSTRInterface, self).do_output(line)
            return

        args = self.split_arg(line)
        # Check Argument validity
        self.check_output(args)
        
        noclean = '-noclean' in args
        force = '-f' in args 
        nojpeg = '-nojpeg' in args
        main_file_name = ""
        try:
            main_file_name = args[args.index('-name') + 1]
        except Exception:
            pass

        # For NLO, the group_subprocesses is automatically set to false
        group_processes = False

        # MZ these are copied from the V4Factory...
        # It may not be the most efficient way
        # ==========================================================================
        # First treat the MadLoop5 standalone case       
        MadLoop_SA_options = {'clean': not noclean, 
          'complex_mass':self.options['complex_mass_scheme'],
          'export_format':'madloop', 
          'mp':True,
          'loop_dir': os.path.join(self._mgme_dir,'Template','loop_material'),
          'cuttools_dir': self._cuttools_dir,
          'iregi_dir':self._iregi_dir,
          'golem_dir':self.options['golem'],
          'samurai_dir':self.options['samurai'],
          'ninja_dir':self.options['ninja'],
          'collier_dir':self.options['collier'],
          'fortran_compiler':self.options['fortran_compiler'],
          'f2py_compiler':self.options['f2py_compiler'],
          'output_dependencies':self.options['output_dependencies'],
          'SubProc_prefix':'P',
          'compute_color_flows':self.options['loop_color_flows'],
          'mode': 'reweight' if self._export_format == "standalone_rw" else '',
          'cluster_local_path': self.options['cluster_local_path'],
          'output_options': {'group_subprocesses': False}
          }

        try:
            # pjfry is not supported any more from v 2.7.2
            # so some special care is needed
            MadLoop_SA_options['pjfry_dir'] = self.options['pjfry']
        except KeyError:
            pass

        # initialize the writer
        if self._export_format in ['NLO']:
            to_pass = dict(MadLoop_SA_options)
            to_pass['mp'] = len(self._fks_multi_proc.get_virt_amplitudes()) > 0
            to_pass['export_format'] = 'FKS5_optimized'
            self._curr_exporter = madstr_exporter.MadSTRExporter(self._export_dir, to_pass)
            
            self._curr_exporter.pass_information_from_cmd(self)

        # check if a dir with the same name already exists
        if not force and not noclean and os.path.isdir(self._export_dir)\
               and self._export_format in ['NLO']:
            # Don't ask if user already specified force or noclean
            logger.info('INFO: directory %s already exists.' % self._export_dir)
            logger.info('If you continue this directory will be deleted and replaced.')
            answer = self.ask('Do you want to continue?', 'y', ['y','n'], 
                                                timeout=self.options['timeout'])
            if answer != 'y':
                raise self.InvalidCmd('Stopped by user request')

        # if one gets here either used -f or answered yes to the question about
        # removing the dir
        if os.path.exists(self._export_dir):
            shutil.rmtree(self._export_dir)

        # Make a Template Copy
        if self._export_format in ['NLO']:
            self._curr_exporter.copy_fkstemplate()

        # Reset _done_export, since we have new directory
        self._done_export = False

        # Perform export and finalize right away
        self.export(nojpeg, main_file_name, group_processes=group_processes)

        # Pass potential new information generated during the export.
        self._curr_exporter.pass_information_from_cmd(self)

        # Automatically run finalize
        self.finalize(nojpeg)
            
        # Generate the virtuals if from OLP
        if self.options['OLP']!='MadLoop':
            self._curr_exporter.generate_virtuals_from_OLP(
              self.born_processes_for_olp,self._export_dir,self.options['OLP'])
                
        # Remember that we have done export
        self._done_export = (self._export_dir, self._export_format)

        # Reset _export_dir, so we don't overwrite by mistake later
        self._export_dir = None



    # Export a matrix element  
    def export(self, nojpeg = False, main_file_name = "", group_processes=False, args=[]):
        """Export a generated amplitude to file"""

        self._curr_helas_model = helas_call_writers.FortranUFOHelasCallWriter(self._curr_model)
        def generate_matrix_elements(self, group=False):
            """Helper function to generate the matrix elements before
            exporting"""

            # Sort amplitudes according to number of diagrams,
            # to get most efficient multichannel output
            self._curr_amps.sort(key = lambda a: a.get_number_of_diagrams(), reverse=True)

            cpu_time1 = time.time()
            ndiags = 0
            if not self._curr_matrix_elements.get_matrix_elements():
                if group:
                    raise MadGraph5Error("Cannot group subprocesses when "+\
                                                              "exporting to NLO")
                else:
                    if not self.options['low_mem_multicore_nlo_generation']: 
                        # generate the code the old way
                        self._curr_matrix_elements = \
                                 madstr_fks.FKSHelasMultiProcessWithOS(\
                                    self._fks_multi_proc, 
                                    loop_optimized= self.options['loop_optimized_output'])
                    
                        ndiags = sum([len(me.get('diagrams')) for \
                                      me in self._curr_matrix_elements.\
                                      get_matrix_elements()])
                        # assign a unique id number to all process and
                        # generate a list of possible PDF combinations
                        uid = 0 
                        initial_states=[]
                        for me in self._curr_matrix_elements.get_matrix_elements():
                            uid += 1 # update the identification number
                            me.get('processes')[0].set('uid', uid)
                            try:
                                initial_states.append(sorted(list(set((p.get_initial_pdg(1),p.get_initial_pdg(2)) for \
                                                                      p in me.born_matrix_element.get('processes')))))
                            except IndexError:
                                initial_states.append(sorted(list(set((p.get_initial_pdg(1)) for \
                                                                      p in me.born_matrix_element.get('processes')))))
                        
                            for fksreal in me.real_processes:
                            # Pick out all initial state particles for the two beams
                                try:
                                    initial_states.append(sorted(list(set((p.get_initial_pdg(1),p.get_initial_pdg(2)) for \
                                                                 p in fksreal.matrix_element.get('processes')))))
                                except IndexError:
                                    initial_states.append(sorted(list(set((p.get_initial_pdg(1)) for \
                                                                 p in fksreal.matrix_element.get('processes')))))
                                    
                            
                        # remove doubles from the list
                        checked = []
                        for e in initial_states:
                            if e not in checked:
                                checked.append(e)
                        initial_states=checked

                        self._curr_matrix_elements.set('initial_states',initial_states)

                    else:
                        #new NLO generation
                        self._curr_matrix_elements = \
                                 fks_helas.FKSHelasMultiProcess(\
                                    self._fks_multi_proc, 
                                    loop_optimized= self.options['loop_optimized_output'])
                        if self._curr_matrix_elements['has_loops']:
                            self._curr_exporter.opt['mp'] = True
                        self._curr_exporter.model = self._curr_model
                        ndiags = 0

            cpu_time2 = time.time()
            return ndiags, cpu_time2 - cpu_time1

        # Start of the actual routine

        ndiags, cpu_time = generate_matrix_elements(self, group=group_processes)
        calls = 0

        path = self._export_dir

        if self._export_format in ['NLO']:
            path = os.path.join(path, 'SubProcesses')

            #_curr_matrix_element is a FKSHelasMultiProcess Object 
            self._fks_directories = []
            proc_charac = self._curr_exporter.proc_characteristic 
            for charac in ['has_isr', 'has_fsr', 'has_loops']:
                proc_charac[charac] = self._curr_matrix_elements[charac]

            # prepare for the generation
            # glob_directories_map is for the new NLO generation
            global glob_directories_map
            glob_directories_map = []

            # Save processes instances generated
            self.born_processes_for_olp = []
            self.born_processes = []
            for ime, me in \
                enumerate(self._curr_matrix_elements.get('matrix_elements')):
                if not self.options['low_mem_multicore_nlo_generation']:
                    #me is a FKSHelasProcessFromReals
                    calls = calls + \
                            self._curr_exporter.generate_directories_fks(me, 
                            self._curr_helas_model, 
                            ime, len(self._curr_matrix_elements.get('matrix_elements')), 
                            path,self.options['OLP'])
                    self._fks_directories.extend(self._curr_exporter.fksdirs)
                    self.born_processes_for_olp.append(me.born_matrix_element.get('processes')[0])
                    self.born_processes.append(me.born_matrix_element.get('processes'))
                else:
                    glob_directories_map.append(\
                            [self._curr_exporter, me, self._curr_helas_model, 
                             ime, len(self._curr_matrix_elements.get('matrix_elements')), 
                             path, self.options['OLP']])

            if self.options['low_mem_multicore_nlo_generation']:
                # start the pool instance with a signal instance to catch ctr+c
                logger.info('Writing directories...')
                original_sigint_handler = signal.signal(signal.SIGINT, signal.SIG_IGN)
                if self.ncores_for_proc_gen < 0: # use all cores
                    pool = multiprocessing.Pool(maxtasksperchild=1)
                else:
                    pool = multiprocessing.Pool(processes=self.ncores_for_proc_gen,maxtasksperchild=1)
                signal.signal(signal.SIGINT, original_sigint_handler)
                try:
                    # the very large timeout passed to get is to be able to catch
                    # KeyboardInterrupts
                    diroutputmap = pool.map_async(generate_directories_fks_async,
                                                  range(len(glob_directories_map))).get(9999999)
                except KeyboardInterrupt:
                    pool.terminate()
                    raise KeyboardInterrupt 
    
                pool.close()
                pool.join()
                
                #clean up tmp files containing final matrix elements
                for mefile in self._curr_matrix_elements.get('matrix_elements'):
                    os.remove(mefile)

                for charac in ['nexternal', 'ninitial']:
                    proc_charac[charac] = self._curr_exporter.proc_characteristic[charac]
                # ninitial and nexternal
                proc_charac['nexternal'] = max([diroutput[4] for diroutput in diroutputmap])
                ninitial_set = set([diroutput[3] for diroutput in diroutputmap])
                if len(ninitial_set) != 1:
                    raise MadGraph5Error("Invalid ninitial values: %s" % ' ,'.join(list(ninitial_set)))    
                proc_charac['ninitial'] = list(ninitial_set)[0]

                self.born_processes = []
                self.born_processes_for_olp = []
                max_loop_vertex_ranks = []
                os_couplings = []
                os_lorentz = []
                
                for diroutput in diroutputmap:
                    calls = calls + diroutput[0]
                    self._fks_directories.extend(diroutput[1])
                    max_loop_vertex_ranks.append(diroutput[2])
                    self.born_processes.extend(diroutput[5])
                    self.born_processes_for_olp.append(diroutput[5][0])
                    os_couplings.extend(diroutput[6])
                    os_lorentz.extend(diroutput[7])

                # we need to update the couplings/lorentz of the ME
                # in order to take into account also the OS matrix elements
                os_couplings = list(set(os_couplings))
                os_lorentz = list(set(os_lorentz))
                self._curr_matrix_elements.get_used_couplings()
                self._curr_matrix_elements['used_couplings'].extend(os_couplings)
                self._curr_matrix_elements.get_used_lorentz()
                self._curr_matrix_elements['used_lorentz'].extend(os_lorentz)


            else:
                max_loop_vertex_ranks = [me.get_max_loop_vertex_rank() for \
                                         me in self._curr_matrix_elements.get_virt_matrix_elements()]

            card_path = os.path.join(path, os.path.pardir, 'SubProcesses', \
                                     'procdef_mg5.dat')
            
            if self.options['loop_optimized_output'] and \
               len(max_loop_vertex_ranks) > 0:
                self._curr_exporter.write_coef_specs_file(max_loop_vertex_ranks)
            if self._generate_info:
                self._curr_exporter.write_procdef_mg5(card_path, #
                                self._curr_model['name'],
                                self._generate_info)
                try:
                    cmd.Cmd.onecmd(self, 'history .')
                except Exception:
                    logger.debug('fail to run command \"history cmd\"')
                    pass
            subproc_path = os.path.join(path, os.path.pardir, 'SubProcesses', \
                                     'initial_states_map.dat')
            self._curr_exporter.write_init_map(subproc_path,
                                self._curr_matrix_elements.get('initial_states'))
            
        cpu_time1 = time.time()

        

class ResummationRunInterface(amcatnlo_run.aMCatNLOCmdShell):
    """ Specialization of the aMC@NLO run interface. In practice, needed to 
    steer the NLO and aMC@LO run and to monitor the resummation jobs."""
    cluster_jobs_identifier_specifier = 'production_me'


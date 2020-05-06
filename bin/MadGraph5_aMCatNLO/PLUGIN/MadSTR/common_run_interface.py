####################################################
#                                                   #
#  A wrapper to the usual common_run_interface      #
#  with few functions changed                       #
#                                                   #
#####################################################

import copy
import os 
import logging
pjoin = os.path.join

import internal.common_run_interface_MG as common_run_interface
import internal.check_param_card as param_card_mod

logger = common_run_interface.logger

#===============================================================================
# CommonRunCmd
#===============================================================================
class CommonRunCmd(common_run_interface.CommonRunCmd):

    def do_treatcards(self, line, amcatnlo=False):
        """ call the mother, then, if the param_card has to be updated, write it again
        """
        super(CommonRunCmd, self).do_treatcards(line, amcatnlo)

        keepwidth = False
        if '--keepwidth' in line:
            keepwidth = True
            line = line.replace('--keepwidth', '')
        args = self.split_arg(line)
        mode,  opt  = self.check_treatcards(args)

        if amcatnlo and mode in ['all', 'param'] and not keepwidth:

            if os.path.exists(pjoin(self.me_dir, 'Source', 'MODEL', 'mp_coupl.inc')):
                param_card = param_card_mod.ParamCardMP(opt['param_card'])
            else:
                param_card = param_card_mod.ParamCard(opt['param_card'])

            os_pids = self.get_os_pids()
            if not os_pids: return # nothing else to do here

            # append to the param_card.inc  lines with 
            #'MDL_WX_KEEP = VALUE' for each particle that can go onshell
            import ufomodel as ufomodel
            zero = ufomodel.parameters.ZERO

            parts_keep = [p for p in ufomodel.all_particles if p.pdg_code in os_pids or -p.pdg_code in os_pids]
            decay_to_keep = [(part.get('width'), copy.copy(param_card['decay'].get((abs(part.pdg_code),)))) for part in parts_keep]
            # Force these widths to be set to zero in param_card.inc
            # to ensure gauge / pole cancelation
            param_inc = pjoin(self.me_dir, 'Source', 'param_card.inc')

            self.replace_widths_in_paramcard_inc(decay_to_keep, param_inc)

            # finally append the width with _keep, which will enter only
            # the resonant diagrams and the OS counterterms
            incfile = open(param_inc, 'a')
            for width, param in decay_to_keep:
                incfile.write('      mdl_%s_keep = %s\n' % (width, ('%e'%float(param.value)).replace('e','d')))
            incfile.close()



    ############################################################################
    def replace_widths_in_paramcard_inc(self, decay_to_keep, param_inc):
        """replace the widths passed in decay_to_keep inside param_inc and
        force them to be zero"""
        widths = [('MDL_%s' % d[0]).upper() for d in decay_to_keep]
        lines = open(param_inc).read().split('\n')

        replaced = False

        for l in lines:
            if not l: continue
            param = l.split('=')[0].strip()
            if param in widths: # widths with standard precision
                replaced = True
                lines[lines.index(l)]=l.replace('=', '= 0D0 !! MadSTR Forced !! ')
                logger.info('MadSTR: Forcing width %s to zero inside param_card.inc' % param) 
            if param.startswith('MP__') and param[4:] in widths: # widths in quadruple precision 
                lines[lines.index(l)]=l.replace('=', '= 0E+00_16 !! MadSTR Forced !! ')
                logger.info('MadSTR: Forcing width %s to zero inside param_card.inc' % param) 

        if replaced:
            outfile = open(param_inc, 'w')
            outfile.write('\n'.join(lines))
            outfile.close()
            logger.warning('The replacements above ensure poles cancelation, and affect all widths\n' + 
                        '   EXCEPT those which enter the resonance-treatment counterterms, which\n' +
                        '   are taken from the param_card.\n' +
                        '   Do NOT set these widths to zero in the param_card.')





    ############################################################################
    def get_os_pids(self):
        """Find the pid of all particles in the intermediate on-sheel partices"""
        pids = set()
        try:
            os_ids_lines = [l.strip() for l \
                in open(pjoin(self.me_dir,'SubProcesses', 'os_ids.mg')) if l]
        except IOError:
            return pids

        for l in os_ids_lines:
            ids = [int(i) for i in l.split()[1:]]
            pids.update(set(ids))

        return pids

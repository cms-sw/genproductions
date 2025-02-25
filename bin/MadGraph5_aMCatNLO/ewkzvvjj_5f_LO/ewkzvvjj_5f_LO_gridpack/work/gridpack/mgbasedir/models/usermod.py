################################################################################
#
# Copyright (c) 2011 The MadGraph Development team and Contributors
#
# This file is a part of the MadGraph 5 project, an application which 
# automatically generates Feynman diagrams and matrix elements for arbitrary
# high-energy processes in the Standard Model and beyond.
#
# It is subject to the MadGraph license which should accompany this 
# distribution.
#
# For more information, please visit: http://madgraph.phys.ucl.ac.be
#
################################################################################
""" Set of Tool in order to modify a given UFO model.
    (mainly by adding-suppressing interactions and allow to modify by text the 
    different part of the model. Check of consistency of the model are performed.
    This produce a new valid UFO model in output.
"""
from __future__ import absolute_import
import copy
import glob
import logging
import os
import re
import sys

import madgraph.core.base_objects as base_objects
import madgraph.iolibs.files as files
import madgraph.various.misc as misc
import models as ufomodels
import models.import_ufo as import_ufo
import models.check_param_card as check_param_card
from madgraph import MG5DIR
import six
from six.moves import range

pjoin =os.path.join
logger = logging.getLogger('madgraph.model')

class USRMODERROR(Exception): pass


def repr(obj):
    
    text = obj.__repr__()
    if text.startswith('_'):
        text =  '%s%s' % (str(obj.__class__.__name__)[0].upper(), text)
    return text

class UFOModel(object):
    """ The class storing the current status of the model """
    
    def __init__(self, modelpath, addon='__1'):
        """load the model from a valid UFO directory (otherwise keep everything
        as empty."""
        self.modelpath = modelpath
        model = ufomodels.load_model(modelpath)
        # Check the validity of the model. Too old UFO (before UFO 1.0)
        if not hasattr(model, 'all_orders'):
            raise USRMODERROR('Base Model doesn\'t follows UFO convention (no couplings_order information)\n' +\
                               'MG5 is able to load such model but NOT to the add model feature.')
        if isinstance(model.all_particles[0].mass, six.string_types):
            raise USRMODERROR('Base Model doesn\'t follows UFO convention (Mass/Width of particles are string name, not object)\n' +\
                               'MG5 is able to load such model but NOT to the add model feature.') 
                                 
        old_particles = [id(p) for p in model.all_particles]
        self.particles = [copy.copy(p) for p in model.all_particles]
        if any(hasattr(p, 'loop_particles') for p in self.particles):
            raise USRMODERROR('Base Model doesn\'t follows UFO convention ')
        self.vertices = list(model.all_vertices)
        # ensure that the particles are correctly listed
        for v in self.vertices:
            new_p = []
            for p in v.particles:
                try:
                    new_p.append(self.particles[old_particles.index(id(p))])
                except:
                    p3 = [p2 for p2 in self.particles if p2.name == p.name and p2.pdg_code == p.pdg_code]
                    new_p.append(p3[0])
            v.particles  = new_p

        self.couplings = list(model.all_couplings)
        self.lorentz = list(model.all_lorentz)
        self.parameters = list(model.all_parameters)
        self.Parameter = self.parameters[0].__class__
        self.orders = list(model.all_orders)
        
        self.functions = list(model.all_functions)
        self.new_external = []
        # UFO optional file
        if hasattr(model, 'all_propagators'):
            self.propagators = list(model.all_propagators)
        else:
            self.propagators = [] 
            
        # UFO NLO extension
        if hasattr(model, 'all_CTvertices'):
            self.CTvertices = list(model.all_CTvertices)
        else:
            self.CTvertices = []
        # UFO NLO extension
        if hasattr(model, 'all_CTparameters'):
            self.CTparameters = list(model.all_CTparameters)
        else:
            self.CTparameters = []

        
        #translate for how to write the python file
        if 'self.expr = expression' in open(pjoin(self.modelpath, 'object_library.py')).read():
            self.translate = {'expr': 'expression'}
        else:
            self.translate = {}
        
        #translate for the expression of the UFO model
        self.old_new = {}
        self.addon = addon
        
        # particle id -> object
        self.particle_dict = {}
        for particle in self.particles:
            self.particle_dict[particle.pdg_code] = particle
            
        # path to all model that should be used for the Fortran file.
        self.all_path = [self.modelpath]

    def write(self, outputdir):
        """ """
        if not os.path.exists(outputdir):
            os.mkdir(outputdir)
        files.cp(os.path.join(self.modelpath, '__init__.py'), outputdir)
        files.cp(os.path.join(self.modelpath, 'object_library.py'), outputdir)
        files.cp(os.path.join(self.modelpath, 'write_param_card.py'), outputdir)

        self.write_particles(outputdir)
        self.write_vertices(outputdir)
        self.write_couplings(outputdir)
        self.write_lorentz(outputdir)
        self.write_parameters(outputdir)
        self.write_orders(outputdir)
        self.write_functions(outputdir)
        self.write_propagators(outputdir)
        self.write_ctvertices(outputdir)
        self.write_ctparameters(outputdir)
        
        self.write_external_files(outputdir)
        self.write_restrict_card(outputdir)
    
  
    def mod_file(self, inputpath, outputpath):
        
        fsock = open(outputpath, 'w')
        
        to_change = {}
        to_change.update(self.translate)
        to_change.update(self.old_new)

        pattern = re.compile(r'\b(%s)\b' % ('|'.join(to_change)))
        
        #need to check that all particle are written correctly <- Fix potential issue
        #            of lower/upper case in FR
        all_particles_name = [self.format_param(P)[2:] for P in self.particles]
        all_lower = [p.lower() for p in all_particles_name]
        pat2 = re.compile(r'\bP\.(\w+)\b')
        
        
        for line in open(inputpath):
            line =  pattern.sub(lambda mo: to_change[mo.group()], line)
            part_in_line = set(pat2.findall(line))
            
            #handle the case of lower/upper case particle
            to_replace = {}
            for p in part_in_line:
                if p in all_particles_name:
                    continue
                else:
                    ind = all_lower.index(p.lower())
                    to_replace[p] = all_particles_name[ind]
            if to_replace:
                pat3 = re.compile(r'\bP\.(%s)\b' % '|'.join(p for p in to_replace))
                line =  pat3.sub(lambda mo: 'P.%s'%to_replace[mo.groups(0)[0]], line)
            fsock.write(line)
        
  
    def write_restrict_card(self, outputdir):
        """ propagate model restriction of the original model. """

        restrict_list = [l for l in os.listdir(self.modelpath) if l.startswith('restrict_')]
        if not self.new_external:
            # no new entry in the card => just copy the restrict_card.dat
            for p in restrict_list:
                files.cp(pjoin(self.modelpath, p), outputdir)
                
        else:
            # need to add the parameter and ensure that they will not be restricted!
            for p in restrict_list: 
                param_card = check_param_card.ParamCard(pjoin(self.modelpath, p))
                for parameter in self.new_external:
                    block = parameter.lhablock 
                    lhaid = parameter.lhacode
                    value = parameter.value
                    if value == 0:
                        value = 1e-99
                    elif value == 1:
                        value = 9.999999e-1
                    try:    
                        param_card.add_param(block.lower(), lhaid, value, 'from addon')
                    except check_param_card.InvalidParamCard:
                        logger.warning("%s will not acting for %s %s" % (p, block, lhaid))
                        param_card[block.lower()].get(lhaid).value = value
                # all added -> write it
                param_card.write(pjoin(outputdir, p), precision=7)

    def format_param(self, param):
        """convert param to string in order to have it written correctly for the 
        UFO file"""

        if isinstance(param, six.string_types): 
            return "'%s'" % param.replace("\\", "\\\\").replace('\'', '\\\'').replace('\"', '\\\"')
        elif isinstance(param, int) or isinstance(param, float) or \
                                                       isinstance(param, complex):
            return "%s" % param
        elif isinstance(param, int):
            return ("%s" % param).replace('L','')
        elif isinstance(param, list):
            return '[%s]' % ', '.join(self.format_param(p) for p in param)
        elif isinstance(param, tuple):
            if len(param) == 1:
                return '(%s,)' % self.format_param(param[0]) 
            else:
                return '(%s)' % ','.join([self.format_param(p) for p in param])
        elif isinstance(param, dict):
            return '{%s}' % ','.join(['%s: %s' % (self.format_param(key), self.format_param(value)) for key, value in param.items()])
        elif param.__class__.__name__ == 'Parameter':
            return 'Param.%s' % repr(param)
        elif param.__class__.__name__ == 'Coupling':
            return 'C.%s' % repr(param)
        elif param.__class__.__name__ == 'Lorentz':
            return 'L.%s' % repr(param)
        elif param.__class__.__name__ == 'Particle':
            return 'P.%s' % repr(param)
        elif param is None:
            return 'None'
        else:
            raise Exception('%s unknow type for writting UFO' % param.__class__.__name__)



    def create_data_text(self, obj):
        """ create the data associate to the object"""
        # Most of the object comes from the UFOBASECLASS 
        # BUT NOT ALL (some object) need to deal with both
        
        nb_space = 0    
        if hasattr(obj, 'require_args_all'):
            args = obj.require_args_all
        elif hasattr(obj, 'require_args'):
            args = obj.require_args
        else:
            args = []
        if args:
            text = """%s = %s(""" % (repr(obj), obj.__class__.__name__)
        else:
            text = """%s = %s(""" % (obj.name, obj.__class__.__name__)
            
            
        for data in args:
            if data in self.translate:
                data = self.translate[data]
            if not nb_space:
                add_space = len(text)
            else:
                add_space = 0
                
            if ',' in data:
                continue 
            
            try:
                expr = getattr(obj, data)
            except:
                if data in ['counterterm', 'propagator', 'loop_particles']:
                    expr = None
                    setattr(obj, data, None)
                else:
                    raise
            name =str(data)
            if name in self.translate:
                name = self.translate[name]            
            #if data == 'lhablock':
            #    print data, type(self.format_param(getattr(obj, data)))
            text += '%s%s = %s,\n' % (' ' * nb_space,name, self.format_param(getattr(obj, data)))
            nb_space += add_space

        if hasattr(obj, 'get_all'):
            other_attr = [name for name in obj.get_all().keys() 
                                                  if name not in args]
        else:
            other_attr = list(obj.__dict__.keys())
        
        if str(obj.__class__.__name__) == 'CTParameter' and 'nature' in other_attr:
            logger.critical('UFO model is outdated (including some bugs). Please update object_library.py to latest version')
            other_attr.remove('nature')
        
        other_attr.sort()
        if other_attr == ['GhostNumber', 'LeptonNumber', 'Y', 'partial_widths', 'selfconjugate']:
            other_attr=['GhostNumber', 'LeptonNumber', 'Y','selfconjugate']
            
        for data in other_attr:
            name =str(data)
            if name in ['partial_widths', 'loop_particles']:
                continue
            if name in self.translate:
                name = self.translate[name] 
            if not nb_space:
                add_space = len(text)
            else:
                add_space = 0
            text += '%s%s = %s,\n' % (' ' * nb_space, name, self.format_param(getattr(obj, data)))
            nb_space += add_space
            
        text = text[:-2] + ')\n\n'
        #print text

        return text
             
    def create_file_content(self, datalist):
        """ """
        return '\n'.join([self.create_data_text(obj) for obj in datalist])

            
    def write_particles(self, outputdir):
        """ """
        text = """
# This file was automatically created by The UFO_usermod        

from __future__ import division
from object_library import all_particles, Particle
import parameters as Param

"""
        text += self.create_file_content(self.particles)
        ff = open(os.path.join(outputdir, 'particles.py'), 'w')
        ff.writelines(text)
        ff.close()
        return

    def write_vertices(self, outputdir):
        """ """
        text = """
# This file was automatically created by The UFO_usermod        

from object_library import all_vertices, Vertex
import particles as P
import couplings as C
import lorentz as L

"""
        text += self.create_file_content(self.vertices)
        ff = open(os.path.join(outputdir, 'vertices.py'), 'w')
        ff.writelines(text)
        ff.close()
        return

    def write_ctvertices(self, outputdir):
        """ """
        
        if not self.CTvertices:
            return
        
        text = """
# This file was automatically created by The UFO_usermod        

from object_library import all_vertices, all_CTvertices, Vertex, CTVertex
import particles as P
import couplings as C
import lorentz as L

"""
        text += self.create_file_content(self.CTvertices)
        ff = open(os.path.join(outputdir, 'CT_vertices.py'), 'w')
        ff.writelines(text)
        ff.close()
        return


    def write_couplings(self, outputdir):
        """ """
        text = """
# This file was automatically created by The UFO_usermod        

from object_library import all_couplings, Coupling
"""
        text += self.create_file_content(self.couplings)
        ff = open(os.path.join(outputdir, 'couplings.py'), 'w')
        ff.writelines(text)
        ff.close()
        return

    def write_lorentz(self, outputdir):
        """ """
        text = """
# This file was automatically created by The UFO_usermod        

from object_library import all_lorentz, Lorentz
"""

        text += self.create_file_content(self.lorentz)
        ff = open(os.path.join(outputdir, 'lorentz.py'), 'w')
        ff.writelines(text)
        ff.close()
        return

    def write_parameters(self, outputdir):
        """ """
        text = """
# This file was automatically created by The UFO_usermod        

from object_library import all_parameters, Parameter
"""

        text += self.create_file_content(self.parameters)
        ff = open(os.path.join(outputdir, 'parameters.py'), 'w')
        ff.writelines(text)
        ff.close()
        return

    def write_ctparameters(self, outputdir):
        """ """
        if not self.CTparameters:
            return
        
        text = """
# This file was automatically created by The UFO_usermod        

from object_library import all_CTparameters, CTParameter

from function_library import complexconjugate, re, im, csc, sec, acsc, asec, cot
"""

        text += self.create_file_content(self.CTparameters)
        ff = open(os.path.join(outputdir, 'CT_parameters.py'), 'w')
        ff.writelines(text)
        ff.close()
        return


    def write_orders(self, outputdir):
        """ """
        text = """
# This file was automatically created by The UFO_usermod        
from object_library import all_orders, CouplingOrder
"""

        text += self.create_file_content(self.orders)
        ff = open(os.path.join(outputdir, 'coupling_orders.py'), 'w')
        ff.writelines(text)
        ff.close()
        return

    def write_functions(self, outputdir):
        """ """
        text = """
# This file was automatically created by The UFO_usermod        

import cmath
from object_library import all_functions, Function

"""

        text += self.create_file_content(self.functions)
        ff = open(os.path.join(outputdir, 'function_library.py'), 'w')
        ff.writelines(text)
        ff.close()
        return

    def write_propagators(self, outputdir):
        """ """
        
        text = """
# This file was automatically created by The UFO_usermod   

from object_library import all_propagators, Propagator
"""

        text += self.create_file_content(self.propagators)
        ff = open(os.path.join(outputdir, 'propagators.py'), 'w')
        ff.writelines(text)
        ff.close()
        return

    def write_external_files(self, outputdir):
        """Copy/merge the routines written in Fortran/C++/pyhton"""
        
        #1. Special case for the formfactor written in Fortran
        re_fct = re.compile('''^\s{7,70}[\w\s]*function (\w*)\(''',re.M+re.I)
        present_fct = set()
        for dirpath in self.all_path:
            if os.path.exists(pjoin(dirpath, 'Fortran', 'functions.f')):
                text = open(pjoin(dirpath, 'Fortran', 'functions.f')).read()
                new_fct = re_fct.findall(text)
                nb_old = len(present_fct)
                nb_added = len(new_fct)
                new_fct = set([f.lower() for f in new_fct])
                present_fct.update(new_fct)
                if len(present_fct) < nb_old + nb_added:
                    logger.critical('''Some Functions in functions.f are define in more than one model.
                    This require AT LEAST manual modification of the resulting file. But more likely the 
                    model need to be consider as un-physical! Use it very carefully.''')
                
                if not os.path.exists(pjoin(outputdir, 'Fortran')):
                    os.mkdir(pjoin(outputdir, 'Fortran'))
                fsock = open(pjoin(outputdir, 'Fortran','functions.f'),'a')
                fsock.write(text)
                fsock.close()
                
        #2. Ohter files present in Fortran/Cpp/Python directory
        #   ASk user to handle it if any!
        for dirpath in self.all_path:
            for subdir in ['Fortran', 'CPP', 'Python']:
                if os.path.exists(pjoin(dirpath, subdir)):
                    for filepath in os.listdir(pjoin(dirpath, subdir)):
                        if filepath == 'functions.f':
                            continue
                        if '.' not in filepath:
                            continue
                        logger.warning('Manual HELAS routine associated to the model. Those are not modified automaticaly!! So you need to manually checked them')
                        nb = 0
                        name, extension = filepath.rsplit('.', 1) 

                        while 1:
                            filename = '%s%s%s' %(name, '.moved' * nb, extension)
                            if os.path.exists(pjoin(outputdir, subdir, filename)):
                                nb+=1
                            else:
                                break
                        if not os.path.exists(pjoin(outputdir, subdir)):
                            os.mkdir(pjoin(outputdir, subdir))
                        files.cp(pjoin(dirpath, subdir, filepath), pjoin(outputdir, subdir, filename))
                        
    def get_particle(self, name):
        """ """
        for part in self.particles:
            if part.name == name:
                return part
        
        raise USRMODERROR('no particle %s in the model' % name)

    def add_parameter(self, parameter, identify_pid={}):
        """wrapper to call the correct function"""

        if parameter.nature == 'internal':
            self.add_internal_parameter(parameter)
        else:
            self.add_external_parameter(parameter, identify_pid)

    def add_particle(self, particle, identify=None):
        """Add a particle in a consistent way"""
        
        name = particle.name
        if identify:
            name = identify
        old_part = next((p for p in self.particles if p.name==name), None)
        if not old_part:
            first = True
            for p in self.particles:
                if p.name.lower() == name.lower():
                    if not first:
                        raise Exception
                    else:
                        first =False
                    old_part = p
        
        
        
        if old_part:
            #Check if the two particles have the same pdgcode
            if old_part.pdg_code == particle.pdg_code:
                particle.replace = old_part
                return self.check_mass_width_of_particle(old_part, particle)
            elif identify:
                if particle.spin != old_part.spin:
                    raise USRMODERROR("identify particles should have the same spin")
                elif particle.color != old_part.color:
                    raise USRMODERROR("identify particles should have the same color")
                particle.replace = old_part
                return self.check_mass_width_of_particle(old_part, particle)
            else:
                logger.warning('The particle name \'%s\' is present in both model with different pdg code' % name)
                logger.warning('The particle coming from the plug-in model will be rename to \'%s%s\'' % (name, self.addon))
                particle.name = '%s%s' % (name, self.addon)
                particle.antiname = '%s%s' % (particle.antiname, self.addon)
                self.particles.append(particle)
                return
        elif identify:
            raise USRMODERROR("Particle %s is not in the model" % identify)

        pdg = particle.pdg_code
        if pdg in self.particle_dict:
            particle.replace = self.particle_dict[pdg]
            return self.check_mass_width_of_particle(self.particle_dict[pdg], particle)
        else:
            if hasattr(particle, 'replace'):
                del particle.replace
            self.particles.append(particle)
        
                
    def check_mass_width_of_particle(self, p_base, p_plugin):
        # Check the mass
        if p_base.mass.name != p_plugin.mass.name:
            #different name but actually  the same
            if p_plugin.mass.name in self.old_new:
                if self.old_new[p_plugin.mass.name] != p_base.mass.name:
                    raise USRMODERROR('Some inconsistency in the mass assignment in the model: equivalent of %s is %s != %s ' % ( p_plugin.mass.name, self.old_new[p_plugin.mass.name], p_base.mass.name))
            elif  p_base.mass.name.lower() == 'zero':
                p_base.mass = p_plugin.mass
            elif  p_plugin.mass.name.lower() == 'zero':
                pass
            else:
                misc.sprint(p_base.mass.value, p_plugin.mass.value, dir(p_base.mass))
                misc.sprint(p_base.mass.nature, p_plugin.mass.nature)
                misc.sprint(self.old_new)
                raise USRMODERROR('Some inconsistency in the mass assignment in the model\n' + \
             '     Mass: %s and %s\n' %(p_base.mass.name, p_plugin.mass.name) + \
             '     conflict name %s\n' % self.old_new + \
             '     pdg_code: %s %s' % (p_base.pdg_code, p_plugin.pdg_code))
        # Check the width
        if p_base.width.name != p_plugin.width.name:
            #different name but actually  the same
            if p_plugin.width.name in self.old_new:
                if self.old_new[p_plugin.width.name] != p_base.width.name:
                    raise USRMODERROR('Some inconsistency in the mass assignment in the model')
            elif  p_base.width.name.lower() == 'zero':
                p_base.width = p_plugin.width
            elif  p_plugin.width.name.lower() == 'zero':
                pass
            else:
                raise USRMODERROR('Some inconsistency in the mass assignment in the model')        
        
        return

    def add_external_parameter(self, parameter, identify_pid):
        """adding a param_card parameter inside the current model.
           if the parameter block/lhcode already exists then just do nothing
           (but if the name are different then keep the info for future translation)
           If the name already exists in the model. raise an exception.
        """
  
        name = parameter.name
        # check if a parameter already has this name
        old_param = next((p for p in self.parameters if p.name==name), None)
        if old_param:
            if old_param.lhablock == parameter.lhablock and \
                old_param.lhacode == parameter.lhacode:
                return #Nothing to do!
            else:
                logger.info('The two model defines the parameter \'%s\'\n' % parameter.name +
                  '      the original model for %s :%s\n' %(old_param.lhablock, old_param.lhacode)+
                  '      the plugin for %s :%s\n' %(parameter.lhablock,parameter.lhacode)+
                  '      We will rename the one from the plugin to %s%s' % (parameter.name, self.addon))
                if old_param.nature == 'internal':
                    logger.warning('''The parameter %s is actually an internal parameter of the base model.
    his value is given by %s.
    If those two parameters are expected to be identical, you need to provide the value in the param_card according to this formula.
    ''')
                #add the parameter with a new name. 
                self.old_new[parameter.name] = '%s%s' % (parameter.name, self.addon)
                parameter.name = '%s%s' % (parameter.name, self.addon)
                #
                #self.parameters.append(parameter)
                #return
        #check if a parameter already has this lhablock/code information
        lhacode = parameter.lhacode
        if parameter.lhablock.lower() in ['mass', 'decay']:
            if int(parameter.lhacode[0]) in identify_pid:
                lhacode = [identify_pid[int(parameter.lhacode[0])]]
        
        old_param = next((p for p in self.parameters if p.lhacode==lhacode \
                          and p.lhablock==parameter.lhablock), None)
        if old_param:
            logger.info('The two model defines the block \'%s\' with id \'%s\' with different parameter name \'%s\', \'%s\'\n'\
                      %  (old_param.lhablock, old_param.lhacode, parameter.name, old_param.name) + \
            '     We will merge those two parameters in a single one')
            if parameter.name in list(self.old_new.values()):
                key = [k for k in self.old_new if self.old_new[k] == parameter.name][0]
                self.old_new[key] = old_param.name
                self.old_new[parameter.name] = old_param.name
            else:
                self.old_new[parameter.name] = old_param.name
            #            self.add_internal_parameter(iden_param)
        
        elif parameter.lhablock.lower() in ['mass', 'decay'] and int(parameter.lhacode[0]) in identify_pid:
            # this means that the parameter is an internal parameter in the original model...
            #find it via the particle name
            orig_particle = self.particle_dict[lhacode[0]]
            if parameter.lhablock.lower() == 'mass':
                old_param = orig_particle.mass
            else:
                old_param = orig_particle.width
            if old_param.name.lower() == 'zero':
                #Just add the new parameter to the current list
                self.parameters.append(parameter) 
                self.new_external.append(parameter)                
            else:
                logger.info('The two model defines the parameter for  block \'%s\' with id \'%s\' with different parameter name \'%s\', \'%s\'\n'\
                      %  (parameter.lhablock.lower(), lhacode[0], parameter.name, old_param.name) + \
                '     We will merge those two parameters in a single one')
                if parameter.name in list(self.old_new.values()):
                    key = [k for k in self.old_new if self.old_new[k] == parameter.name][0]
                    self.old_new[key] = old_param.name
                    self.old_new[parameter.name] = old_param.name
                else:
                    self.old_new[parameter.name] = old_param.name
                    #            self.add_internal_parameter(iden_param)                            
        else:
            #Just add the new parameter to the current list
            self.parameters.append(parameter) 
            self.new_external.append(parameter)
    
    def add_internal_parameter(self, parameter):
        """ add a parameter of type internal """
        
        name = parameter.name
        # check if a parameter already has this name
        old_param = next((p for p in self.parameters if p.name==name), None)
        if old_param:
            if old_param.value == parameter.value:
                return #Nothing to do!
            else:
                if self.old_new:
                    pattern = re.compile(r'\b(%s)\b' % '|'.join(list(self.old_new.keys())))
                    def replace(matchobj):
                        return self.old_new[matchobj.group(0)]
                    parameter.value = pattern.sub(replace, parameter.value)
                    self.old_new[parameter.name] = '%s%s' % (parameter.name, self.addon)
                
                parameter.name = '%s%s' % (parameter.name, self.addon)
                self.parameters.append(parameter)
                return
        
        # No name conflict:
        if self.old_new:
            pattern = re.compile(r'\b(%s)\b' % '|'.join(list(self.old_new.keys())))
            def replace(matchobj):
                return self.old_new[matchobj.group(0)]
            parameter.value = pattern.sub(replace, parameter.value)

        self.parameters.append(parameter)
        



    def add_coupling(self, coupling):
        """add one coupling"""
        
        # avoid name duplication
        name = coupling.name
        same_name = next((p for p in self.couplings if p.name==name), None)
        if same_name:
            coupling.name = '%s%s' % (coupling.name, self.addon)
            return self.add_coupling(coupling)
        
        if self.old_new:  
            pattern = re.compile(r'\b(%s)\b' % '|'.join(list(self.old_new.keys())))
            def replace(matchobj):
                return self.old_new[matchobj.group(0)]
            coupling.value = pattern.sub(replace, coupling.value)
        
        old_coupling = next((p for p in self.couplings if p.value==coupling.value), None)
        
        if old_coupling:
            coupling.replace = old_coupling #tag for replacement
        else:
            self.couplings.append(coupling)
    
    def add_coupling_order(self, coupling_order):
        """adding a new coupling order inside the model"""
        
        name = coupling_order.name
        same_name = next((p for p in self.orders if p.name==name), None)
        if same_name:
            if coupling_order.hierarchy != same_name.hierarchy:
                logger.warning('%s has different hierarchy use the minimal value (%s, %s) => %s' \
                               % (name, same_name.hierarchy, coupling_order.hierarchy,
                                  min(same_name.hierarchy, coupling_order.hierarchy)))
                same_name.hierarchy = min(same_name.hierarchy, coupling_order.hierarchy)
            if coupling_order.expansion_order != same_name.expansion_order:
                logger.warning('%s has different expansion_order use the minimal value (%s, %s) => %s' \
                               % (name, coupling_order.expansion_order, same_name.expansion_order, 
                                  min(same_name.expansion_order, coupling_order.expansion_order)))
                same_name.expansion_order = min(same_name.expansion_order, coupling_order.expansion_order)
            if hasattr(same_name, 'perturbative_expansion') and same_name.perturbative_expansion:
                logger.info('%s will be forbidden to run at NLO' % same_name.name)
                same_name.perturbative_expansion = 0
                
                
        else:
            self.orders.append(coupling_order)
    
    def add_lorentz(self, lorentz):
        """add one coupling"""
        
        # avoid name duplication
        name = lorentz.name
        same_name = next((p for p in self.lorentz if p.name==name), None)
        if same_name:
            lorentz.name = '%s%s' % (lorentz.name, self.addon)
        
        if self.old_new:     
            pattern = re.compile(r'\b(%s)\b' % '|'.join(list(self.old_new.keys())))
            def replace(matchobj):
                return self.old_new[matchobj.group(0)]
            lorentz.structure = pattern.sub(replace, lorentz.structure)
        
        old_lor = next((p for p in self.lorentz 
                        if p.structure==lorentz.structure and p.spins == lorentz.spins), 
                       None)
        
        if old_lor:
            lorentz.replace = old_lor #tag for replacement
        else:
            self.lorentz.append(lorentz)    
        
    def add_interaction(self, interaction , model):
        """Add one interaction to the model. This is UNCONDITIONAL!
        if the same interaction is in the model this means that the interaction
        will appear twice. This is now weaken if both interaction are exactly identical!
        (EXACT same color/lorentz/coupling expression)
        """

        interaction = interaction.__class__(**interaction.__dict__)
        model.all_vertices.pop(-1)
        
        #0. check name:
        name = interaction.name
        same_name = next((p for p in self.vertices if p.name==name), None)
        if same_name:
            interaction.name = '%s%s' % (interaction.name, self.addon)
        
        #1. check particles translation
        particles = [p.replace if hasattr(p, 'replace') else p for p in interaction.particles]
        interaction.particles = particles
        #2. check the lorentz structure
        lorentz = [l.replace if hasattr(l, 'replace') else l for l in interaction.lorentz]
        interaction.lorentz = lorentz

        #3. check the couplings
        couplings = [(key, c.replace) if hasattr(c, 'replace') else (key, c)
                     for key, c in interaction.couplings.items()]
        interaction.couplings = dict(couplings)
        
        #4. Try to avoid duplication of interaction:
        # A crash is raised if the same particles have already the some lorentz structure
        # at the same coupling order:
        get_pdg = lambda vertex: sorted([p.pdg_code for p in vertex.particles])
        id_part = get_pdg(interaction)
        iden_vertex = [v for v in self.vertices if get_pdg(v) == id_part]
        iden = False
        nb_coupling = len(interaction.couplings)
        keys = list(interaction.couplings.keys()) # to have a fixed order!
        
        get_lor_and_color =  lambda i: (interaction.lorentz[keys[i][1]].structure,
                       interaction.color[keys[i][0]])
        for v in iden_vertex:
            if len(v.couplings) != nb_coupling:
                continue
            found = []
            for ((i,j), coup) in v.couplings.items():
                new_lorentz = v.lorentz[j].structure
                new_color = v.color[i]
                k=0
                same = [k for k in range(nb_coupling) if k not in found and 
                        get_lor_and_color(k) == (new_lorentz, new_color)]
                if not same:
                    break
                else:
                    for k in same:
                        if interaction.couplings[keys[k]] == coup:
                            found.append(k)
                            break
                    else:
                        # check only the coupling order
                        for k in same:
                            if interaction.couplings[keys[k]].order == coup.order:
                                found.append(k)
                                warning = """Did NOT add interaction %s since same particles/lorentz/color/coupling order 
    BUT did not manage to ensure that the coupling is the same. couplings expression:
    base model: %s
    addon model: %s
    """ % (id_part, coup.value, interaction.couplings[keys[k]].value)
                                logger.warning(warning)
                                found.append(k)
                                break                        
                        else:
                            pass
                            # mat
            else:
                # all found one identical...
                return
                
        logger.info('Adding interaction for the following particles: %s' % id_part)
        
        
        
        
        self.vertices.append(interaction)

    def add_CTinteraction(self, interaction):
        """Add one interaction to the model. This is UNCONDITIONAL!
        if the same interaction is in the model this means that the interaction
        will appear twice."""
        
        #0. check name:
        name = interaction.name
        same_name = next((p for p in self.vertices if p.name==name), None)
        if same_name:
            interaction.name = '%s%s' % (interaction.name, self.addon)
        
        #1. check particles translation
        particles = [p.replace if hasattr(p, 'replace') else p for p in interaction.particles]
        interaction.particles = particles
        
        #2. check the lorentz structure
        lorentz = [l.replace if hasattr(l, 'replace') else l for l in interaction.lorentz]
        interaction.lorentz = lorentz

        #3. check the couplings
        couplings = [(key, c.replace) if hasattr(c, 'replace') else (key, c)
                     for key, c in interaction.couplings.items()]
        interaction.couplings = dict(couplings)
        

        #4. check the loop_particles
        loop_particles=[ [p.replace if hasattr(p, 'replace') else p for p in plist]
                         for plist in interaction.loop_particles]
        interaction.loop_particles = loop_particles
        self.CTvertices.append(interaction)
        

    def add_model(self, model=None, path=None, identify_particles=None):
        """add another model in the current one"""
        
        
        self.new_external = []
        if path:
            model = ufomodels.load_model(path) 
                
        if not model:
            raise USRMODERROR('Need a valid Model')
        else:
            path = model.__path__[0]
        # Check the validity of the model. Too old UFO (before UFO 1.0)
        if not hasattr(model, 'all_orders'):
            raise USRMODERROR('Add-on Model doesn\'t follows UFO convention (no couplings_order information)\n' +\
                               'MG5 is able to load such model but NOT to the add model feature.')
        if isinstance(model.all_particles[0].mass, six.string_types):
            raise USRMODERROR('Add-on Model doesn\'t follows UFO convention (Mass/Width of particles are string name, not object)\n' +\
                               'MG5 is able to load such model but NOT to the add model feature.') 
    
        for order in model.all_orders:
            if hasattr(order, 'perturbative_expansion') and order.perturbative_expansion:
                raise USRMODERROR('Add-on model can not be loop model.') 
                              
        for order in model.all_orders:
            self.add_coupling_order(order)
        
        # Adding automatically identification for anti-particle if needed
        # + define identify_pid which keep tracks of the pdg_code identified
        identify_pid = {}
        if identify_particles:
            for new, old in dict(identify_particles).items():
                new_part = next((p for p in model.all_particles if p.name==new), None)
                old_part = next((p for p in self.particles if p.name==old), None)
                # secure agqinst lower/upper case problem
                if not new_part:
                    first = True
                    for p in model.all_particles:
                        if p.name.lower() == new.lower():
                            if not first:
                                raise Exception
                            else:
                                first =False
                            new_part = p
                if not old_part:
                    first = True
                    for p in self.particles:
                        if p.name.lower() == old.lower():
                            if not first:
                                raise Exception
                            else:
                                first =False
                            old_part = p
                    if not old_part:
                    # last possibility is that the model do not follow MG5 convention
                    # but that "old" does
                        defaultname = base_objects.Model.load_default_name() # id->name
                        for pdg, value in defaultname.items():
                            if value == old:
                                old_part = self.particle_dict[pdg]
                                identify_particles[new] = old_part.name
                                break
                    
                # end for the case security
                identify_pid[new_part.pdg_code] = old_part.pdg_code   
                if new_part is None:
                    raise USRMODERROR("particle %s not in added model" % new)
                if old_part is None:
                    raise USRMODERROR("particle %s not in original model" % old)
                if new_part.antiname not in identify_particles:
                    new_anti = new_part.antiname
                    old_anti = old_part.antiname
                    if old_anti == old:
                        raise USRMODERROR("failed identification (one particle is self-conjugate and not the other)")
                    logger.info("adding identification for anti-particle: %s=%s" % (new_anti, old_anti))
                    identify_particles[new_anti] = old_anti
                    
        for parameter in model.all_parameters:
            self.add_parameter(parameter, identify_pid)
        for coupling in model.all_couplings:
            self.add_coupling(coupling)
        for lorentz in model.all_lorentz:
            self.add_lorentz(lorentz)
        for particle in model.all_particles:
            if particle.name in identify_particles:
                self.add_particle(particle, identify=identify_particles[particle.name])
            else:
                self.add_particle(particle)
        for vertex in model.all_vertices:
            self.add_interaction(vertex, model)
        
        self.all_path.append(path)
        
        
        return

#    def add_particle_from_model(self, model, name):
#        """add the particles NAME from model model (either path or object)
#        names can be either the name of one particle or a list of particle name
#        """
#
#        if isinstance(model, basestring):
#            model = UFOModel(self.modelpath)
#
#        
#        if isinstance(name, list):
#            [self.add_particles(self.modelpath, name) for name in names]
#            return 
#        
#        # Check Validity
#        part = self.get_particle(name)
#        if self.particles_dict.has_key(part.pdg_code):
#            raise USRMODERROR, 'The model contains already a particle with pdg_code %s.' % part.pdg_code
#        
#        # Add the particles to model
#        self.particles.append(part)
#        self.particles_dict[part.pdg_code] = part
#
#        # Loop over the interactions of the other model and add (if possible) the interactions 
#        #associated to the new particles
#        possibility = [v for v in vertex if part in v.particles]
#
#        for vertex in possibility:
#            # Check that all particles are define in the model
#            for particles in vertex.particles:
#                if particles.pdg_code not in self.particles_dict:
#                    continue
#            # Add the interactions/lorentz structure/coupling
#            self.vertices.append(vertex)
#            # NEED WORK!!!!!
            
    



    

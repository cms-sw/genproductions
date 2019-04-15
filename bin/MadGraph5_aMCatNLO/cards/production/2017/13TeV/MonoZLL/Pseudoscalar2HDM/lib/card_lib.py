import os
import sys
import fnmatch
import logging
import shutil
import copy
from card_utilities import *

log = logging.getLogger( 'card_lib.py' )

class Parameter():
    def __init__(self, name, value=None, printme=False):
        self.name = name
        self.value = value
        self.printme = printme
    def __repr__(self):
        return "{NAME}-{VALUE}".format(NAME=self.name, VALUE=self.value)
    def __str__(self):
        return self.__repr__()
    def get_set_string(self):
        return "set param_card {NAME} {VALUE}".format(NAME=self.name, VALUE=self.value)



class ParameterPoint():
    def __init__(self,other=None):
        if other:
            self.parameters = copy.deepcopy(other.parameters)
        else:
            self.parameters = {}

    def get_set_string(self):
        s=""
        for par in sorted(self.parameters.values(), key = lambda x : x.name):
            if(not par):
                raise RuntimeError( "Encountered unitiated parameter: " + par.name )
            s = s + par.get_set_string() + "\n"
        return s
    def get_dataset_identifier(self):
        raise NotImplementedError("You need to reimplement get_dataset_identifier for each type of model!")


class CardWriter():
    def __init__(self,name=""):
        self.name = name
        self.model = ""
        self.decay = ""
        self.process = ""
        self.restrict = ""
        self.parameter_points = []
        self.auto_widths = []
        self.template_path = ""

        self.output_path = "./output"

    def copy_and_fill_template(self,point):
        dataset = point.get_dataset_identifier(tag=self.name)

        # Create card folder
        outdir = os.path.join(self.output_path,dataset)
        log.debug("Setting up output directory " + outdir)
        if(not os.path.exists(outdir)):
            os.makedirs(outdir)

        # Copy each of the files from the template directory
        for filebase in os.listdir(self.template_path):
            infile = os.path.join(self.template_path, filebase)

            if( not self.decay and "madspin" in infile.lower() ): continue

            # File-type dependent treatment
            if(any( [infile.endswith(x) for x in [".f",".dat"]]) ):
                outfile = os.path.join(outdir, "{DATASET}_{FILEBASE}".format(DATASET=dataset, FILEBASE=filebase))
            elif(any( [infile.endswith(x) for x in [".tar.gz",".tgz",".zip","makefile"]])):
                outfile = os.path.join(outdir, "{FILEBASE}".format(FILEBASE=filebase))
            else:
                continue
            shutil.copyfile(infile, outfile)

            # Fill placeholder values
            if(any( [outfile.endswith(x) for x in [".f",".dat"] ])):
                sed_inplace(outfile,"\@PROCESS",self.process)
                sed_inplace(outfile,"\@MODEL",self.model)
                if(len(self.restrict)):
                    sed_inplace(outfile,"\@RESTRICT",("" if self.restrict.startswith("-") else "-") + self.restrict )
                else:
                    sed_inplace(outfile,"\@RESTRICT", "" )
                sed_inplace(outfile,"\@DATASET",dataset)
                if(self.decay): sed_inplace(outfile,"\@DECAY",self.decay)

    def write_customize_card(self,parameter_point):
        dataset = parameter_point.get_dataset_identifier(self.name)
        path_to_card = os.path.join(self.output_path,dataset,"{DATASET}_customizecards.dat".format(DATASET=dataset))
        with open(path_to_card, "w") as f:
            f.write(parameter_point.get_set_string())
            for width in self.auto_widths:
                f.write("set {WIDTH} AUTO\n".format(WIDTH=width))

    def write_cards(self):
        for point in self.parameter_points:
            log.info("Writing card for dataset: " + point.get_dataset_identifier(self.name))
            self.copy_and_fill_template(point)
            self.write_customize_card(point)

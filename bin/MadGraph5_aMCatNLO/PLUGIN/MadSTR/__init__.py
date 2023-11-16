#####################################################
#                                                   #
#  Source file of the MadSTR plugin                  #
#  Use only with consent of its authors.            #
#                                                   #
#              authors: M. Zaro,                    #
#                                                   #
#                                                   #
#####################################################

import os
import sys
root_path = os.path.split(os.path.dirname(os.path.realpath( __file__ )))[0]
sys.path.insert(0, root_path)

import MadSTR.madstr_interface as madstr_interface
import MadSTR.madstr_exporter as madstr_exporter
##import Resummation.resummation_exporters as resummation_exporters

# Three types of functionality are allowed in a plugin
#   1. new output mode
#   2. new cluster support
#   3. new interface

# 1. Define new output mode
#    example: new_output = {'myformat': MYCLASS}
#    madgraph will then allow the command "output myformat PATH"
#    MYCLASS should inherated of the class madgraph.iolibs.export_v4.VirtualExporter 
new_output = {'madstr': madstr_exporter.MadSTRExporter}

# 2. Define new way to handle the cluster.
#    example new_cluster = {'mycluster': MYCLUSTERCLASS}
#    allow "set cluster_type mycluster" in madgraph
#    MYCLUSTERCLASS should inherated from madgraph.various.cluster.Cluster
new_cluster = {}

# 3. Define a new interface (allow to add/modify MG5 command)
#    This can be activated via ./bin/mg5_aMC --mode=PLUGINNAME
## Put None if no dedicated command are required
new_interface = madstr_interface.MadSTRInterface
 
 
########################## CONTROL VARIABLE ####################################
__author__ = 'Marco Zaro'
__email__ = 'marco.zaro@gmail.com'
__version__ = (1,0,0)
minimal_mg5amcnlo_version = (2,9,0) 
maximal_mg5amcnlo_version = (2,9,1000)
latest_validated_version = (2,9,0)

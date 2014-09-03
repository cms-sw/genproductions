import FWCore.ParameterSet.Config as cms
import os

def customise(process):
  os.system("echo 'Start directory'; pwd")
  os.system("cd $CMSSW_BASE/..")
  os.system("fn-fileget -c `cmsGetFnConnect frontier://smallfiles` slc5_ia32_gcc434/sherpa/1.3.0-cms/sherpa_DYToLL_10to50_MASTER_v2.tgz")
  os.system("tar -xzf sherpa_DYToLL_10to50_MASTER_v2.tgz")
  os.system("echo 'CmsRun directory:'; pwd")
  os.system("ls *")
  os.system("cd -")
  os.system("echo 'Back to original directory:'; pwd")
  return(process)

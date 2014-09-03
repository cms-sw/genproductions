import FWCore.ParameterSet.Config as cms
import os

def customise(process):
  os.system("echo 'Start directory'; pwd;
             cd $CMSSW_BASE/..;
             fn-fileget -c `cmsGetFnConnect frontier://smallfiles` slc5_ia32_gcc434/sherpa/1.3.0-cms/sherpa_DYToLL_10to50_MASTER_v2.tgz;
             tar -xzf sherpa_DYToLL_10to50_MASTER_v2.tgz;
             echo 'CmsRun directory:'; pwd;
             ls *;
             cd -;
             echo 'Back to original directory:'; pwd")
  return(process)

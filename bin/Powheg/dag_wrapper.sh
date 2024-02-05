#!/bin/bash

# cannot submit condor DAG from within CMSSW environment
eval `scram unsetenv -sh`
condor_submit_dag $1

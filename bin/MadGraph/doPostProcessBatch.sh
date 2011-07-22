#!/bin/bash

PRODSOURCE=/castor/cern.ch/user/b/bortigno/MadGraph/zvv_pT_100GeV
PRODDEST=/castor/cern.ch/user/l/lenzip/Madgraph/zvv_pT_100GeV_bortignon/postprocessed_new

#same inputs as doPostProcess.sh

# this is just the tarball of what comes from cvs co Configuration/GenProduction/bin/MadGraph
# I keep a tarball just not to have multiple processes doing cvs co.
cp /afs/cern.ch/cms/CAF/CMSCOMM/COMM_GLOBAL/lenzip/postprocessing/MadGraph_postproc.tgz .
tar -xzvf MadGraph_postproc.tgz
cd MadGraph
tar -xzvf upload2mcdb_replace.tar.gz


for i in `seq $1 $2`; do
  rfcp ${PRODSOURCE}/events_zvv_pT_100GeV_nevt50000_seed${i}.lhe ./7TeV_${3}_run${i}_unweighted_events.lhe
done

./doPostProcess.sh $1 $2 $3 $4 $5 $6 $7 $8 $9 | tee postproc_${1}_${2}.log

rfcp mgPostv2/*.lhe ${PRODDEST}
rfcp postproc_${1}_${2}.log ${PRODDEST}

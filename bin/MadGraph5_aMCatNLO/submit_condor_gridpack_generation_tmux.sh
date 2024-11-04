# this two lines must be in Utilities/source_condor.sh 
# export _condor_SCHEDD_HOST=bigbird17.cern.ch
# export _condor_CREDD_HOST=bigbird17.cern.ch 

# condor_q -name bigbird17.cern.ch
#
host="$(hostname)"  
date="$(date)" 
(
  echo "gridpack: $1"
  echo "host: ${host}. $(date)"
  echo "session: gridpack_submission_$1"
) >> tmux_condor_gridpack.log

systemd-run --scope --user tmux new -s "gridpack_submission_$1" -d  
tmux send-keys -t "gridpack_submission_$1" "./submit_condor_gridpack_generation.sh \"$1\" \"$2\"" C-m  
tmux attach -t "gridpack_submission_$1"  

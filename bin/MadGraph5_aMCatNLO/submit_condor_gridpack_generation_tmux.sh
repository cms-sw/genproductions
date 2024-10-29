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

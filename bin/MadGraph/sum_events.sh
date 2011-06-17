

awk '/^(#|$)/{next;}{++c;s+=$1} END {printf ("# %d total jobs: ",c);c=(c>0?c:1);print "sum: " s ", naverage: " s/c ;print  s/c}'  $1  > total_events_$2

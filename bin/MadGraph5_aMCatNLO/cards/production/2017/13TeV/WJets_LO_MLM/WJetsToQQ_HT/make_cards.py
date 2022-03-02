import os
import sys
import re

ht_bins = {
    "400to600": [400, 600], 
    "600to800": [600, 800],
    "800toInf": [800, -1]
}

for ht_bin_name, ht_range in ht_bins.items():
    with open("WJetsToQQ_HTx_proc_card.dat", "r") as proc_template:
        with open("WJetsToQQ_HT{}_proc_card.dat".format(ht_bin_name), "w") as proc_output:
            for in_line in proc_template:
                out_line = in_line.replace("HTx", "HT{}".format(ht_bin_name))
                proc_output.write(out_line)

    with open("WJetsToQQ_HTx_run_card.dat", "r") as run_template:
        with open("WJetsToQQ_HT{}_run_card.dat".format(ht_bin_name), "w") as run_output:
            for in_line in run_template:
                out_line = in_line.replace("HTx", "HT{}".format(ht_bin_name))\
                                  .replace("xHTJMIN", str(ht_range[0]))\
                                  .replace("xHTJMAX", str(ht_range[1]))
                run_output.write(out_line)

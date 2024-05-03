#!/usr/bin/env python
# coding: utf-8
import os
import sys
import re

ht_bins = {
    "100to200": [100.0, 200.0], 
    "200": [200.0, -1],
}

for ht_bin_name, ht_range in ht_bins.items():
    with open("ZBJetsToNuNu_Zpt-HTx_proc_card.dat", "r") as proc_template:
        with open("ZBJetsToNuNu_Zpt-{}_proc_card.dat".format(ht_bin_name), "w") as proc_output:
            for in_line in proc_template:
                out_line = in_line.replace("HTx", "{}".format(ht_bin_name))
                proc_output.write(out_line)

    with open("ZBJetsToNuNu_Zpt-HTx_run_card.dat", "r") as run_template:
        with open("ZBJetsToNuNu_Zpt-{}_run_card.dat".format(ht_bin_name), "w") as run_output:
            for in_line in run_template:
                out_line = in_line.replace("HTx", "{}".format(ht_bin_name))                                  .replace("xPTJMIN", str(ht_range[0]))                                  .replace("xPTJMAX", str(ht_range[1]))
                run_output.write(out_line)

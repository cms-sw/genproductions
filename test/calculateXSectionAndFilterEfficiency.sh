# Use this command to obtain the cross section and filter efficiency

# Adjust the MySample_cff.py file, number of events to generate and GlobalTag

# To calculate filter efficiency, in the output look for:

# TrigReport ---------- Modules in Path: generation_step ------------
# TrigReport  Trig Bit#    Visited     Passed     Failed      Error Name
# TrigReport     1    0     100000     100000          0          0 generator
# TrigReport     1    0     100000      25834      74166          0 oniafilter
# TrigReport     1    0      25834      14286      11548          0 mumugenfilter

cmsDriver.py Configuration/GenProduction/python/MySample_cff.py -s GEN -n 100000 --conditions FrontierConditions_GlobalTag,STARTUP3X_V8H::All --customise Configuration/GenProduction/calculateXSectionAndFilterEfficiency.py


These can be used to reweight POWHEG samples to correct for P_decay(m4l).  See talks at the [HWW](https://indico.cern.ch/event/515348/contributions/2038297/attachments/1253630/1849648/high_mass_WW_ZZ.pdf) and [HZZ](https://indico.cern.ch/event/515348/contributions/2038276/attachments/1255124/1852487/high_mass_ZZ_WW.pdf) meetings.

These distributions were calculated with 200000 integration points at each mass point.  They should be fine for high mass samples, with a precision of around 0.2%.  They should not be used for low mass samples, where interference effects become important.  Even for final states without interference, like any WW or ZZ->2l2q, once the Z's or W's go offshell the precision gets worse, and more iterations would be needed.  This effect is not important for high mass samples with a low mass tail because the VV decay is suppressed once the V's are offshell.

Because the weights are normalized to 1 at the pole mass, these distributions should be fine for any ZZ or WW final state, which differ only by normalization above 2mV.

To use:
 1. Download one of the distributions from this directory, either the ZZ or WW.  It should be stored in the girdpack in the same directory where JHUGen will be run.  It also has to be called PMZZdistribution.out (even for WW).
 2. Add the following command line options in the JHUGen card:
      ReadLHE=(input file).lhe ReweightDecay WidthSchemeIn=3 ReadPMZZ
 3. Run

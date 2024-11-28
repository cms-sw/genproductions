These cards are for the SMP aTGC analysis with full run-2 dataset. The process that we considered is WV semileptonic decay.

To get the cards first setup the environment and run the command

```bash
. /cvmfs/sft.cern.ch/lcg/views/LCG_106/x86_64-el9-gcc13-opt/setup.sh
python generate_cards.py
```

Few important points to note:
1. The template cards are in the `CardsTemplates` directory.
2. The cards: `run_card.dat`,  `customizecards.dat`, `extramodels.dat`, `FKS_params.dat`, `reweight_card.dat` and `run_card.dat` remains same for all the processes.
3. The `proc_card.dat` is different for each process. The processs names are defined in the dict that we are using to generate the cards.

    ```python
    processes = {
        "WpWmToLpNujj": ["p p > ell+ vl w- $$ t t~ H QED=3 [QCD] @0", "p p > ell+ vl w- j $$ t t~ H QED=3 [QCD] @1"],
        "WmWpToLmNujj": ["p p > ell- vl~ w+ $$ t t~ H QED=3 [QCD] @0", "p p > ell- vl~ w+ j $$ t t~ H QED=3 [QCD] @1"],
        "WpZToLpNujj": ["p p > ell+ vl z $$ t t~ H QED=3 [QCD] @0", "p p > ell+ vl z j $$ t t~ H QED=3 [QCD] @1"],
        "WmZToLmNujj": ["p p > ell- vl~ z $$ t t~ H QED=3 [QCD] @0", "p p > ell- vl~ z j $$ t t~ H QED=3 [QCD] @1"],
        "ZWpToLpLmjj": ["p p > ell+ ell- w+ $$ t t~ H QED=3 [QCD] @0", "p p > ell+ ell- w+ j $$ t t~ H QED=3 [QCD] @1"],
        "ZWmToLpLmjj": ["p p > ell+ ell- w- $$ t t~ H QED=3 [QCD] @0", "p p > ell+ ell- w- j $$ t t~ H QED=3 [QCD] @1"],
    }
    ```

4. The `madspin_card.dat` decays the other boson to jets. The dict that we use for it is following:

    ```python
    DecayProcess = {
        "WpWmToLpNujj": "w- > q q",
        "WmWpToLmNujj": "w+ > q q",
        "WpZToLpNujj": "z > q q",
        "WmZToLmNujj": "z > q q",
        "ZWpToLpLmjj": "w+ > q q",
        "ZWmToLpLmjj": "w- > q q",
    }
    ```

    Furthermore, we chooose to include b in the quark definition when we have Z-boson. The dict for it is:

    ```python
    DecayProcessQuarkDefinition = {
        "WpWmToLpNujj": "define q = g u c d s u~ c~ d~ s~ ",
        "WmWpToLmNujj": "define q = g u c d s u~ c~ d~ s~ ",
        "WpZToLpNujj": "define q = g u c d s b u~ c~ d~ s~ b~",
        "WmZToLmNujj": "define q = g u c d s b u~ c~ d~ s~ b~",
        "ZWpToLpLmjj": "define q = g u c d s u~ c~ d~ s~ ",
        "ZWmToLpLmjj": "define q = g u c d s u~ c~ d~ s~ ",
    }
    ```

5. Finally, we need to update `cuts.f` to adapt for different mWV window, which is ["150to600", "600to800", "800toInf"].

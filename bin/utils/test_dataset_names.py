from check_dataset_names import *

test_cases = ["DYto2L-4Jets_Bin-MLL-4to10_TuneCP5_13p6TeV_madgraphMLM-pythia8",
              "DYto2L-4Jets_Bin-MLL-10to50_TuneCP5_13p6TeV_madgraphMLM-pythia8",
              "DYto2L-2Jets_Bin-MLL-4to10_TuneCP5_13p6TeV_amcatnloFXFX-pythia8",
              "DYto2L-2Jets_Bin-MLL-10to50_TuneCP5_13p6TeV_amcatnloFXFX-pythia8",
              "DYto2L-4Jets_Bin-MLL-50_TuneCP5_13p6TeV_madgraphMLM-pythia8",
              "DYto2L-2Jets_Bin-MLL-50_TuneCP5_13p6TeV_amcatnloFXFX-pythia8",
              "DYto2L-4Jets_Bin-0J-MLL-50_TuneCP5_13p6TeV_madgraphMLM-pythia8",
              "DYto2L-4Jets_Bin-1J-MLL-50_TuneCP5_13p6TeV_madgraphMLM-pythia8",
              "DYto2L-4Jets_Bin-2J-MLL-50_TuneCP5_13p6TeV_madgraphMLM-pythia8",
              "DYto2L-4Jets_Bin-3J-MLL-50_TuneCP5_13p6TeV_madgraphMLM-pythia8",
              "DYto2L-4Jets_Bin-4J-MLL-50_TuneCP5_13p6TeV_madgraphMLM-pythia8",
              "DYto2L-2Jets_Bin-0J-MLL-50_TuneCP5_13p6TeV_amcatnloFXFX-pythia8",
              "DYto2L-2Jets_Bin-1J-MLL-50_TuneCP5_13p6TeV_amcatnloFXFX-pythia8",
              "DYto2L-2Jets_2J-MLL-50_TuneCP5_13p6TeV_amcatnloFXFX-pythia8", #THIS IS NOT A VALID NAME
              "DYto2L-2Jets_Bin-2J-MLL-50_TuneCP5_13p6TeV_amcatnloFXFX-pythia8"
              ]

# Validate the test cases
for name in test_cases:
    valid, message, feedback = validate_dataset_name(name)
    print(f"Dataset: {name} -> {message}")
    if not valid:
        for item in feedback:
            print(f"  - {item}")

    print("\n")


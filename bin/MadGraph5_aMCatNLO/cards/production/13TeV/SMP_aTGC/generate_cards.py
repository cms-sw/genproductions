# Before run set the LCG environment:
# . /cvmfs/sft.cern.ch/lcg/views/LCG_106/x86_64-el9-gcc13-opt/setup.sh
import os
from shutil import copyfile

# Configuration
template_dir = "CardsTemplates"
output_base_dir = "GeneratedCards"

sample_names = ["WpWmToLpNujj", "WmWpToLmNujj", "WpZToLpNujj", "WmZToLmNujj", "ZWpToLpLmjj", "ZWmToLpLmjj"]
mWW_window = ["150to600", "600to800", "800toInf"]
which_boson = ["W", "Z"]

processes = {
    "WpWmToLpNujj": ["p p > ell+ vl w- $$ t t~ H QED=3 [QCD] @0", "p p > ell+ vl w- j $$ t t~ H QED=3 [QCD] @1"],
    "WmWpToLmNujj": ["p p > ell- vl~ w+ $$ t t~ H QED=3 [QCD] @0", "p p > ell- vl~ w+ j $$ t t~ H QED=3 [QCD] @1"],
    "WpZToLpNujj": ["p p > ell+ vl z $$ t t~ H QED=3 [QCD] @0", "p p > ell+ vl z j $$ t t~ H QED=3 [QCD] @1"],
    "WmZToLmNujj": ["p p > ell- vl~ z $$ t t~ H QED=3 [QCD] @0", "p p > ell- vl~ z j $$ t t~ H QED=3 [QCD] @1"],
    "ZWpToLpLmjj": ["p p > ell+ ell- w+ $$ t t~ H QED=3 [QCD] @0", "p p > ell+ ell- w+ j $$ t t~ H QED=3 [QCD] @1"],
    "ZWmToLpLmjj": ["p p > ell+ ell- w- $$ t t~ H QED=3 [QCD] @0", "p p > ell+ ell- w- j $$ t t~ H QED=3 [QCD] @1"],
}

DecayProcessQuarkDefinition = {
    "WpWmToLpNujj": "define q = g u c d s u~ c~ d~ s~ ",
    "WmWpToLmNujj": "define q = g u c d s u~ c~ d~ s~ ",
    "WpZToLpNujj": "define q = g u c d s b u~ c~ d~ s~ b~",
    "WmZToLmNujj": "define q = g u c d s b u~ c~ d~ s~ b~",
    "ZWpToLpLmjj": "define q = g u c d s u~ c~ d~ s~ ",
    "ZWmToLpLmjj": "define q = g u c d s u~ c~ d~ s~ ",
}

DecayProcess = {
    "WpWmToLpNujj": "w- > q q",
    "WmWpToLmNujj": "w+ > q q",
    "WpZToLpNujj": "z > q q",
    "WmZToLmNujj": "z > q q",
    "ZWpToLpLmjj": "w+ > q q",
    "ZWmToLpLmjj": "w- > q q",
}


# Ensure the output base directory exists
os.makedirs(output_base_dir, exist_ok=True)

# Function to replace placeholders in a file
def replace_placeholders(template_file, output_file, replacements):
    with open(template_file, 'r') as file:
        content = file.read()
    for placeholder, replacement in replacements.items():
        content = content.replace(placeholder, replacement)
    with open(output_file, 'w') as file:
        file.write(content)

# Function to determine `cuts.f` logic
def get_cuts_logic(mWW):
    if mWW == "150to600":
        return {"mWVCUTLOW": "lt.150", "mWVCUTHIGH": "gt.600"}
    elif mWW == "600to800":
        return {"mWVCUTLOW": "le.600", "mWVCUTHIGH": "gt.800"}
    elif mWW == "800toInf":
        return {"mWVCUTLOW": "le.800", "mWVCUTHIGH": None}  # No upper cut
    else:
        raise ValueError(f"Invalid mWW window: {mWW}")

# Generate directories and files
for sample in sample_names:
    for mWW in mWW_window:
        directory_name = f"{sample}_01j_aTGC_pT{'W' if 'Wm' in sample or 'Wp' in sample else 'Z'}-150toInf_mWV-{mWW}_4f_NLO_FXFX"
        directory_path = os.path.join(output_base_dir, directory_name)
        os.makedirs(directory_path, exist_ok=True)

        # Copy and process each template file
        for file_name in os.listdir(template_dir):
            template_file = os.path.join(template_dir, file_name)
            output_file = os.path.join(directory_path, f'{directory_name}_{file_name}')

            if file_name == "proc_card.dat":
                # Process proc_card.dat
                replacements = {
                    "PROCESS1": processes[sample][0],
                    "PROCESS2": processes[sample][1],
                    "DIRNAME": directory_name,
                }
                replace_placeholders(template_file, output_file, replacements)
            elif file_name == "madspin_card.dat":
                # Process madspin_card.dat
                replacements = {
                    "DECAYPROCESS": DecayProcess[sample],
                    "DEFINEQUARKS": DecayProcessQuarkDefinition[sample],
                }
                replace_placeholders(template_file, output_file, replacements)
            elif file_name == "cuts.f":
                # # Process cuts.f
                # low, high = (mWW.split("to") + [None])[:2]

                # replacements = {
                #     "mWVCUTLOW": low,
                #     "mWVCUTHIGH": high,
                # }
                # remove_high_cut = "Inf" in mWW
                # replace_placeholders(template_file, output_file, replacements, remove_high_cut=remove_high_cut)
                # Process cuts.f
                cuts_logic = get_cuts_logic(mWW)
                if cuts_logic["mWVCUTHIGH"] is None:
                    # Remove the high cut
                    with open(template_file, 'r') as file:
                        lines = file.readlines()
                    # Remove the line with "mWVCUTHIGH" and the 3 lines above it
                    for i in range(len(lines)):
                        if "mWVCUTHIGH" in lines[i]:
                            lines = lines[:i-3] + lines[i+1:]
                            break
                    with open(output_file, 'w') as file:
                        file.writelines(lines)
                    # Replace the low cut
                    with open(output_file, 'r') as file:
                        content = file.read().replace("mWVCUTLOW", cuts_logic["mWVCUTLOW"])
                    with open(output_file, 'w') as file:
                        file.write(content)
                else:
                    # Replace both cuts
                    replace_placeholders(template_file, output_file, cuts_logic)

            else:
                # Copy other files as-is
                copyfile(template_file, output_file)

print(f"All directories and files have been generated in '{output_base_dir}'.")

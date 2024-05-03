import os
import shutil

# Define the source directory where the original cards are located
src_dir = './originalCards'

# List of original card files
card_files = [
    'tHq_4f_NLO_M125_CP_customizecards.dat',
    'tHq_4f_NLO_M125_CP_extramodels.dat',
    'tHq_4f_NLO_M125_CP_madspin_card.dat',
    'tHq_4f_NLO_M125_CP_proc_card.dat',
    'tHq_4f_NLO_M125_CP_run_card.dat'
]

# Define the base directory where the new folders will be created
base_dir = './'

customize_card_changes = {
    'k1_ktilde0':       "\nset param_card kHtt 1.0 \nset param_card kAtt 0.0\nset param_card cosa 1.0",
    'km1_ktilde0':      "\nset param_card kHtt -1.0 \nset param_card kAtt 0.0\nset param_card cosa 1.0",
    'k0_ktilde0':       "\nset param_card kHtt 0.0001 \nset param_card kAtt 0.0\nset param_card cosa 1.0",
    'k0_ktilde1':       "\nset param_card kHtt 0.0 \nset param_card kAtt 1.0\nset param_card cosa 0.0001 \nset param_card kSM 10000",
    'k0p7_ktilde0p7':   "\nset param_card kHtt 1.0 \nset param_card kAtt 1.0\nset param_card cosa 0.7071 \nset param_card kSM 1.4142",
    'k0p7_ktildem0p7':  "\nset param_card kHtt 1.0 \nset param_card kAtt -1.0\nset param_card cosa 0.7071 \nset param_card kSM 1.4142",
    'k0_ktildem1':      "\nset param_card kHtt 0.0 \nset param_card kAtt -1.0 \nset param_card cosa 0.0001 \nset param_card kSM 10000",
}


# Function to copy the original card files to a new folder for a given parameter configuration and decay channel
def write_cards(parameter_point, decay_channel, madspin_input):

    new_dir = os.path.join(base_dir, f'{decay_channel}/{parameter_point}')
    os.makedirs(new_dir, exist_ok=True)

    # Copy all card files to the new directory
    for card_file in card_files:
        src_file = os.path.join(src_dir, card_file)
        card_name = card_file.replace("CP", f"{decay_channel}_{parameter_point}")
        dst_file = os.path.join(new_dir, card_name)
        shutil.copy(src_file, dst_file)

        # make additions to customizecard for each parameter point
        if 'customizecards' in card_file:
            with open(dst_file, 'a') as f:
                f.write(customize_card_changes[parameter_point])

        # write gridpack name in output command
        elif 'proc_card' in card_file:
            with open(dst_file, 'a') as f:
                out_line = card_name.replace("_proc_card.dat", "")
                output_command = f"\n\noutput {out_line} -nojpeg"
                f.write(output_command)

        # define decay channel
        elif 'madspin_card' in card_file and decay_channel:
            with open(dst_file, 'r') as f:
                lines = f.readlines()
            with open(dst_file, 'w') as f:
                for line in lines:
                    if '<my decays>' in line:  # Placeholder line to be replaced
                        f.write(madspin_input)
                    else:
                        f.write(line)


madspin_input_leptonic = "decay t > w+ b, w+ > ell+ vl \ndecay t~ > w- b~, w- > ell- vl~"
madspin_input_hadronic = "decay t > w+ b, w+ > had had \ndecay t~ > w- b~, w- > had had"

# Create new folders and copy cards for each parameter point and decay type
for parameterPoint in [*customize_card_changes]:

    write_cards(parameterPoint, 'TLep', madspin_input_leptonic)

    write_cards(parameterPoint, 'THad', madspin_input_hadronic)
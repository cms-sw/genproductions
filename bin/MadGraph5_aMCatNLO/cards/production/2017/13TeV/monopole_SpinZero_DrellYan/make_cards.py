"""
Script for Generating Configuration Files for Particle Simulations
Owner: Matheus Macedo, UERJ-RIO / Brazil

This script is designed to automate the creation of configuration files for particle simulation studies, focusing on different masses of particles with spin zero. It systematically generates a structured directory with necessary configuration files for each specified particle mass. These include:
1. 'customizecards.dat' for setting specific particle mass, decay parameters, and coupling constants.
2. 'proc_card.dat' for specifying the simulation process using the imported model.
3. 'run_card.dat' for defining runtime parameters, copied from a template if it exists.

How to Use:
- Make sure Python is installed on your computer.
- Copy this script into a Python file, e.g., 'make_cards.py'.
- Execute the script from your terminal or command prompt by running 'python generate_spinzero_configs.py'.
- The script will generate a 'cards' directory in your current working directory with subdirectories for each mass value, each containing the required configuration files.

"""

import os

# Defines a list of particle masses for which to generate configuration files.
masses = [1000, 1500, 2000, 2500, 3000, 3400, 3500, 3600, 3700, 3800, 3900, 4000, 4100, 4200, 4300, 4400, 4500]

# Creates a main directory called 'cards' if it does not already exist. 
os.makedirs("cards", exist_ok=True)

# Iterates through each mass in the list to create specific directories and configuration files.
for mass in masses:
    # Constructs a directory path for each mass and creates the directory.
    output_card_dir = f"cards/SpinZero_DrellYan_{mass}"
    os.makedirs(output_card_dir, exist_ok=True)

    # Prepares the path for 'customizecards.dat' and writes configuration settings to it.
    customizecards_path = f"{output_card_dir}/monopole_SpinZero_DrellYan_M{mass}_customizecards.dat"
    with open(customizecards_path, 'w') as customizecards_file:
        customizecards_file.write(f"set param_card mass 4110000 {mass}\n")  # Sets the mass of the particle.
        customizecards_file.write("set param_card mass 25 125\n")  # Sets the Higgs boson mass, for example.
        customizecards_file.write("set param_card decay 4110000 0.000000e+0\n")  # Defines the particle's decay.
        customizecards_file.write("set param_card gch 1 1.0\n")  # Sets a coupling constant.

    # Generates 'proc_card.dat' with the model import and process generation commands.
    proc_card_content = f"""import model mono_spinzero
generate p p > a > mm+ mm-
output monopole_SpinZero_DrellYan_M{mass} -nojpeg
"""
    proc_card_path = f"{output_card_dir}/monopole_SpinZero_DrellYan_M{mass}_proc_card.dat"
    with open(proc_card_path, 'w') as proc_card_file:
        proc_card_file.write(proc_card_content)

    # Checks for the existence of a template 'run_card.dat'. If found, copies its content into the new directory.
    run_card_input_path = "monopole_SpinZero_DrellYan_run_card.dat"
    run_card_output_path = f"{output_card_dir}/monopole_SpinZero_DrellYan_M{mass}_run_card.dat"
    if os.path.exists(run_card_input_path):
        with open(run_card_input_path, 'r') as run_card_input_file, open(run_card_output_path, 'w') as run_card_output_file:
            for line in run_card_input_file:
                run_card_output_file.write(line)
            run_card_output_file.write("\n")  # Ensures an extra newline at the end for file integrity.
    else:
        # Creates an empty 'run_card.dat' if the template is not found.
        open(run_card_output_path, 'a').close()



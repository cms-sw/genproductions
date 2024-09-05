"""
Owner: Matheus Macedo, UERJ-RIO / Brazil

This script automates the generation of configuration files for particle simulation processes, specifically focusing on simulations of different particle masses. It creates a directory structure and files necessary for each mass value specified in the 'masses' list. The script generates three main types of files for each mass:
1. 'customizecards.dat' for setting particle mass and decay parameters.
2. 'proc_card.dat' for specifying the simulation process and model.
3. 'run_card.dat' for simulation run configurations, copied from a template if available.

How to Use:
- Ensure Python is installed on your system.
- Save this script in your desired directory.
- Run the script using Python (e.g., 'python3 make_cards.py').
- The script will create a 'cards' directory with subdirectories and files for each mass.
"""

import os

# List of particle masses for which configuration files will be generated.
masses = [1000, 1500, 2000, 2500, 3000, 3500, 3600, 3700, 3800, 3900, 4000, 4100, 4200, 4300, 4400, 4500]

# Creates the main 'cards' directory if it does not exist. 'exist_ok=True' prevents error if the directory already exists.
os.makedirs("cards", exist_ok=True)

# Iterates over each mass value in the list.
for mass in masses:
    # Creates a specific directory for each mass inside the 'cards' directory.
    output_card_dir = f"cards/SpinHalf_DrellYan_{mass}"
    os.makedirs(output_card_dir, exist_ok=True)

    # Creates and writes to 'customizecards.dat' file with specific simulation configuration for the mass.
    customizecards_path = f"{output_card_dir}/monopole_SpinHalf_DrellYan_M{mass}_customizecards.dat"
    with open(customizecards_path, 'w') as customizecards_file:
        customizecards_file.write(f"set param_card mass 4110000 {mass}\n")  # Sets the mass of the particle.
        customizecards_file.write("set param_card mass 25 125\n")  # Sets the mass of the Higgs boson (example).
        customizecards_file.write("set param_card decay 4110000 0.000000e+0\n")  # Sets the decay of the particle.
        customizecards_file.write("set param_card gch 1 1.0\n")  # Sets a generic coupling parameter.

    # Creates and writes to 'proc_card.dat' file with commands for generating the simulation process.
    proc_card_content = f"""import model mono_spinhalf
generate p p > a > mm+ mm-
output monopole_SpinHalf_DrellYan_M{mass} -nojpeg
"""
    proc_card_path = f"{output_card_dir}/monopole_SpinHalf_DrellYan_M{mass}_proc_card.dat"
    with open(proc_card_path, 'w') as proc_card_file:
        proc_card_file.write(proc_card_content)

    # Checks for the existence of 'run_card.dat'. If it exists, copies its content to the new directory.
    run_card_input_path = "monopole_SpinHalf_DrellYan_run_card.dat"
    run_card_output_path = f"{output_card_dir}/monopole_SpinHalf_DrellYan_M{mass}_run_card.dat"
    if os.path.exists(run_card_input_path):
        with open(run_card_input_path, 'r') as run_card_input_file, open(run_card_output_path, 'w') as run_card_output_file:
            for line in run_card_input_file:
                run_card_output_file.write(line)
            run_card_output_file.write("\n")  # Adds an extra newline for safety.
    else:
        # If 'run_card.dat' does not exist, creates an empty file.
        open(run_card_output_path, 'a').close()





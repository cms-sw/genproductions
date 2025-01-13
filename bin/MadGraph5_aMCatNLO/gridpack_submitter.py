import os
import argparse

def execute_commands(directories_list, names_list):
    '''
    Executes a series of commands based on the provided parameters.
    
    Parameters:
        jsons_list (list): List of jsons to loop over.
        campaign (str): The campaign name.
        year (str): The year.
        executor (str): Which executor.
        overwrite (str): The overwrite flag.
        skipbadfiles (str): The Skipbadfiles flag.
        validate (str): To run the file validation.
    '''
    for i, directory in enumerate(directories_list):
        directory = directory
        name = names_list[i]
        command = (
            f'./submit_gridpack_generation.sh 15000 15000 2nw {name} {directory} 8nh'
        )
        print(f'Executing: {command}')
        #os.system(command)


if __name__ == '__main__':

    #### Set mass ranges to produce ####
    masses = [400, 600, 1000, 1600, 2000, 2600, 3000, 3600, 4000, 4600, 5000, 5600, 6000, 6600]
    #### Set couplings to produce ####
    couplings = ['0p01', '0p1', '1p0', '2p0', '3p0', '5p0']
    directories_list = []
    names_list = []
    for mass in masses:
        for coupling in couplings:
            name = f'WprimetoENu_Par-kR-{coupling}-M-{mass}_NNPDF31nnlo'
            directory = f'cards/production/13p6TeV/WprimetoENu_2024/WprimetoENu_Par-kR-{coupling}-M-{mass}_NNPDF31nnlo'
            names_list.append(name)
            directories_list.append(directory)

    # Execute commands
    execute_commands(directories_list, names_list)

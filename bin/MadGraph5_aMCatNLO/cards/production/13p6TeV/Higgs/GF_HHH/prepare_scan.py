# Script to prepare the cross-section scans of lambda3 and lambda4
# author Marko Stamenkovic

# import 

import os

# global variables


customize_card = '_customizecards.dat'
proc_card = '_proc_card.dat'
run_card = '_run_card.dat'
extramodels_card = '_extramodels.dat'

production = 'GF_HHH'
reference = '%s_c3_0_d4_0'%production

path_to_ref = reference


# main

k3_scan = [0,1,2,3,-0.5, 5,10, 20]
c3_scan = [el - 1 for el in k3_scan]

k4_scan = [0,1,0.5,3,0.5,10, 20, 50, 100]
d4_scan = [el -1 for el in k4_scan]

scans = [[0., -1.],  # [c3, d4]
         [0., 99.],
         [-1., 0.],
         [-1., 0.],
         [1., 0.],
         [-1., -1.],
         [1., 2.],
         [-1.5, -0.5],
         [19., 19.],
         [2., -1.],
         [4., 9.], 
        ]
for scan in scans:
    c3 = scan[0]
    d4 = scan[1]
    new_scan = '%s_c3_%.3f_d4_%.3f'%(production,c3,d4)
    new_dir = new_scan

    # create dir

    if not os.path.isdir(new_dir):
        os.makedirs(new_dir)

    # read file
    # start with customize
    with open(path_to_ref + '/' + reference + customize_card, 'r') as f:
        inp = f.read() 

    inp = inp.replace('set param_card tripcoup 4 0','set param_card tripcoup 4 %.3f'%c3)
    inp = inp.replace('set param_card quartcoup 6 0','set param_card quartcoup 6 %.3f'%d4)

    #print('set param_card tripcoup 4 %.3f'%c3)
    #print('set param_card quartcoup 6 %.3f'%d4)

    with open(new_dir + '/' + new_scan + customize_card, 'w') as f:
        f.write(inp)

    # proc
    with open(path_to_ref + '/' + reference + proc_card, 'r') as f:
        inp = f.read() 

    inp = inp.replace(reference, new_scan)

    with open(new_dir + '/' + new_scan + proc_card, 'w') as f:
        f.write(inp)

    # run
    with open(path_to_ref + '/' + reference + run_card,'r') as f:
        inp = f.read() 

    inp = inp.replace(reference, new_scan)

    with open(new_dir + '/' + new_scan + run_card, 'w') as f:
        f.write(inp)

    # extramodels
    with open(path_to_ref + '/' + reference + extramodels_card,'r') as f:
        inp = f.read() 

    inp = inp.replace(reference, new_scan)

    with open(new_dir + '/' + new_scan + extramodels_card, 'w') as f:
        f.write(inp)



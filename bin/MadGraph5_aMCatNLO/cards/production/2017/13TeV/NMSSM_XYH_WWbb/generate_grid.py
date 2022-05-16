import os

# the prototype name of the production folder
dir_proto = "NMSSM_XYH_WWbb_MX_{0}_MY_{1}"

# quantities to be replaced:
#	TEMPLATEMH03 [mX]
#	TEMPLATEMH02 [mY]

def change_cards(cardname, replacements):

    ## first make a backup copy
    bkpname = cardname + '.bak'
    os.system('mv %s %s' % (cardname, bkpname))

    # edit the file
    fin  = open(bkpname, 'r')
    fout = open(cardname, 'w')

    for line in fin:
        for key, rep in replacements.items():
            line = line.replace(key, rep)
        fout.write(line)

    fin.close()
    fout.close()

    ## delete the backup file
    os.system('rm %s' % bkpname)


def do_point(mx, my):
    # 1 - create the folder
    folder = dir_proto.format(mx, my)
    if os.path.isdir(folder):
        print(" >> folder "+ folder+ " already existing, forcing its deletion")
	os.system('rm -r %s' % folder)
    os.system('mkdir ' + folder)

    # 2 -copy the original files
    template_flrd = 'Template'

    run_card      = 'run_card.dat'
    proc_card     = 'proc_card.dat'
    extramodels   = 'extramodels.dat'
    customizecard = 'customizecards.dat'

    to_copy = [run_card, proc_card, extramodels, customizecard]

    for tc in to_copy:
	os.system('cp %s/%s %s/%s_%s' % (template_flrd, tc, folder, folder, tc) )

    replacements = {
        'TEMPLATEMH03' : str(mx),
        'TEMPLATEMH02' : str(my),
    }

    # 3 - edit in place the cards
    change_cards('%s/%s_%s' % (folder, folder, customizecard), replacements)
    change_cards('%s/%s_%s' % (folder, folder, proc_card), replacements)

def grid_to_points(grid):
    '''
    grid [dict] = grid dictionary containing mass points in the form {MX : [MY]}
    returns list of (MX, MY) tuples 
    '''
    return [(xmass,ymass) for xmass,ymasses in grid.items() for ymass in ymasses]

####################################################################################

# Mass points (MX : [MY])
GRID = {
    "1300": [200],
    "1500": [400],
    "2000": [400,800],
    "3000": [800]
}

if __name__ == "__main__":
    points = grid_to_points(GRID)
    for p in points:
	print('... generating {}'.format(p))
	do_point(*p)


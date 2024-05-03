import os

# the prototype name of the production folder
prod_proto = "NMSSM_XYH_WWWW_MX_{0}_MY_{1}_MH_{2}"

### things to replace are
### TEMPLATEMH03 [mX]
### TEMPLATEMH02 [mY]
### TEMPLATEMH01 [mH]

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


def do_point(mx, my, mH):
    # 1 - create the folder
    print(mx, my, mH)

    folder = prod_proto.format(mx, my, mH)
    if os.path.isdir(folder):
        print " >> folder", folder, "already existing, forcing its deletion"
        os.system('rm -r %s' % folder)
    os.system('mkdir ' + folder)
    
    # 2 - copy the original files
    template_flrd = 'Template'
    
    run_card      = 'run_card.dat'
    proc_card     = 'proc_card.dat'
    # param_card    = 'param_card.dat'
    extramodels   = 'extramodels.dat'
    customizecard = 'customizecards.dat'
    
    # to_copy = [run_card, proc_card, param_card, extramodels, customizecard]
    to_copy = [run_card, proc_card, extramodels, customizecard]

    for tc in to_copy:
        os.system('cp %s/%s %s/%s_%s' % (template_flrd, tc, folder, folder, tc) )

    replacements = {
        'TEMPLATEMH03' : str(mx),
        'TEMPLATEMH02' : str(my),
        'TEMPLATEMH01' : str(mH),
    }

    # 3 - edit in place the cards
    # change_cards('%s/%s_%s' % (folder, folder, param_card), replacements)
    change_cards('%s/%s_%s' % (folder, folder, customizecard), replacements)
    change_cards('%s/%s_%s' % (folder, folder, proc_card), replacements)


####################################################################################

## mX, mY
MassesX = [2000, 3000, 5000]
MassesYZ = [ (170,170), (170, 400), (400, 400)]

for mx in MassesX:
    for my,mH in MassesYZ:
        print '... generating', mx, my, mH
        do_point(mx,my,mH)

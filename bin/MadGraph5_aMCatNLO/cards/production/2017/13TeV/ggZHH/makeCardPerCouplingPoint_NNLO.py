import os

# the prototype name of the production folder
prod_proto = "{0}_CV_{1}_C2V_{2}_C3_{3}_KF_{4}_13TeV-madgraph-NLO"

def change_cards(cardname, replacements):
    
    ## first make a backup copy
    bkpname = cardname + '.bak'
    print cardname, bkpname
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


def do_point(vhh, cv, c2v, kL, kF):
    # 1 - create the folder
    folder = prod_proto.format(vhh, str(cv).replace(".","p"), 
        str(c2v).replace(".","p"), str(kL).replace(".","p"), str(kF).replace(".","p"))
    if os.path.isdir(folder):
        print " >> folder", folder, "already existing, forcing its deletion"
        os.system('rm -r %s' % folder)
    os.system('mkdir ' + folder)
    
    # 2 - copy the original files
    templateFolder = 'Templates_NNLO'
    
    run_card      = 'run_card.dat'
    proc_card     = vhh+'_proc_card.dat'
    # param_card    = 'param_card.dat'
    extramodels   = 'extramodels.dat'
    #customizecard = 'customizecards.dat'
    
    # to_copy = [run_card, proc_card, param_card, extramodels, customizecard]
    #to_copy = [run_card, proc_card, extramodels, customizecard]
    to_copy = [run_card, proc_card, extramodels]

    for fileName in to_copy:
        os.system('cp %s/%s %s/%s_%s' % (templateFolder, fileName, folder, folder, fileName.split(vhh+"_")[-1] ))

    replacementNumbers = {
        'TEMPCV'  : str(cv),
        'TEMPC2V' : str(c2v),
        'TEMPKL'  : str(kL),
        'TEMPKF'  : str(kF),
    }

    replacementStrings = {
        'TEMPCV'  : str(cv).replace(".","p"),
        'TEMPC2V' : str(c2v).replace(".","p"),
        'TEMPKL'  : str(kL).replace(".","p"),
        'TEMPKF'  : str(kF).replace(".","p"),
    }

    # 3 - edit in place the cards
    #change_cards('%s/%s_%s' % (folder, folder, customizecard), replacementNumbers)
    change_cards('%s/%s_%s' % (folder, folder, proc_card.split(vhh+"_")[-1]), replacementStrings)


####################################################################################

## cv, c2v, kL
points = [
#    (1, 1, 1,1),  # SM already in
    (0.5, 1, 0, 0),
    (2, 10, 5, 0),
    (0, 0, 5, 1),
    (0, 10, 20, 1),
    (0, 5, 20, 1),
    (0.5, 0, 20, 1),
    (0.5, 0, 5, 1),
    (0.5, 1, 20, 1),
    (0.5, 2, 20, 1),
    (1, 0, 20, 1),
    (1, 2, 0, 1),
    (1, 0, 0, 1),
    (2, 5, 5, 1),
    (2, 10, 5, 1),
    (2, 10, 0, 1),
    (2, 10, 1, 1),
    (0, 5, 20, 3),
    (0, 1, 5, 3),
    (0, 2, 2, 3),
    (0, 5, 5, 3),
    (2, 0, 1, 3),
    (2, 0, 2, 3),
    (1, 0, 20, 3),
]

for p in points:
    print '... generating', p
    do_point("ggZHH",*p)

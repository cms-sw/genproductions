import os

# the prototype name of the production folder
prod_proto = "{0}_CV_{1}_C2V_{2}_C3_{3}_13TeV-madgraph"

### things to replace are
### TEMPLATEMH02 [mX]
### TEMPLATEMH03 [mY]

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


def do_point(vhh, cv, c2v, kL):
    # 1 - create the folder
    folder = prod_proto.format(vhh, str(cv).replace(".","_"), 
        str(c2v).replace(".","_"), str(kL).replace(".","_"))
    if os.path.isdir(folder):
        print " >> folder", folder, "already existing, forcing its deletion"
        os.system('rm -r %s' % folder)
    os.system('mkdir ' + folder)
    
    # 2 - copy the original files
    templateFolder = 'Templates'
    
    run_card      = 'run_card.dat'
    proc_card     = vhh+'_proc_card.dat'
    # param_card    = 'param_card.dat'
    extramodels   = 'extramodels.dat'
    customizecard = 'customizecards.dat'
    
    # to_copy = [run_card, proc_card, param_card, extramodels, customizecard]
    to_copy = [run_card, proc_card, extramodels, customizecard]

    for fileName in to_copy:
        os.system('cp %s/%s %s/%s_%s' % (templateFolder, fileName, folder, folder, fileName.split(vhh+"_")[-1] ))

    replacementNumbers = {
        'TEMPCV'  : str(cv),
        'TEMPC2V' : str(c2v),
        'TEMPKL'  : str(kL),
    }

    replacementStrings = {
        'TEMPCV'  : str(cv).replace(".","_"),
        'TEMPC2V' : str(c2v).replace(".","_"),
        'TEMPKL'  : str(kL).replace(".","_"),
    }

    # 3 - edit in place the cards
    change_cards('%s/%s_%s' % (folder, folder, customizecard), replacementNumbers)
    change_cards('%s/%s_%s' % (folder, folder, proc_card.split(vhh+"_")[-1]), replacementStrings)


####################################################################################

## cv, c2v, kL
points = [
    (0.5, 1.0, 1.0),
    (1.0, 0.0, 1.0),
    (1.0, 1.0, 0.0),
    (1.0, 1.0, 1.0),
    (1.0, 2.0, 1.0),
    (1.0, 1.0, 2.0),
    (1.5, 1.0, 1.0),
    (1.0, 10.0, 1.0),
    (1.0, 1.0, 20.0)
]


for p in points:
    print '... generating', p
    do_point("ZHH",*p)
    do_point("WHH",*p)

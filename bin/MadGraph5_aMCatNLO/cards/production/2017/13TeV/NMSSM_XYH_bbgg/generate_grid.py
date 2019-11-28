import os

# the prototype name of the production folder
prod_proto = "NMSSM_XYH_bbgg_MX_{0}_MY_{1}"

### things to replace are
### TEMPLATEMH02 [mX]
### TEMPLATEMH03 [mY]

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
    folder = prod_proto.format(mx, my)
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
    }

    # 3 - edit in place the cards
    # change_cards('%s/%s_%s' % (folder, folder, param_card), replacements)
    change_cards('%s/%s_%s' % (folder, folder, customizecard), replacements)
    change_cards('%s/%s_%s' % (folder, folder, proc_card), replacements)


####################################################################################

## mX, mY
points = [
    (300,60),
    (300,70),
    (300,80),
    (300,90),
    (300,100),
    (300,125),
    (300,150),
    (400,60),
    (400,70),
    (400,80),
    (400,90),
    (400,100),
    (400,125),
    (400,150),
    (400,200),
    (400,250),
    (500,60),
    (500,70),
    (500,80),
    (500,90),
    (500,100),
    (500,125),
    (500,150),
    (500,200),
    (500,250),
    (500,300),
    (600,60),
    (600,70),
    (600,80),
    (600,90),
    (600,100),
    (600,125),
    (600,150),
    (600,200),
    (600,250),
    (600,300),
    (600,400),
    (700,60),
    (700,70),
    (700,80),
    (700,90),
    (700,100),
    (700,125),
    (700,150),
    (700,200),
    (700,250),
    (700,300),
    (700,400),
    (700,500),
    (800,60),
    (800,70),
    (800,80),
    (800,90),
    (800,100),
    (800,125),
    (800,150),
    (800,200),
    (800,250),
    (800,300),
    (800,400),
    (800,500),
    (800,600),
    (900,60),
    (900,70),
    (900,80),
    (900,90),
    (900,100),
    (900,125),
    (900,150),
    (900,200),
    (900,250),
    (900,300),
    (900,400),
    (900,500),
    (900,600),
    (900,700),
    (1000,60),
    (1000,70),
    (1000,80),
    (1000,90),
    (1000,100),
    (1000,125),
    (1000,150),
    (1000,200),
    (1000,250),
    (1000,300),
    (1000,400),
    (1000,500),
    (1000,600),
    (1000,700),
    (1000,800),
    (1100,90),
    (1100,100),
    (1100,125),
    (1100,150),
    (1100,200),
    (1100,250),
    (1100,300),
    (1100,400),
    (1100,500),
    (1100,600),
    (1100,700),
    (1100,800),
    (1100,900),
    (1200,90),
    (1200,100),
    (1200,125),
    (1200,150),
    (1200,200),
    (1200,250),
    (1200,300),
    (1200,400),
    (1200,500),
    (1200,600),
    (1200,700),
    (1200,800),
    (1200,900),
    (1200,1000),
    (1400,90),
    (1400,100),
    (1400,125),
    (1400,150),
    (1400,200),
    (1400,250),
    (1400,300),
    (1400,400),
    (1400,500),
    (1400,600),
    (1400,700),
    (1400,800),
    (1400,900),
    (1400,1000),
    (1400,1200),
    (1600,90),
    (1600,100),
    (1600,125),
    (1600,150),
    (1600,200),
    (1600,250),
    (1600,300),
    (1600,400),
    (1600,500),
    (1600,600),
    (1600,700),
    (1600,800),
    (1600,900),
    (1600,1000),
    (1600,1200),
    (1600,1400),
    (1800,90),
    (1800,100),
    (1800,125),
    (1800,150),
    (1800,200),
    (1800,250),
    (1800,300),
    (1800,400),
    (1800,500),
    (1800,600),
    (1800,700),
    (1800,800),
    (1800,900),
    (1800,1000),
    (1800,1200),
    (1800,1400),
    (1800,1600),
    (2000,90),
    (2000,100),
    (2000,125),
    (2000,150),
    (2000,200),
    (2000,250),
    (2000,300),
    (2000,400),
    (2000,500),
    (2000,600),
    (2000,700),
    (2000,800),
    (2000,900),
    (2000,1000),
    (2000,1200),
    (2000,1400),
    (2000,1600),
    (2000,1800)
]

for p in points:
    print '... generating', p
    do_point(*p)

import os

# the prototype name of the production folder
prod_proto_Y_tautau = "NMSSM_XYH_Y_tautau_H_gg_MX_{0}_MY_{1}"
prod_proto_Y_gg = "NMSSM_XYH_Y_gg_H_tautau_MX_{0}_MY_{1}"

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


def do_point(mx, my, Y_decay):
    # 1 - create the folder
    if Y_decay == "tautau": prod_proto = prod_proto_Y_tautau
    else:                   prod_proto = prod_proto_Y_gg
    folder = prod_proto.format(mx, my)
    if os.path.isdir(folder):
        print " >> folder", folder, "already existing, forcing its deletion"
        os.system('rm -r %s' % folder)
    os.system('mkdir ' + folder)
    
    # 2 - copy the original files
    template_flrd = 'Template'
    
    run_card      = 'run_card.dat'
    # param_card    = 'param_card.dat'
    extramodels   = 'extramodels.dat'
    customizecard = 'customizecards.dat'
   
    if Y_decay == "tautau": proc_card = "proc_card_Y_tautau_H_gg.dat"
    else:                   proc_card = "proc_card_Y_gg_H_tautau.dat"
 
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

    # 4 - rename proc card
    os.system('mv %s/%s_%s %s/%s_proc_card.dat'%(folder, folder, proc_card, folder, folder))


####################################################################################

## mX, mY
#points = [
#    (500, 300),
#    (700, 400),
#]
Xs = [300,400,500,600,700,800,900,1000]
Ys = [90,100,125,150,200,250,300,400,500,600,700,800]
N_Ys = [4,6,7,8,9,10,11,12]

points = []
for i in range(len(Xs)):
    for j in range(N_Ys[i]):
        points.append((Xs[i],Ys[j]))

for p in points:
    print '... generating', p
    do_point(p[0], p[1], "tautau")
    do_point(p[0], p[1], "gg")

import os
import shutil
import fileinput

prodAddress='cards/production/13TeV/DarkMatter_Codex/'

run_card='Template/_run_card.dat'
proc_card='Template/_proc_card.dat'
extramodels='Template/_extramodels.dat'
customizecards='Template/_customizecards.dat'


def TriName(TriMass):
    DirName= 'Codex_LQ%s_DM_%s_X_%s_gen2'%(TriMass[0],TriMass[1],TriMass[2])
    AddressName=prodAddress+DirName
    return DirName,AddressName

def create_Directory(TriMass):
    DirName,AddressName=TriName(TriMass)
    if not os.path.exists(AddressName):
        print '>> file  "%s" does not exits, so it is now created' % AddressName
        os.makedirs(AddressName)


def create_run_card(TriMass):
    DirName,AddressName=TriName(TriMass)
    shutil.copy (run_card, AddressName+'/%s_run_card.dat'%DirName)
    if os.path.isfile (run_card):  print('\x1b[3;30;42m' + 'Success in making run_card%s!'%DirName + '\x1b[0m')

def create_extramodels(TriMass):
    DirName,AddressName=TriName(TriMass)
    shutil.copy (extramodels, AddressName+'/%s_extramodels.dat'%DirName)
    if os.path.isfile (extramodels):  print('\x1b[7;31;42m' + 'Success in making extramodels%s!'%DirName + '\x1b[0m')


def create_proc_card(TriMass):
    DirName,AddressName=TriName(TriMass)
    shutil.copy (proc_card,AddressName+'/%s_proc_card.dat'%DirName)
    if os.path.isfile (proc_card):  print('\x1b[6;31;40m' + 'Success in making proc_card%s!'%DirName + '\x1b[0m')
    ## replace the current directory address to the proper one
    for line in fileinput.input(AddressName+'/%s_proc_card.dat'%DirName,inplace=1):
        if 'Codex' in line:
            line=line.replace('Codex_LQ1000_DM_400_X_440_gen2',DirName)
        print(line.strip())

def create_customizecards_card(TriMass):
    DirName,AddressName=TriName(TriMass)
    shutil.copy (customizecards,AddressName+'/%s_customizecards.dat'%DirName)
    if os.path.isfile (customizecards):  print('\x1b[6;33;40m' + 'Success in making customizecards%s!'%DirName + '\x1b[0m')
    ## replace the current mass parameters to the proper ones
    for line in fileinput.input(AddressName+'/%s_customizecards.dat'%DirName,inplace=1):
        if 'frblock  1' in line:
            line=line.replace('4.000000e+02',TriMass[1])
        if 'frblock  2' in line:
            line=line.replace('4.400000e+02',TriMass[2])
        if 'frblock  3' in line:
            line=line.replace('4.400000e+02',TriMass[2])
        if 'frblock  4' in line:
            line=line.replace('1.000000e+03',TriMass[0])
        if 'frblock  5' in line:
            line=line.replace('1.000000e+03',TriMass[0])
        print(line.strip())


def create_gridpack_generation_script(FullTriMass):
    grdPack_Generate=open("create_gridpack_generation_script.sh","w")
    for TriMass in FullTriMass:
        DirName,AddressName=TriName(TriMass)
        grdPack_Generate.write('./gridpack_generation.sh  %s  %s  8nh\n'%(DirName,AddressName))
    grdPack_Generate.close()


def create_submit_gridpack_generation(FullTriMass):
    grdPack_Submit=open("create_submit_gridpack_generation.sh","w")
    for TriMass in FullTriMass:
        DirName,AddressName=TriName(TriMass)
        grdPack_Submit.write('./submit_gridpack_generation.sh 12000 12000  8nh  %s  %s  8nh\n'%(DirName,AddressName))
    grdPack_Submit.close()


def create_submit_gridpack_generation_lxbatch(FullTriMass):
    location = os.getcwd()
    selfBatchSubmit=open("create_SelfSubmitBatch.sh","w")
    for TriMass in FullTriMass:
        
        DirName,AddressName=TriName(TriMass)
        
        submitName=open('tosubmit_%s.sh'%DirName,"w")
        submitName.write('cd %s\n'%location)
        submitName.write('./gridpack_generation.sh  %s  %s  8nh\n'%(DirName,AddressName))
        submitName.close()

        selfBatchSubmit.write('bsub -q 8nh -J   %s < tosubmit_%s.sh \n'%(DirName,DirName))
    selfBatchSubmit.close()


def CheckList_GridPack(FullTriMass):
    checkList=open("create_ChechList.sh","w")
    for TriMass in FullTriMass:
        DirName,AddressName=TriName(TriMass)
        checkList.write('ls %s_tarball.tar.xz\n'%DirName)
    checkList.close()


FullTriMass=[
             ['600','200','220'],
             ['600','250','275'],
             ['700','200','220'],
             ['700','250','275'],
             ['700','300','330'],
             ['800','200','220'],
             ['800','250','275'],
             ['800','300','330'],
             ['800','350','385'],
             ['900','200','220'],
             ['900','250','275'],
             ['900','300','330'],
             ['900','350','385'],
             ['900','400','440'],
             ['1000','200','220'],
             ['1000','250','275'],
             ['1000','300','330'],
             ['1000','350','385'],
             ['1000','400','440'],
             ['1000','450','495'],
             ['1100','200','220'],
             ['1100','250','275'],
             ['1100','300','330'],
             ['1100','350','385'],
             ['1100','400','440'],
             ['1100','450','495'],
             ['1100','500','550'],
             ['1200','200','220'],
             ['1200','250','275'],
             ['1200','300','330'],
             ['1200','350','385'],
             ['1200','400','440'],
             ['1200','450','495'],
             ['1200','500','550'],
             ['1200','550','605'],
             ['1300','200','220'],
             ['1300','250','275'],
             ['1300','300','330'],
             ['1300','350','385'],
             ['1300','400','440'],
             ['1300','450','495'],
             ['1300','500','550'],
             ['1300','550','605'],
             ['1300','600','660'],
             ['1400','200','220'],
             ['1400','250','275'],
             ['1400','300','330'],
             ['1400','350','385'],
             ['1400','400','440'],
             ['1400','450','495'],
             ['1400','500','550'],
             ['1400','550','605'],
             ['1400','600','660'],
             ['1400','650','715'],
             ['1500','200','220'],
             ['1500','250','275'],
             ['1500','300','330'],
             ['1500','350','385'],
             ['1500','400','440'],
             ['1500','450','495'],
             ['1500','500','550'],
             ['1500','550','605'],
             ['1500','600','660'],
             ['1500','650','715'],
             ['1500','700','770'],

]



for TriMass in FullTriMass:
    create_Directory(TriMass)
    create_run_card(TriMass)
    create_extramodels(TriMass)
    create_proc_card(TriMass)
    create_customizecards_card(TriMass)
create_gridpack_generation_script(FullTriMass)
create_submit_gridpack_generation(FullTriMass)
create_submit_gridpack_generation_lxbatch(FullTriMass)
CheckList_GridPack(FullTriMass)




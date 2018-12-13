## Authors:
## Shu-Xiao Liu, Fasya Khuzaimah
## Department of Physics, National Central University

import os
import shutil

exe = 0 #exe=1 sub=0 to run locally in lxplus
sub = 1 #sub=1 exe=0 to run in batch jobs

def mkDir(dirName):
    if not os.path.isdir(dirName): os.mkdir(dirName)

def main():
    
    # first
    # unit of MZp is GeV
    MZpList = [100, 250, 500, 750, 1000, 1250, 1500, 2000, 2500, 3000, 3500]
    
    #unit of MChi is GeV
    mdmList = [1, 100]
    
    print "produce MZp list: ", MZpList
    print
    for MZp in MZpList:
        for mdm in mdmList:
        
            # ZpBaryonic for Z' Mass
            dirName = 'ZpBaryonic_MZp'+str(MZp)+'_MChi'+str(mdm)
        
            mkDir('cards/'+dirName)
            print('create '+dirName)
            shutil.copyfile('cards/ZpBaryonic_template/ZpBaryonic_MZp1000_MChi1_run_card.dat','cards/'+dirName+'/'+dirName+'_run_card.dat')
            shutil.copyfile('cards/ZpBaryonic_template/ZpBaryonic_MZp1000_MChi1_extramodels.dat','cards/'+dirName+'/'+dirName+'_extramodels.dat')
            
            f_proc0 = open('cards/ZpBaryonic_template/ZpBaryonic_MZp1000_MChi1_proc_card.dat','r')
            f_proc1 = open('cards/'+dirName+'/'+dirName+'_proc_card.dat','w')
            for line in f_proc0:
                f_proc1.write(line.replace('ZpBaryonic_MZp1000_MChi1',dirName))
            f_proc0.close()
            f_proc1.close()
            
            f_cust0 = open('cards/ZpBaryonic_template/ZpBaryonic_MZp1000_MChi1_customizecards.dat','r')
            f_cust1 = open('cards/'+dirName+'/'+dirName+'_customizecards.dat','w')
            for line in f_cust0:
                if line.find('MZp') > 0: f_cust1.write(line.replace('MZp',str(MZp)))
                elif line.find('mdm') > 0: f_cust1.write(line.replace('mdm',str(mdm)))
                else: f_cust1.write(line)

            f_cust0.close()
            f_cust1.close()
            command = './gridpack_generation.sh ' + dirName + ' cards/' + dirName
            subcom = './submit_gridpack_generation.sh 12000 12000 1nw ' + dirName + ' cards/' + dirName + ' 2nd'
            print(command)
            if (exe): os.system(command)
            if (sub): os.system(subcom)

    #Second
    # unit of MZp is GeV
    MZpList2 = [1000, 1500, 2000, 2500]

    #unit of MChi is GeV
    mdmList2 = [200, 400, 600, 800]

    print "produce MZp list2: ", MZpList2
    print
    for MZp2 in MZpList2:
        for mdm2 in mdmList2:
            
            # ZpBaryonic for Z' Mass
            dirName = 'ZpBaryonic_MZp'+str(MZp2)+'_MChi'+str(mdm2)

            mkDir('cards/'+dirName)
            print('create '+dirName)
            shutil.copyfile('cards/ZpBaryonic_template/ZpBaryonic_MZp1000_MChi1_run_card.dat','cards/'+dirName+'/'+dirName+'_run_card.dat')
            shutil.copyfile('cards/ZpBaryonic_template/ZpBaryonic_MZp1000_MChi1_extramodels.dat','cards/'+dirName+'/'+dirName+'_extramodels.dat')

            f_proc0 = open('cards/ZpBaryonic_template/ZpBaryonic_MZp1000_MChi1_proc_card.dat','r')
            f_proc1 = open('cards/'+dirName+'/'+dirName+'_proc_card.dat','w')
            for line in f_proc0:
                f_proc1.write(line.replace('ZpBaryonic_MZp1000_MChi1',dirName))
            f_proc0.close()
            f_proc1.close()

            f_cust0 = open('cards/ZpBaryonic_template/ZpBaryonic_MZp1000_MChi1_customizecards.dat','r')
            f_cust1 = open('cards/'+dirName+'/'+dirName+'_customizecards.dat','w')
            for line in f_cust0:
                if line.find('MZp2') > 0: f_cust1.write(line.replace('MZp2',str(MZp2)))
                elif line.find('mdm2') > 0: f_cust1.write(line.replace('mdm2',str(mdm2)))
                else: f_cust1.write(line)

            f_cust0.close()
            f_cust1.close()
            command = './gridpack_generation.sh ' + dirName + ' cards/' + dirName
            subcom = './submit_gridpack_generation.sh 12000 12000 1nw ' + dirName + ' cards/' + dirName + ' 2nd'
            print(command)
            if (exe): os.system(command)
            if (sub): os.system(subcom)

    #Third
    # unit of MZp is GeV
    MZpList3 = [500]

    #unit of MChi is GeV
    mdmList3 = [200, 400]

    print "produce MZp list3: ", MZpList3
    print
    for MZp3 in MZpList3:
        for mdm3 in mdmList3:

            # ZpBaryonic for Z' Mass
            dirName = 'ZpBaryonic_MZp'+str(MZp3)+'_MChi'+str(mdm3)

            mkDir('cards/'+dirName)
            print('create '+dirName)
            shutil.copyfile('cards/ZpBaryonic_template/ZpBaryonic_MZp1000_MChi1_run_card.dat','cards/'+dirName+'/'+dirName+'_run_card.dat')
            shutil.copyfile('cards/ZpBaryonic_template/ZpBaryonic_MZp1000_MChi1_extramodels.dat','cards/'+dirName+'/'+dirName+'_extramodels.dat')

            f_proc0 = open('cards/ZpBaryonic_template/ZpBaryonic_MZp1000_MChi1_proc_card.dat','r')
            f_proc1 = open('cards/'+dirName+'/'+dirName+'_proc_card.dat','w')
            for line in f_proc0:
                f_proc1.write(line.replace('ZpBaryonic_MZp1000_MChi1',dirName))
            f_proc0.close()
            f_proc1.close()

            f_cust0 = open('cards/ZpBaryonic_template/ZpBaryonic_MZp1000_MChi1_customizecards.dat','r')
            f_cust1 = open('cards/'+dirName+'/'+dirName+'_customizecards.dat','w')
            for line in f_cust0:
                if line.find('MZp3') > 0: f_cust1.write(line.replace('MZp3',str(MZp3)))
                elif line.find('mdm3') > 0: f_cust1.write(line.replace('mdm3',str(mdm3)))
                else: f_cust1.write(line)

            f_cust0.close()
            f_cust1.close()
            command = './gridpack_generation.sh ' + dirName + ' cards/' + dirName
            subcom = './submit_gridpack_generation.sh 12000 12000 1nw ' + dirName + ' cards/' + dirName + ' 2nd'
            print(command)
            if (exe): os.system(command)
            if (sub): os.system(subcom)

    #Fouth
    # unit of MZp is GeV
    MZpList4 = [3000]
                
    #unit of MChi is GeV
    mdmList4 = [200]

    print "produce MZp list4: ", MZpList4
    print
    for MZp4 in MZpList4:
        for mdm4 in mdmList4:

            # ZpBaryonic for Z' Mass
            dirName = 'ZpBaryonic_MZp'+str(MZp4)+'_MChi'+str(mdm4)

            mkDir('cards/'+dirName)
            print('create '+dirName)
            shutil.copyfile('cards/ZpBaryonic_template/ZpBaryonic_MZp1000_MChi1_run_card.dat','cards/'+dirName+'/'+dirName+'_run_card.dat')
            shutil.copyfile('cards/ZpBaryonic_template/ZpBaryonic_MZp1000_MChi1_extramodels.dat','cards/'+dirName+'/'+dirName+'_extramodels.dat')

            f_proc0 = open('cards/ZpBaryonic_template/ZpBaryonic_MZp1000_MChi1_proc_card.dat','r')
            f_proc1 = open('cards/'+dirName+'/'+dirName+'_proc_card.dat','w')
            for line in f_proc0:
                f_proc1.write(line.replace('ZpBaryonic_MZp1000_MChi1',dirName))
            f_proc0.close()
            f_proc1.close()

            f_cust0 = open('cards/ZpBaryonic_template/ZpBaryonic_MZp1000_MChi1_customizecards.dat','r')
            f_cust1 = open('cards/'+dirName+'/'+dirName+'_customizecards.dat','w')
            for line in f_cust0:
                if line.find('MZp4') > 0: f_cust1.write(line.replace('MZp4',str(MZp4)))
                elif line.find('mdm4') > 0: f_cust1.write(line.replace('mdm4',str(mdm4)))
                else: f_cust1.write(line)

            f_cust0.close()
            f_cust1.close()
            command = './gridpack_generation.sh ' + dirName + ' cards/' + dirName
            subcom = './submit_gridpack_generation.sh 12000 12000 1nw ' + dirName + ' cards/' + dirName + ' 2nd'
            print(command)
            if (exe): os.system(command)
            if (sub): os.system(subcom)

if __name__ == "__main__":
    main()
#os.system('cp *.tarball.tar.xz ~/public/gridpackDir')

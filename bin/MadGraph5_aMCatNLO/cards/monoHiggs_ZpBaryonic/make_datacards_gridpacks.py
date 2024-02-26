## written by Fasya Khuzaimah and Shu-Xiao Liu, slightly modified by Shivani Lomte

import os
import shutil

exe=0 #to produce cards only   
#exe{n}=1 #to run locally and generate gridpack
exe1=0
exe2=0
exe3=0
exe4=0

def mkDir(dirName):
    if not os.path.isdir(dirName): os.mkdir(dirName)

def main():

    # First Set
    # unit of MZp is GeV
    MZpList = [100, 250, 500, 750, 1000, 1250, 1500, 1750, 2000, 2250, 2500, 3000, 3500]
    
    #unit of MChi is GeV
    #mdm = MChi
    mdmList = [1, 100]
    
    print "produce MZp list: ", MZpList
    print
    for MZp in MZpList:
        for mdm in mdmList:
        
            # ZpBaryonic for Z' Mass
            dirName = 'ZpBaryonic_MZp'+str(MZp)+'_MChi'+str(mdm)
        
            mkDir('cards/'+dirName)
            print('create '+dirName)
            shutil.copyfile('cards/monoHiggs_ZpBaryonic/ZpBaryonic_run_card.dat','cards/'+dirName+'/'+dirName+'_run_card.dat')
            shutil.copyfile('cards/monoHiggs_ZpBaryonic/ZpBaryonic_extramodels.dat','cards/'+dirName+'/'+dirName+'_extramodels.dat')
            
            f_proc0 = open('cards/monoHiggs_ZpBaryonic/ZpBaryonic_proc_card.dat','r')
            f_proc1 = open('cards/'+dirName+'/'+dirName+'_proc_card.dat','w')
            for line in f_proc0:
                f_proc1.write(line.replace('ZpBaryonic_MZp1000_MChi1',dirName))
            f_proc0.close()
            f_proc1.close()
            
            f_cust0 = open('cards/monoHiggs_ZpBaryonic/ZpBaryonic_customizecards.dat','r')
            f_cust1 = open('cards/'+dirName+'/'+dirName+'_customizecards.dat','w')
            for line in f_cust0:
                if line.find('MZp') > 0: f_cust1.write(line.replace('MZp',str(MZp)))
                elif line.find('mdm') > 0: f_cust1.write(line.replace('mdm',str(mdm)))
                else: f_cust1.write(line)
            f_cust0.close()
            f_cust1.close()            
                
            command = './gridpack_generation.sh ' + dirName + ' cards/' + dirName
            print(command)
            if (exe1): os.system(command)
                

    # Second Set
    # unit of MZp is GeV
    MZpList2 = [1000, 1500, 1750, 2000, 2250, 2500]

    #unit of MChi is GeV
    #mdm = MChi
    mdmList2 = [200, 400, 600, 800]

    print "produce MZp list2: ", MZpList2
    print
    for MZp in MZpList2:
        for mdm in mdmList2:
            
            # ZpBaryonic for Z' Mass
            dirName = 'ZpBaryonic_MZp'+str(MZp)+'_MChi'+str(mdm)

            mkDir('cards/'+dirName)
            print('create '+dirName)
            shutil.copyfile('cards/monoHiggs_ZpBaryonic/ZpBaryonic_run_card.dat','cards/'+dirName+'/'+dirName+'_run_card.dat')
            shutil.copyfile('cards/monoHiggs_ZpBaryonic/ZpBaryonic_extramodels.dat','cards/'+dirName+'/'+dirName+'_extramodels.dat')

            f_proc0 = open('cards/monoHiggs_ZpBaryonic/ZpBaryonic_proc_card.dat','r')
            f_proc1 = open('cards/'+dirName+'/'+dirName+'_proc_card.dat','w')
            for line in f_proc0:
                f_proc1.write(line.replace('ZpBaryonic_MZp1000_MChi1',dirName))
            f_proc0.close()
            f_proc1.close()

            f_cust0 = open('cards/monoHiggs_ZpBaryonic/ZpBaryonic_customizecards.dat','r')
            f_cust1 = open('cards/'+dirName+'/'+dirName+'_customizecards.dat','w')
            for line in f_cust0:
                if line.find('MZp') > 0: f_cust1.write(line.replace('MZp',str(MZp)))
                elif line.find('mdm') > 0: f_cust1.write(line.replace('mdm',str(mdm)))
                else: f_cust1.write(line)
            f_cust0.close()
            f_cust1.close()

            command = './gridpack_generation.sh ' + dirName + ' cards/' + dirName

            print(command)
            if (exe2): os.system(command)

                

    # Third Set
    # unit of MZp is GeV
    MZpList3 = [500]

    #unit of MChi is GeV
    #mdm = MChi
    mdmList3 = [200, 400]

    print "produce MZp list3: ", MZpList3
    print
    for MZp in MZpList3:
        for mdm in mdmList3:

            # ZpBaryonic for Z' Mass
            dirName = 'ZpBaryonic_MZp'+str(MZp)+'_MChi'+str(mdm)

            mkDir('cards/'+dirName)
            print('create '+dirName)
            shutil.copyfile('cards/monoHiggs_ZpBaryonic/ZpBaryonic_run_card.dat','cards/'+dirName+'/'+dirName+'_run_card.dat')
            shutil.copyfile('cards/monoHiggs_ZpBaryonic/ZpBaryonic_extramodels.dat','cards/'+dirName+'/'+dirName+'_extramodels.dat')

            f_proc0 = open('cards/monoHiggs_ZpBaryonic/ZpBaryonic_proc_card.dat','r')
            f_proc1 = open('cards/'+dirName+'/'+dirName+'_proc_card.dat','w')
            for line in f_proc0:
                f_proc1.write(line.replace('ZpBaryonic_MZp1000_MChi1',dirName))
            f_proc0.close()
            f_proc1.close()

            f_cust0 = open('cards/monoHiggs_ZpBaryonic/ZpBaryonic_customizecards.dat','r')
            f_cust1 = open('cards/'+dirName+'/'+dirName+'_customizecards.dat','w')
            for line in f_cust0:
                if line.find('MZp') > 0: f_cust1.write(line.replace('MZp',str(MZp)))
                elif line.find('mdm') > 0: f_cust1.write(line.replace('mdm',str(mdm)))
                else: f_cust1.write(line)
            f_cust0.close()
            f_cust1.close()
                            
            command = './gridpack_generation.sh ' + dirName + ' cards/' + dirName

            print(command)
            if (exe3): os.system(command)

                

    # Fourth Set
    # unit of MZp is GeV
    MZpList4 = [3000]
                
    #unit of MChi is GeV
    #mdm = MChi
    mdmList4 = [200]

    print "produce MZp list4: ", MZpList4
    print
    for MZp in MZpList4:
        for mdm in mdmList4:

            # ZpBaryonic for Z' Mass
            dirName = 'ZpBaryonic_MZp'+str(MZp)+'_MChi'+str(mdm)

            mkDir('cards/'+dirName)
            print('create '+dirName)
            shutil.copyfile('cards/monoHiggs_ZpBaryonic/ZpBaryonic_run_card.dat','cards/'+dirName+'/'+dirName+'_run_card.dat')
            shutil.copyfile('cards/monoHiggs_ZpBaryonic/ZpBaryonic_extramodels.dat','cards/'+dirName+'/'+dirName+'_extramodels.dat')

            f_proc0 = open('cards/monoHiggs_ZpBaryonic/ZpBaryonic_proc_card.dat','r')
            f_proc1 = open('cards/'+dirName+'/'+dirName+'_proc_card.dat','w')
            for line in f_proc0:
                f_proc1.write(line.replace('ZpBaryonic_MZp1000_MChi1',dirName))
            f_proc0.close()
            f_proc1.close()

            f_cust0 = open('cards/monoHiggs_ZpBaryonic/ZpBaryonic_customizecards.dat','r')
            f_cust1 = open('cards/'+dirName+'/'+dirName+'_customizecards.dat','w')
            for line in f_cust0:
                if line.find('MZp') > 0: f_cust1.write(line.replace('MZp',str(MZp)))
                elif line.find('mdm') > 0: f_cust1.write(line.replace('mdm',str(mdm)))
                else: f_cust1.write(line)
            f_cust0.close()
            f_cust1.close()
                            
            command = './gridpack_generation.sh ' + dirName + ' cards/' + dirName

            print(command)
            if (exe4): os.system(command)

                

if __name__ == "__main__":
    main()
#os.system('mv *.tarball.tar.xz /afs/cern.ch/work/s/slomte/public/monoHiggsZpBaryonic_gridpacks/')

### Author: Shu-Xiao Liu
### Department of Physics, National Central University

import os
import shutil

exe = 0
sub = 1
def mkDir(dirName):
    if not os.path.isdir(dirName): os.mkdir(dirName)

def main():

    # mZp grid
    mZpList = [400,450,500,550,600,800,1000,1200,1400,1700,2000,2500,3000,3500,4000,4500,5000,5500,6000] 

    print "produce MZp list: ", mZpList
    print
    for mZp in mZpList:

        dirName = 'Zprime_Zh_Zinv_MZp'+str(mZp)
        
        mkDir('cards/'+dirName)
        print('create '+dirName)
        shutil.copyfile('cards/Zprime_Zh_Zinv_template/Zprime_Zh_Zinv_MZp1400_run_card.dat','cards/'+dirName+'/'+dirName+'_run_card.dat')
        shutil.copyfile('cards/Zprime_Zh_Zinv_template/Zprime_Zh_Zinv_MZp1400_extramodels.dat','cards/'+dirName+'/'+dirName+'_extramodels.dat')

        f_proc0 = open('cards/Zprime_Zh_Zinv_template/Zprime_Zh_Zinv_MZp1400_proc_card.dat','r')
        f_proc1 = open('cards/'+dirName+'/'+dirName+'_proc_card.dat','w')
        for line in f_proc0:
            if line.find('Zprime_Zh_Zinv_MZp1400') > 0: 
                f_proc1.write(line.replace('Zprime_Zh_Zinv_MZp1400',dirName))
            else:f_proc1.write(line)                    
        f_proc0.close()
        f_proc1.close()

        f_cust0 = open('cards/Zprime_Zh_Zinv_template/Zprime_Zh_Zinv_MZp1400_customizecards.dat','r')
        f_cust1 = open('cards/'+dirName+'/'+dirName+'_customizecards.dat','w')
        for line in f_cust0:
            if line.find('1400') > 0: f_cust1.write(line.replace('1400',str(mZp)))
            else: f_cust1.write(line)
        f_cust0.close()
        f_cust1.close()
            
        command = './gridpack_generation.sh ' + dirName + ' cards/' + dirName
        subcom = './submit_gridpack_generation.sh 12000 12000 1nw ' + dirName + ' cards/' + dirName + ' 2nd'
        print(command)
        if (exe): os.system(command)
        elif (sub): os.system(subcom)

if __name__ == "__main__":
    main()
    #os.system('cp *.tarball.tar.xz ~/public/gridpackDir')

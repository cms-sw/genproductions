### Author: Shu-Xiao Liu
### Department of Physics, National Central University

import os
import shutil

exe = 0
sub = 1
def mkDir(dirName):
    if not os.path.isdir(dirName): os.mkdir(dirName)

def main():

    # mZp x mA0 grid
    mZpList = [400,450,500,550,600,800,1000,1200,1400,1700,2000,2500,3000,3500,4000,4500,5000,5500,6000] 
    mA0List = [300,400,500,600,700,800,900,1000,1200,1400,1600]

    print "produce MZp x A0 list: ", mZpList, mA0List
    print
    for mZp in mZpList:
        for mA0 in mA0List: 
            if mZp > mA0:
                # spin 1, Zprime 2HDM 
                dirName = 'Zprime_A0h_A0chichi_MZp'+str(mZp)+'_MA0'+str(mA0)
        
                mkDir('cards/'+dirName)
                print('create '+dirName)
                shutil.copyfile('cards/Zprime_A0h_A0chichi_template/Zprime_A0h_A0chichi_MZp1400_MA0400_run_card.dat','cards/'+dirName+'/'+dirName+'_run_card.dat')
                shutil.copyfile('cards/Zprime_A0h_A0chichi_template/Zprime_A0h_A0chichi_MZp1400_MA0400_extramodels.dat','cards/'+dirName+'/'+dirName+'_extramodels.dat')

                f_proc0 = open('cards/Zprime_A0h_A0chichi_template/Zprime_A0h_A0chichi_MZp1400_MA0400_proc_card.dat','r')
                f_proc1 = open('cards/'+dirName+'/'+dirName+'_proc_card.dat','w')
                for line in f_proc0:
                    if line.find('Zprime_A0h_A0chichi_MZp1400_MA0400') > 0: 
                        f_proc1.write(line.replace('Zprime_A0h_A0chichi_MZp1400_MA0400',dirName))
                    else:f_proc1.write(line)                    
                f_proc0.close()
                f_proc1.close()

                f_cust0 = open('cards/Zprime_A0h_A0chichi_template/Zprime_A0h_A0chichi_MZp1400_MA0400_customizecards.dat','r')
                f_cust1 = open('cards/'+dirName+'/'+dirName+'_customizecards.dat','w')
                for line in f_cust0:
                    if line.find('1400') > 0: f_cust1.write(line.replace('1400',str(mZp)))
                    elif line.find('400') > 0: f_cust1.write(line.replace('400',str(mA0)))
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

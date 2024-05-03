### Author: Shu-Xiao Liu
### Department of Physics, National Central University

import os
import shutil

exe = 0
sub = 0
def mkDir(dirName):
    if not os.path.isdir(dirName): os.mkdir(dirName)

def main():

    # unit of MX and width is GeV
    mXList = [3000] 
    widthRatio = 0.1


    print "produce MX list: ", mXList
    print
    for mX in mXList: 
        width = mX*widthRatio
        # width = 0.001

        # spin 2, BulkGraviton
        dirName = 'BulkGraviton_hh_M'+str(mX)
        if (width>1):   dirName+='_wideWidth' + str(widthRatio)
        else:           dirName+='_narrowWidth'
        
        mkDir('cards/'+dirName)
        print('create '+dirName)
        shutil.copyfile('cards/BulkGraviton_hh_template/tem_run_card.dat','cards/'+dirName+'/'+dirName+'_run_card.dat')
        shutil.copyfile('cards/BulkGraviton_hh_template/tem_extramodels.dat','cards/'+dirName+'/'+dirName+'_extramodels.dat')

        f_proc0 = open('cards/BulkGraviton_hh_template/tem_proc_card.dat','r')
        f_proc1 = open('cards/'+dirName+'/'+dirName+'_proc_card.dat','w')
        for line in f_proc0:
            f_proc1.write(line.replace('BulkGraviton_hh_M5000_test',dirName))
        f_proc0.close()
        f_proc1.close()

        f_cust0 = open('cards/BulkGraviton_hh_template/tem_customizecards.dat','r')
        f_cust1 = open('cards/'+dirName+'/'+dirName+'_customizecards.dat','w')
        for line in f_cust0:
            if line.find('MX') > 0: f_cust1.write(line.replace('MX',str(mX)))
            elif line.find('WX') > 0: f_cust1.write(line.replace('WX',str(width)))
            else: f_cust1.write(line)
        f_cust0.close()
        f_cust1.close()
        command = './gridpack_generation.sh ' + dirName + ' cards/' + dirName
        subcom = './submit_gridpack_generation.sh 12000 12000 1nw ' + dirName + ' cards/' + dirName + ' 2nd'
        print(command)
        if (exe): os.system(command)
        elif (sub): os.system(subcom)

        # spin 0, Radion
        dirName = 'Radion_hh_M'+str(mX)
        if (width>1):   dirName+='_wideWidth' + str(widthRatio)
        else:           dirName+='_narrowWidth'
        mkDir('cards/'+dirName)
        print('create '+dirName)
        shutil.copyfile('cards/Radion_hh_template/tem_run_card.dat','cards/'+dirName+'/'+dirName+'_run_card.dat')
        shutil.copyfile('cards/Radion_hh_template/tem_extramodels.dat','cards/'+dirName+'/'+dirName+'_extramodels.dat')

        f_proc0 = open('cards/Radion_hh_template/tem_proc_card.dat','r')
        f_proc1 = open('cards/'+dirName+'/'+dirName+'_proc_card.dat','w')
        for line in f_proc0:
            f_proc1.write(line.replace('Radion_hh_M5000_test',dirName))
        f_proc0.close()
        f_proc1.close()

        f_cust0 = open('cards/Radion_hh_template/tem_customizecards.dat','r')
        f_cust1 = open('cards/'+dirName+'/'+dirName+'_customizecards.dat','w')
        for line in f_cust0:
            if line.find('MX') > 0: f_cust1.write(line.replace('MX',str(mX)))
            elif line.find('WX') > 0: f_cust1.write(line.replace('WX',str(width)))
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

import os
import sys


def main():
    # make list of parameters
    mass_list = [700, 800, 900, 1000, 1100, 1200, 1400, 1600, 1800, 2000, 2200, 2400, 2600]
    decay_percent = [5, 10, 20 ,30]

    refFolder = 'TprimeBToTH_LH_M1000_decay30pct'

    folder_type = [ '_int',
                    '_sch',
                    '_tch' ]
    file_type = [ '_customizecards.dat',
                    '_extramodels.dat',
                    '_proc_card.dat',
                    '_run_card.dat' ]

    # cp directories
    for mass in mass_list:
        for decay in decay_percent:
            for type1 in folder_type:
                if mass == 1000 and decay == 30:
                    continue
                currFolder = f'TprimeBToTH_LH_M{mass}_decay{decay}pct{type1}'
                os.system(f'cp -r {refFolder}{type1} {currFolder}')

                # rename files as well
                for file1 in file_type:
                    os.system(f'mv {currFolder}/{refFolder}{type1}{file1} {currFolder}/{currFolder}{file1}')

                # sed on proc_card
                os.system(f'sed -i "s/M1000_decay30/M{mass}_decay{decay}/" {currFolder}/*proc_card.dat')

                # change mass and decay value in customize card
                os.system(f'sed -i "s/mass 6000006 1000/mass 6000006 {mass}/" {currFolder}/*customizecards.dat')
                os.system(f'sed -i "s/DECAY 6000006 300/DECAY 6000006 {decay * mass / 100}/" {currFolder}/*customizecards.dat')

                print(f'{currFolder} done')
                print('')


if __name__ == '__main__':
    main()

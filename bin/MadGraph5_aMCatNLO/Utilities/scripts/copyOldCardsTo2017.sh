#!/bin/bash
# Usage: ./copyOldCardsFor2017.sh <old folder name>
# example: ./copyOldCardsFor2017.sh dyellell012j_5f_NLO_FXFX

if [ -z $1 ]; then
    echo "You need to enter the folder name as an argument"
    echo "ex: ./copyOldCardsFor2017.sh dyellell012j_5f_NLO_FXFX"
    exit 1
fi

base_folder=$(git rev-parse --show-toplevel)/bin/MadGraph5_aMCatNLO

old_cards_path=${base_folder}/cards/production/13TeV/$1
new_cards_path=${base_folder}/cards/production/2017/$1

git diff-index --quiet HEAD -- $base_folder/cards 
if [ $? -ne 0 ]; then
    echo "This script has to be run from a clean git area. "
    echo "If you've made other changes, commit them. If you've "
    echo "changed things but didn't mean to, run 'git reset HEAD' "
    echo "to get back where you started"
    exit 1
fi

git checkout tags/pre2017 -- $old_cards_path

if [ $? -ne 0 ]; then
    echo ""
    echo "ERROR: $old_cards_path doesn't exist in the pre2017 repository. "
    echo "Verify the name and try again."
    exit 1
fi


for old_card in $(find $old_cards_path -type f -follow -print); do 
    new_card=${old_card/13TeV/2017}
    card_dir=$(dirname $new_card)
    if [ ! -d $card_dir ]; then
        mkdir -p $card_dir
    fi
    mv $old_card $new_card
    git add $new_card
    git rm $old_card
done

#git commit -m "Copying $1 cards from legacy production to modify for 2017"

for run_card in $(find $new_cards_path -type f -follow -print -name "*run_card*"); do 
    # reweight_PDF may not be present in older cards
    if grep -q -e ".*= *reweight_PDF" $run_card; then
        sed -i "s/^ [0-9]* *= *lhaid/\$DEFAULT_PDF_SETS = lhaid/g" $run_card
        sed -i "s/.*= *reweight_PDF/\$DEFAULT_PDF_MEMBERS = reweight_PDF/g" $run_card
    else
        sed -i "s/^ [0-9]* *= *lhaid/\$DEFAULT_PDF_SETS = lhaid\n\$DEFAULT_PDF_MEMBERS = reweight_PDF/g" $run_card
    fi
    sed -i "s/.*= *PDF_set_min//g" $run_card
    sed -i "s/.*= *PDF_set_max//g" $run_card
    git add $run_card
done

#git commit -m "Updating PDF sets to 2017 defaults for $1"

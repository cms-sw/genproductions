#!/bin/bash
# Usage: ./copyOldCardsFor2017.sh <old folder name>
# example: ./copyOldCardsFor2017.sh dyellell012j_5f_NLO_FXFX

if [ -z $1 ]; then
    echo "You need to enter the folder name as an argument"
    echo "ex: ./copyOldCardsFor2017.sh dyellell012j_5f_NLO_FXFX"
    exit 1
fi

git diff-index --quiet HEAD -- cards/production
if [ $? -ne 0 ]; then
    echo "This script has to be run from a clean git area. "
    echo "If you've made other changes, commit them. If you've "
    echo "changed things but didn't mean to, run 'git reset HEAD' "
    echo "to get back where you started"
    exit 1
fi

base_folder=$(git rev-parse --show-toplevel)

git checkout tags/pre2017 -- cards/production/13TeV/$1

for old_path in $(git status --porcelain -uno cards/production); do 
    old_card=$base_folder/$old_path
    if [ -f $old_card ]; then 
        new_card=${old_card/13TeV/2017}
        card_dir=$(dirname $new_card)
        if [ ! -d $card_dir ]; then
            mkdir -p $card_dir
        fi
        mv $old_card $new_card
        git add $new_card
        git rm $old_card
    fi 
done
git commit -m "Copying $1 cards from legacy production to modify for 2017"
for run_card in cards/production/2017/$1/*run_card.dat; do
    sed -i "s/^ [0-9]* *= *lhaid/\$DEFAULT_PDF_SETS = lhaid/g" $run_card
    sed -i ".*= *reweight_PDF/\$DEFAULT_PDF_MEMBERS = reweight_PDF/g" $run_card
done

#!/bin/bash
# Script to create all Monojet and monov dmsimp cards
git clone ssh://git@gitlab.cern.ch:7999/aalbert/aa-cardwriter.git
cd aa-cardwriter
git checkout eb084c32a89cfc5a5dab2046c04f6b3b17543623
./create_dmsimp_monojet_spin1_NLO_2017_2018_cards.py
cp -r ./output/* ../dmsimp

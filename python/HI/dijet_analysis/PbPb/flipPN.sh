#!/bin/sh

Pyquen_Dijet120_Np_Unquenched_TuneZ2_5p02TeV_cfi.py

for pt in 15 30 50 80 100 120 170 200 220 250 300 350
do
  mv Pyquen_Dijet${pt}_Np_Unquenched_TuneZ2_5p02TeV_cfi.py m1
  mv Pyquen_Dijet${pt}_pN_Unquenched_TuneZ2_5p02TeV_cfi.py m2
  mv m2 Pyquen_Dijet${pt}_Np_Unquenched_TuneZ2_5p02TeV_cfi.py
  mv m1 Pyquen_Dijet${pt}_pN_Unquenched_TuneZ2_5p02TeV_cfi.py
done
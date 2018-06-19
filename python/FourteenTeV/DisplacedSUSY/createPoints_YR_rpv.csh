#!/bin/tcsh

foreach i (200 250 300 350 400 450 500 550 600 650 700)
    if ($i == '200') then
	set xs=76.8
    endif
    if ($i == '250') then
	set xs=26.05
    endif
    if ($i == '300') then
	set xs=10.42
    endif
    if ($i == '350') then
	set xs=4.68
    endif
    if ($i == '400') then
	set xs=2.2921
    endif
    if ($i == '450') then
	set xs=1.19611
    endif
    if ($i == '500') then
	set xs=0.66227
    endif
    if ($i == '550') then
	set xs=0.381231
    endif
    if ($i == '600') then
	set xs=0.238195
    endif
    if ($i == '650') then
	set xs=0.140248
    endif
    if ($i == '700') then
	set xs=0.0887632
    endif
    foreach j (1 10 100 1000 10000)
	if ($j == '1') then
	    set exp=13
	endif
	if ($j == '10') then
	    set exp=14
	endif
	if ($j == '100') then
	    set exp=15
	endif
	if ($j == '1000') then
	    set exp=16
	endif
	if ($j == '10000') then
	    set exp=17
	endif
	sed "s/XXX/$i/" DisplacedSUSY_stopToBottom_M_XXX_YYYmm_TuneCP5_14TeV_pythia8_cff.py | sed "s/YYY/$j/" | sed "s/ZZZ/$xs/" | sed "s/AAA/$exp/" > DisplacedSUSY_stopToBottom_M_${i}_${j}mm_TuneCP5_14TeV_pythia8_cff.py
    end
end
    

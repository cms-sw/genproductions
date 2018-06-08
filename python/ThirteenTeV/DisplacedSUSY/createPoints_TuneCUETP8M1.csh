#!/bin/tcsh

foreach i (200 400 500 600 700 800 900 1000 1100 1200 1300 1400 1500 1600 1700 1800)
    if ($i == '200') then
	set xs=64.5085
    endif
    if ($i == '400') then
	set xs=1.83537
    endif
    if ($i == '500') then
	set xs=0.51848
    endif
    if ($i == '600') then
	set xs=0.174599
    endif
    if ($i == '700') then
	set xs=0.0670476
    endif
    if ($i == '800') then
	set xs=0.0283338
    endif
    if ($i == '900') then
	set xs=0.0128895
    endif
    if ($i == '1000') then
	set xs=0.00615134
    endif
    if ($i == '1100') then
	set xs=0.00307413
    endif
    if ($i == '1200') then
	set xs=0.00159844
    endif
    if ($i == '1300') then
	set xs=0.000850345
    endif
    if ($i == '1400') then
	set xs=0.000461944
    endif
    if ($i == '1500') then
	set xs=0.000256248
    endif
    if ($i == '1600') then
	set xs=0.000141382
    endif
    if ($i == '1700') then
	set xs=0.0000807774
    endif
    if ($i == '1800') then
	set xs=0.0000467492
    endif

    foreach j (1 10 100 1000)
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
	sed "s/XXX/$i/" DisplacedSUSY_stopToBottom_M_XXX_YYYmm_TuneCUETP8M1_13TeV_pythia8_cff.py | sed "s/YYY/$j/" | sed "s/ZZZ/$xs/" | sed "s/AAA/$exp/" > DisplacedSUSY_stopToBottom_M_${i}_${j}mm_TuneCUETP8M1_13TeV_pythia8_cff.py
	sed "s/XXX/$i/" DisplacedSUSY_stopToLD_M_XXX_YYYmm_TuneCUETP8M1_13TeV_pythia8_cff.py | sed "s/YYY/$j/" | sed "s/ZZZ/$xs/" | sed "s/AAA/$exp/" > DisplacedSUSY_stopToLD_M_${i}_${j}mm_TuneCUETP8M1_13TeV_pythia8_cff.py
    end
end
    

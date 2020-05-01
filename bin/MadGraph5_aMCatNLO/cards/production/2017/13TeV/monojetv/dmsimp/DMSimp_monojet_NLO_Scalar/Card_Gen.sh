#!/bin/bash

massX=( 10 50 350 450 100 200 300 400 500 600 700 800 500 500 500 500 500 10 50 100 50 100 10 50 100 )
massY=( 1 1 1 1 1 1 1 1 1 1 1 1 50 150 200 225 275 4 20 40 22 45 6 28 55 )
width=( 3.742545e-01 1.984664e+00] 1.405314e+01 2.492791e+01 3.976486e+00 7.956554e+00 1.193582e+01 1.901097e+01 3.102530e+01 4.309513e+01 5.478692e+01 6.612294e+01 2.984413e+01 2.131733e+01 1.542859e+01 1.277904e+01 1.113141e+01 8.594367e-02 4.297183e-01 8.594367e-01 2.131762e-01 3.295266e-01 0 0 0 )

#masslist = [[10,1,3.742545e-01],[50,1,1.984664e+00],[350,1,1.405314e+01],[450,1,2.492791e+01],[100,1,3.976486e+00],[200,1,7.956554e+00],[300,1,1.193582e+01],[400,1,1.901097e+01],[500,1,3.102530e+01],[600,1,4.309513e+01],[700,1,5.478692e+01],[800,1,6.612294e+01],[500,50,2.984413e+01],[500,150,2.131733e+01],[500,200,1.542859e+01],[500,225,1.277904e+01],[500,275,1.113141e+01],[10,4,8.594367e-02],[50,20,4.297183e-01],[100,40,8.594367e-01],[50,22,2.131762e-01],[100,45,3.295266e-01],[10,6,0],[50,28,0],[100,55,0]]

for ((i=0;i<${#massX[@]};++i)); do
    printf "%s %s %s\n" "${massX[i]}" "${massY[i]}" "${width[i]}"
    mkdir -p Scalar_MonoJ_NLO_Mphi-${massX[i]}_Mchi-${massY[i]}_gSM-1p0_gDM-1p0
    cp Scalar_MonoJ_NLO_Mphi-MX_Mchi-MY_gSM-1p0_gDM-1p0_run_card.dat Scalar_MonoJ_NLO_Mphi-${massX[i]}_Mchi-${massY[i]}_gSM-1p0_gDM-1p0/Scalar_MonoJ_NLO_Mphi-${massX[i]}_Mchi-${massY[i]}_gSM-1p0_gDM-1p0_run_card.dat 
    cp Scalar_MonoJ_NLO_Mphi-MX_Mchi-MY_gSM-1p0_gDM-1p0_proc_card.dat Scalar_MonoJ_NLO_Mphi-${massX[i]}_Mchi-${massY[i]}_gSM-1p0_gDM-1p0/Scalar_MonoJ_NLO_Mphi-${massX[i]}_Mchi-${massY[i]}_gSM-1p0_gDM-1p0_proc_card.dat
    cp Scalar_MonoJ_NLO_Mphi-MX_Mchi-MY_gSM-1p0_gDM-1p0_customizecards.dat Scalar_MonoJ_NLO_Mphi-${massX[i]}_Mchi-${massY[i]}_gSM-1p0_gDM-1p0/Scalar_MonoJ_NLO_Mphi-${massX[i]}_Mchi-${massY[i]}_gSM-1p0_gDM-1p0_customizecards.dat
    cp Scalar_MonoJ_NLO_Mphi-MX_Mchi-MY_gSM-1p0_gDM-1p0_extramodels.dat Scalar_MonoJ_NLO_Mphi-${massX[i]}_Mchi-${massY[i]}_gSM-1p0_gDM-1p0/Scalar_MonoJ_NLO_Mphi-${massX[i]}_Mchi-${massY[i]}_gSM-1p0_gDM-1p0_extramodels.dat

    sed -i 's|MXvalue|'"${massX[i]}"'|g' Scalar_MonoJ_NLO_Mphi-${massX[i]}_Mchi-${massY[i]}_gSM-1p0_gDM-1p0/Scalar_MonoJ_NLO_Mphi-${massX[i]}_Mchi-${massY[i]}_gSM-1p0_gDM-1p0_customizecards.dat 
    sed -i 's|MYvalue|'"${massY[i]}"'|g' Scalar_MonoJ_NLO_Mphi-${massX[i]}_Mchi-${massY[i]}_gSM-1p0_gDM-1p0/Scalar_MonoJ_NLO_Mphi-${massX[i]}_Mchi-${massY[i]}_gSM-1p0_gDM-1p0_customizecards.dat
    sed -i 's|BRwidth|'"${width[i]}"'|g' Scalar_MonoJ_NLO_Mphi-${massX[i]}_Mchi-${massY[i]}_gSM-1p0_gDM-1p0/Scalar_MonoJ_NLO_Mphi-${massX[i]}_Mchi-${massY[i]}_gSM-1p0_gDM-1p0_customizecards.dat
    sed -i 's|Scalar_MonoJ_NLO_Mphi-MX_Mchi-MY_gSM-1p0_gDM-1p0|Scalar_MonoJ_NLO_Mphi-'"${massX[i]}"'_Mchi-'"${massY[i]}"'_gSM-1p0_gDM-1p0|g' Scalar_MonoJ_NLO_Mphi-${massX[i]}_Mchi-${massY[i]}_gSM-1p0_gDM-1p0/Scalar_MonoJ_NLO_Mphi-${massX[i]}_Mchi-${massY[i]}_gSM-1p0_gDM-1p0_proc_card.dat
done

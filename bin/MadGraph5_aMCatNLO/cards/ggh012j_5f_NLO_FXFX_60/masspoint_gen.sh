cd ..
mkdir ggh012j_5f_NLO_FXFX_$1
cp ggh012j_5f_NLO_FXFX_60/ggh012j_5f_NLO_FXFX_60_customizecards.dat ggh012j_5f_NLO_FXFX_$1/ggh012j_5f_NLO_FXFX_$1_customizecards.dat
cp ggh012j_5f_NLO_FXFX_60/ggh012j_5f_NLO_FXFX_60_extramodels.dat ggh012j_5f_NLO_FXFX_$1/ggh012j_5f_NLO_FXFX_$1_extramodels.dat
cp ggh012j_5f_NLO_FXFX_60/ggh012j_5f_NLO_FXFX_60_proc_card.dat ggh012j_5f_NLO_FXFX_$1/ggh012j_5f_NLO_FXFX_$1_proc_card.dat
cp ggh012j_5f_NLO_FXFX_60/ggh012j_5f_NLO_FXFX_60_run_card.dat ggh012j_5f_NLO_FXFX_$1/ggh012j_5f_NLO_FXFX_$1_run_card.dat

sed -i 's|set param_card mass 25 60.|set param_card mass 25 '"$1"'.|g' ggh012j_5f_NLO_FXFX_$1/ggh012j_5f_NLO_FXFX_$1_customizecards.dat
sed -i 's|output ggh012j_5f_NLO_FXFX_60 -nojpeg|output ggh012j_5f_NLO_FXFX_'"$1"' -nojpeg|g' ggh012j_5f_NLO_FXFX_$1/ggh012j_5f_NLO_FXFX_$1_proc_card.dat

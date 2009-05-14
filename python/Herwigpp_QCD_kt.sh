#!/bin/sh

cd $CMSSW_BASE/src/Configuration/GenProduction/python/

PREFIX="Herwigpp_QCD_kt"

(
cat << EOF
15		1.4755000e+09
30		1.1174600e+08
80		2.0388300e+06
170		6.7980200e+04
300		4.0603200e+03
470		3.5432800e+02
800		1.3268500e+01
1400	1.8450500e-01
2200	1.4957600e-03
3000	9.3023000e-06
EOF
) | while read MINCUT XS; do
	sed -e "s/__MINCUT__/$MINCUT/;s/__XS__/$XS/" \
	$PREFIX.template > ${PREFIX}${MINCUT}_10TeV_cff.py
done

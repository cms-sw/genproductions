#!/bin/sh

cd $CMSSW_BASE/src/Configuration/GenProduction/python/

PREFIX="Pythia6_QCD_pthat"

(
cat << EOF
15		1.457159248e+09
30		1.090572204e+08
80		1.934639567e+06
170		6.256287713e+04
300		3.664608301e+03
470		3.155131272e+02
800		1.194197450e+01
1400	1.720187180e-01
2200	1.420777800e-03
3000	8.600800000e-06
EOF
) | while read MINCUT XS; do
	sed -e "s/__MINCUT__/$MINCUT/;s/__XS__/$XS/" \
	$PREFIX.template > ${PREFIX}${MINCUT}_10TeV_cff.py
done

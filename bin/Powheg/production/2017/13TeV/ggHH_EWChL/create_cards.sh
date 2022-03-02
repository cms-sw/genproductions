#!/usr/bin/env bash

sed 's/.*cHHH.*/cHHH 0.0/' template.input > powheg_cHHH0.input
sed 's/.*cHHH.*/cHHH 1.0/' template.input > powheg_cHHH1.input
sed 's/.*cHHH.*/cHHH 2.45/' template.input > powheg_cHHH2p45.input
sed 's/.*cHHH.*/cHHH 5.0/' template.input > powheg_cHHH5.input

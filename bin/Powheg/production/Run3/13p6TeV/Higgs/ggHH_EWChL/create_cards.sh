#!/usr/bin/env bash

sed 's/.*chhh.*/chhh 0.0/' template.input > powheg_chhh0.input
sed 's/.*chhh.*/chhh 1.0/' template.input > powheg_chhh1.input
sed 's/.*chhh.*/chhh 2.45/' template.input > powheg_chhh2p45.input
sed 's/.*chhh.*/chhh 5.0/' template.input > powheg_chhh5.input

#!/bin/bash

echo "I am here, going to bind this directory:"
pwd

cmssw-el7 --bind $(pwd) --command-to-run bash $1

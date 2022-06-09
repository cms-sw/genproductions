#!/bin/bash

echo "I am here, going to bind this directory:"
pwd

cmssw-cc6 --bind $(pwd) --command-to-run bash $1
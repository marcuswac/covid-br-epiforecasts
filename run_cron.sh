#!/bin/bash
source ~/.bashrc
cd covid-rt-estimates
bin/update-estimates-br.sh
cd ..
bin/update.sh

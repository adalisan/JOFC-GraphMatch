#!/bin/tcsh
#
#$ -cwd 
#$ -V 
#$ -S /bin/tcsh -cwd
#$ -o R.out -j y 

/usr/local/R/bin/R CMD BATCH --no-restore ./src/JOFC-graph-bitflip_sim.R results_bitflip.Rout.$SGE_TASK_ID

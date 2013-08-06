#!/bin/tcsh
#
#$ -cwd 
#$ -V 
#$ -S /bin/tcsh -cwd
#$ -o bitflip.dice.100.R.out -j y 
#$ -pe openmp 24
#$ -v OMP_NUM_THREADS=24

/usr/local/R/bin/R CMD BATCH --no-restore ./src/JOFC-graph-bitflip_sim.R results_bitflip_dice.Rout.100.$SGE_TASK_ID

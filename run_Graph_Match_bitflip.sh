#!/bin/tcsh
#
#$ -cwd 
#$ -V 
#$ -S /bin/tcsh -cwd
#$  -j y 
#$ -pe openmp 8
#$ -v OMP_NUM_THREADS=8

/usr/local/R/bin/R CMD BATCH --no-restore ./src/JOFC-graph-bitflip_sim_base.R results_bitflip_sim_base.$SGE_TASK_ID.Rout

#!/bin/tcsh
#
#$ -cwd 
#$ -V 
#$ -S /bin/tcsh -cwd
#$ -o bitflip.shortpath.300.R.out -j y 
#$ -pe openmp 12
#$ -v OMP_NUM_THREADS=12

/usr/local/R/bin/R CMD BATCH --no-restore ./src/JOFC-graph-bitflip_sim_shortpath.R results_bitflip_shortpath.Rout.300.$SGE_TASK_ID

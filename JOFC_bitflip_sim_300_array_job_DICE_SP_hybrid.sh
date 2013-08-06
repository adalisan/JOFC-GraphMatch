#!/bin/tcsh
#
#$ -cwd 
#$ -V 
#$ -S /bin/tcsh -cwd
#$ -o bitflip.DICE_SP_hybrid.300.R.out -j y 
#$ -pe openmp 12
#$ -v OMP_NUM_THREADS=12

/usr/local/R/bin/R CMD BATCH --no-restore ./src/JOFC-graph-bitflip_sim_DICE_SP_hybrid.R results_bitflip_sim_DICE_SP_hybrid.R.Rout.300.$SGE_TASK_ID

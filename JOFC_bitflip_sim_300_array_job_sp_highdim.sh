#!/bin/tcsh
#
#$ -cwd 
#$ -V 
#$ -S /bin/tcsh -cwd
#$ -o bitflip.shortpath.hd.300.R.out -j y 
#$ -pe openmp 12
#$ -v OMP_NUM_THREADS=12

/usr/local/R/bin/R CMD BATCH --no-restore ./src/JOFC-graph-bitflip_sim_sp_highdim.R results_bitflip_shortpath_hd.Rout.300.$SGE_TASK_ID

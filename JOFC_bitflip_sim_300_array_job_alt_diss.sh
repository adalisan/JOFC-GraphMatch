#!/bin/tcsh
#
#$ -cwd 
#$ -V 
#$ -S /bin/tcsh -cwd
#$ -o bitflip.alt.diss.R.out -j y 
#$ -pe openmp 24
#$ -v OMP_NUM_THREADS=24

/usr/local/R/bin/R CMD BATCH --no-restore ./src/JOFC-graph-bitflip_sim_alt_diss_meas.R results_bitflip_alt_diss.Rout.300.$SGE_TASK_ID

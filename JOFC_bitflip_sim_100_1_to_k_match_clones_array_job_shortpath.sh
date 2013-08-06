#!/bin/tcsh
#
#$ -cwd 
#$ -V 
#$ -S /bin/tcsh -cwd
#$ -o bitflip.shortpath_1_to_k_match_clone.100.R.out -j y 
#$ -pe openmp 1
#$ -v OMP_NUM_THREADS=1

/usr/local/R/bin/R CMD BATCH --no-restore ./src/JOFC.for.Married.Vertices-many-many-corr_par_clone.R results_bitflip_shortpath_1_k_match_clone.Rout.100.$SGE_TASK_ID

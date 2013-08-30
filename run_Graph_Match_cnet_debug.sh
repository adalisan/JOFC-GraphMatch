#!/bin/tcsh
#
#$ -cwd 
#$ -V 
#$ -S /bin/tcsh -cwd
#$  -j y 
#$ -pe openmp 1	
#$ -v OMP_NUM_THREADS=1

/usr/local/R/bin/R CMD BATCH --no-restore ./src/JOFC-graph_charitynet_base.R ./logs/cnet/results_cnet_debug.$SGE_TASK_ID.Rout

#!/bin/tcsh
#
#$ -cwd 
#$ -V 
#$ -S /bin/tcsh -cwd
#$ -o JOFC-graph_enron_dice_legacy.R.out -j y 
#$ -pe openmp 8
#$ -v OMP_NUM_THREADS=8

/usr/local/R/bin/R CMD BATCH --no-restore ./src/JOFC-graph_enron_legacy.R results_enron_dice_legacy.Rout.$SGE_TASK_ID

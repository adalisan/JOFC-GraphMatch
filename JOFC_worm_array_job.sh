#!/bin/tcsh
#
#$ -cwd 
#$ -V 
#$ -S /bin/tcsh -cwd
#$ -o R.out -j y 

/usr/local/R/bin/R CMD BATCH ./src/JOFC-worm-graph-main.R results.Rout.$SGE_TASK_ID

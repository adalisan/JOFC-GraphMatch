#!/bin/tcsh
#
#$ -cwd 
# 
#$ -S /bin/tcsh -cwd
#$ -o R.out -j y 

/usr/local/R/bin/R CMD BATCH ./src/JOFC-worm-$SGE_TASK_ID.R results.Rout.$SGE_TASK_ID

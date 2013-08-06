#!/bin/tcsh
#
#$ -cwd 
#$ -V 
#$ -S /bin/tcsh -cwd
#$ -o R.wiki.out -j y 
#$ -pe openmp 24
#$ -v OMP_NUM_THREADS=24

/usr/local/R/bin/R CMD BATCH ./src/JOFC-graph_wiki.R results_wiki.Rout.$SGE_TASK_ID

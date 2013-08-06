#!/bin/tcsh
#
#$ -cwd 
#$ -V 
#$ -S /bin/tcsh -cwd
#$  -j y 
#$ -pe openmp 8
#$ -v OMP_NUM_THREADS=8
#TODO: take a data file (Rdata) (including graphs G1 and G2 to be matched) as argument

/usr/local/R/bin/R CMD BATCH --no-restore ./src/JOFC-graph_generic_base.R results_bitflip_sim_base.$SGE_TASK_ID.Rout

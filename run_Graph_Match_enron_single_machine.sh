#!/bin/bash
#
#$ -cwd
#$ -V
#$ -S /bin/tcsh -cwd
#$  -j y
for param_list_index in  9 10
do
   SGE_TASK_ID=$param_list_index
   export SGE_TASK_ID
   /usr/bin/R CMD BATCH --no-restore ./src/JOFC-graph_enron_base.R results_enron_base.$param_list_index.Rout
   echo $SGE_TASK_ID
   echo 'Done'
done

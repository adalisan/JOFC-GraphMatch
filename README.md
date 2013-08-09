JOFC-GraphMatch
===============

Uses JOFC  embedding of dissimilarities to embed graphs

The R files in */lib* directory are utility and method functions

The R files in */src* directory are simulation functions, appropriate for the datasets (most of which are in */data*)
One can run simulations in a SGE cluster using the sh files in the root of the repository:
	qsub -t <SGE_TASK_ID> ./run_Graph_match_<graph_data> 
where *<SGE_TASK_ID>* is a positive integer and *<graph_data>* is one of *cnet*, *worm*, *wiki*,or *enron* 
The simulation parameters are read from the file **./src/JOFC-graph_<graph_data>_Params_<SGE_TASK_ID>.R** . See **./src/JOFC-graph_charitynet_Params_1.R** for an example.

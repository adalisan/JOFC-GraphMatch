\documentclass{article}

\begin{document}
\SweaveOpts{concordance=TRUE}

<<results=tex>>=
source("JOFC_FAQ_matlab.Rdata")

table.form.a<-matrix(0,dim(JOFC_corr_worm_wt_dice)[1],4)
table.form.b<-matrix(0,dim(fc_dir_agg)[1],4)
JOFC_res<-as.data.frame(cbind(JOFC_corr_worm_wt_dice_mean,JOFC_corr_worm_wt_dice_unwt_mean,JOFC_corr_worm_wt_dice_directed_mean,JOFC_corr_worm_wt_dice_unwt_directed_mean ))
tabular(n_vals_worm ~ JOFC_corr_worm_wt_dice_mean + JOFC_corr_worm_wt_dice_unwt_mean+ JOFC_corr_worm_wt_dice_directed_mean+JOFC_corr_worm_wt_dice_unwt_directed_mean,data=JOFC_res)

@



\end{document}
#!/usr/bin/env Rscript

library("ggplot2")
library("glue")
library('readxl')


dta_fn <- '/Users/mmir/Library/CloudStorage/Dropbox/git/19A241021CSF_reproducing_plots_4_IGSS_talk/inp/5A_out/corrs.xlsx'

dta <- read_excel(dta_fn)

colnames(dta)[1] <- 'Genotype'
colnames(dta)[2] <- 'Info'
colnames(dta)[3] <- 'IBD'


dta$se = dta$std / sqrt(dta$n_snps)
dta$Info = dta$Info / 100


# plot correlations
ggplot(dta, aes(x = Info, y = mean, color=Genotype, shape=factor(IBD))) +
  geom_line() +
  geom_hline(yintercept=c(0, 0.5, 1), linetype="dashed") +
  theme_classic() +
  labs(x = "INFO Score", y = "Mean Genotype Correlation") + 
  theme(axis.title = element_text(size = 22) , axis.text = element_text(size = 14), legend.text = element_text(size = 20), legend.title = element_text(size = 22))



ofn <- '/Users/mmir/Library/CloudStorage/Dropbox/git/19A241021CSF_reproducing_plots_4_IGSS_talk/out/mean_gt_corr_by_ibd.png'

ggsave(ofn)

#!/usr/bin/env Rscript

library("data.table")
library(dplyr)
library("plinkFile")
library("genio")
library("ggplot2")
library("glue")
library('readxl')
library(writexl)
library(dplyr)


# from 4A240411_plot_all_70_bins/out
dta_fn <- '/Users/mmir/Library/CloudStorage/Dropbox/git/19A241021CSF_reproducing_plots_4_IGSS_talk/inp/4A240411_out/corrs.xlsx'

dta <- read_excel(dta_fn)

dta <- dta %>% mutate(`Pair Type` = case_when(
  InfType == "FS" ~ "Full-Sibs",
  InfType == "PO" ~ "Parent-Offspring",
  TRUE ~ NA_character_  # Default case for all other values
))

colnames(dta)[3] <- 'Info'

dta$se = dta$std / sqrt(dta$n_snps)
dta$Info = dta$Info / 100

l1 <- c("Dosages", 'Hard-Calls')
l2 <- c("Siblings", "Parent-Offspring")

# plot correlations
ggplot(dta, aes(x = Info, y = mean, color=Genotype, linetype = `Pair Type`)) +
  geom_line(size = .65, alpha = 1) +
  geom_hline(yintercept=0.5, linetype="dashed") +
  scale_color_discrete(labels=l1) +
  scale_shape_discrete(labels=l2) + 
  scale_linetype_manual(values = c("solid", "dashed"), labels = unique(dta$`Pair Type`)) + 
  theme_classic() +
  labs(x = "INFO Score", y = "Mean Genotype Correlation") + 
  theme(axis.title = element_text(size = 22) , axis.text = element_text(size = 14), legend.text = element_text(size = 20), legend.title = element_text(size = 22))


ofn <- '/Users/mmir/Library/CloudStorage/Dropbox/git/19A241021CSF_reproducing_plots_4_IGSS_talk/out/mean_gt_corr.png'

ggsave(ofn)



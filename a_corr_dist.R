#!/usr/bin/env Rscript

library("data.table")
library("dplyr")
library("plinkFile")
library("genio")
library("ggplot2")
library("glue")
library('readxl')



plot_corr <- function(dta_fp, pair_name, gtype_name, info_score, fpo) 
{
  
  df <- read_excel(dta_fp, col_names = T)
  names(df) = c('snp', 'corr')
  df <- na.omit(df)
  
  n_snps <- nrow(df)
  dn = info_score / 100
  
  mean_val = mean(df$corr)
  
  p <- ggplot(data.frame(df), aes(x = corr)) +
    geom_histogram(binwidth = 0.02, fill = "#56B4E9", colour = "#56B4E9", alpha = 0.5) +
    labs(title = glue('SNPs with INFO = {dn} - {dn+0.01} (n = {n_snps})'),
         x = "Genotype Correlation",
         y = "Count") +
    geom_vline(xintercept=0.5, linetype="dotted") +
    geom_vline(xintercept=mean_val, linetype="dashed", color = 'red') +
    
    theme_classic() +
    theme(axis.title = element_text(size = 20 / .72) , axis.text = element_text(size = 16 / .72), plot.title = element_text(size = 14 / .72))
  
  counts <- ggplot_build(p)$data[[1]]$count  # Extract the counts from the histogram
  mean_count <- max(counts) / 2
  
  
  p <- p + annotate("text", x = mean_val, y = mean_count, 
                    label = glue("mean = {round(mean_val, 2)}"), 
                    color = "red", angle = 90, vjust = -0.5, size = 6/.72, alpha = .8)
  ggsave(fpo)
  
  # return(p)
}

info <- seq(30, 99, 1)

in_0 <- '/Users/mmir/Library/CloudStorage/Dropbox/git/19A241021CSF_reproducing_plots_4_IGSS_talk/inp/4A240411_out'
out_0 <- '/Users/mmir/Library/CloudStorage/Dropbox/git/19A241021CSF_reproducing_plots_4_IGSS_talk/out'

dirn <- 'FS_DSG'

dta_dir <- glue('{in_0}/{dirn}')
out_dir <- glue('{out_0}/{dirn}')

prd <- expand.grid(out_dir, dta_dir, 'Full Sibs', 'Dosages', info)


dirn <- 'FS_HC'

dta_dir <- glue('{in_0}/{dirn}')
out_dir <- glue('{out_0}/{dirn}')

prd1 <- expand.grid(out_dir, dta_dir, 'Full Sibs', 'Hard-Calls', info)

prd <- rbind(prd, prd1)


dirn <- 'PO_DSG'

dta_dir <- glue('{in_0}/{dirn}')
out_dir <- glue('{out_0}/{dirn}')

prd1 <- expand.grid(out_dir, dta_dir, 'Parent-Offspring', 'Dosages', info)

prd <- rbind(prd, prd1)

dirn <- 'PO_HC'

dta_dir <- glue('{in_0}/{dirn}')
out_dir <- glue('{out_0}/{dirn}')

prd1 <- expand.grid(out_dir, dta_dir, 'Parent-Offspring', 'Hard-Calls', info)

prd <- rbind(prd, prd1)

x <- 1
pair_name = prd[x, 'Var3']
gtype_name = prd[x, 'Var4']
info_score = prd[x, 'Var5']
dta_fp = glue("{prd[x, 'Var2']}/i{info_score}.xlsx")
fpo = glue("{prd[x, 'Var1']}/i{info_score}.png")

plot_corr(dta_fp,pair_name,gtype_name,info_score, fpo)

for (x in 1:nrow(prd))
{
  pair_name = prd[x, 'Var3']
  gtype_name = prd[x, 'Var4']
  info_score = prd[x, 'Var5']
  dta_fp = glue("{prd[x, 'Var2']}/i{info_score}.xlsx")
  fpo = glue("{prd[x, 'Var1']}/i{info_score}.png")
  
  plot_corr(dta_fp,pair_name,gtype_name,info_score, fpo)
}

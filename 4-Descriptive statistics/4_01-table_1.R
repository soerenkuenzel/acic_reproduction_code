library(tidyverse)
library(reshape)
library(xtable)

library("qwraps")
working_dir <- "~/Dropbox/ACIC_workshop_paper/Code/analysis/"
source(paste0(working_dir, "2-ATE/2_00-ATE_Estimation_Functions.R"))


ds <- read.csv("data/synthetic_data.csv")


nrow(ds)

table(ds$schoolid)
ds$schoolid %>% unique %>% length


table_summaries <- list(
  "Schools" = list(
    "Smallest" = ~ min(table(schoolid)),
    "Mean" = ~ round(mean(table(schoolid)), 2),
    "Largest" = ~ max(table(schoolid)),
    "SD" = ~ round(sd(table(schoolid)), 2)
  ),
#  "Intervention" = list(
#    "Treated" = ~n_perc(Z),
#    "Control" = ~n_perc(!Z)
#  ),
  "Outcome" = list(
    "Mean" = ~ round(mean(table(schoolid)), 2),
    "SD" = ~ round(sd(table(schoolid)), 2)
  ),
"Self reported achievement" = list(
  "Mean" = ~ round(mean(S3), 2),
  "SD" = ~ round(sd(S3), 2)
))

tab <- summary_table(ds %>% group_by(Z), table_summaries)


paste(gsub("\\\\hline", "", capture.output(print(tab))), collapse = "")


ds %>% group_by(Z) %>% summarize(mean = paste(round(mean_ci(S3), 2), collapse = " "))


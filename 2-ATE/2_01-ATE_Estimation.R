working_dir <- "~/Dropbox/ACIC_workshop_paper/Code/analysis/"
source(paste0(working_dir, "2-ATE/2_00-ATE_Estimation_Functions.R"))

library(tidyverse)
library(reshape)

ds <- read.csv("data/synthetic_data.csv") %>% 
  tbl_df()   

ATE_estimation(ds$Y, ds$Z, ds)

matching_sensitivity(ds$Y, ds$Z, ds)

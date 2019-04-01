working_dir <- "~/Dropbox/ACIC_workshop_paper/Code/analysis/"
source(paste0(working_dir, "2-ATE/2_00-ATE_Estimation_Functions.R"))
set.seed(59102728)

library(tidyverse)
library(reshape)

# Estimating ATE --------------------------------------------------------------

ds <- read.csv(paste0(working_dir, "data/synthetic_data.csv")) %>% 
  tbl_df()   
ATE_estimation(ds$Y, ds$Z, ds)
matching_sensitivity(ds$Y, ds$Z, ds)


# Assessing overlap --------------------------------------------------------------
library(gam)

pscore_model <- gam(Z ~ C1 + C2  + s(S3) + s(XC) + s(X1) + 
                                s(X2) + s(X3) + s(X4) + s(X5), data = ds)
pscore_hat <- predict(pscore_model)

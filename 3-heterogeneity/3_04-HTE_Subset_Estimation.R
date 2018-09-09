library(tidyverse)
library(reshape)
working_dir <- "~/Dropbox/ACIC_workshop_paper/Code/analysis/"
source(paste0(working_dir, "2-ATE/2_00-ATE_Estimation_Functions.R"))

ev_sepbl <- read.csv("1-IndividualLevel/exploration_validation_splitting.csv")

ds <- read.csv("data/synthetic_data.csv") %>% 
  tbl_df()

subset_analyzer <- function(group_ids, Y, Z, X, summary_function) {
  out <- list()
  for(group in unique(group_ids)) {
    group_results <- list(summary_function(
      Y[group_ids == group], 
      Z[group_ids == group], 
      X[group_ids == group,])
    )
    names(group_results) <- group
    out <- c(out,  group_results)
  }
  return(out)
}

subset_analyzer(ds$X1 > 0.15, ds$Y, ds$Z, ds, ATE_estimation)
subset_analyzer(ds$X1 > 0.15, ds$Y, ds$Z, ds, matching_sensitivity)


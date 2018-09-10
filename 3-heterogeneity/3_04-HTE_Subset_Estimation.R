library(tidyverse)
library(reshape)
working_dir <- "~/Dropbox/ACIC_workshop_paper/Code/analysis/"
source(paste0(working_dir, "2-ATE/2_00-ATE_Estimation_Functions.R"))

ev_sepbl <- read.csv("1-IndividualLevel/exploration_validation_splitting.csv")

ds <- read.csv("data/synthetic_data.csv") %>% 
  tbl_df() %>%
  filter(ev_sepbl$validation) # only take the validation set here


subset_analyzer <- function(group_ids, Y, Z, X, summary_function) {
  #' @group_ids vector of same length as Y,X,Z, the unique values of which
  #' correspond to distinct groups (cf. index argument in tapply)
  #' @Y outcome
  #' @Z trt
  #' @X tbl of covariates
  #' @summary_function function to apply to each subset. 
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

X1groups <- ifelse(ds$X1 > 0.15, "large", "small")

res_split1 <- subset_analyzer(X1groups, ds$Y, ds$Z, ds, ATE_estimation) 
subset_analyzer(ds$X1 > 0.15, ds$Y, ds$Z, ds, matching_sensitivity) 

X2groups <- ifelse(ds$X2 < -.8, "small", ifelse(ds$X2 < 1.1, "medium", "large"))
res_split2 <- subset_analyzer(X2groups, ds$Y, ds$Z, ds, ATE_estimation)
subset_analyzer(X2groups, ds$Y, ds$Z, ds, matching_sensitivity)



#Constructing LaTeX CIs
formatter <- function(num, digits) {
  formatC( round(num, digits), format='f', digits=digits)
}

LaTeXify <- function(CIvec) {
  paste0(formatter(CIvec[2],2), " (", formatter(CIvec[1],2), "--", formatter(CIvec[3],2), ")")
}
sapply(res_split1$large, LaTeXify)
sapply(res_split1$small, LaTeXify)


sapply(res_split2$large, LaTeXify)
sapply(res_split2$medium, LaTeXify)
sapply(res_split2$small, LaTeXify)

# Implementing test to see if there are differenes in response 
# between subgroups.

# permutation_test <- function(group_ids, statistic = "ipw", Y, Z, X) {
#   stopifnot(statistic == "ipw")
#   ATE_estimation(ds$Y, ds$Z, ds) {
#     
#   }
#   
# }

# Neyman




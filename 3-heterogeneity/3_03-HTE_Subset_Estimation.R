# This file assesses treatment effect heterogeneity between the subgroups identified in
# 3_01 and 3_02

library(tidyverse)
library(reshape)
working_dir <- "~/Dropbox/ACIC_workshop_paper/Code/analysis/"
source(paste0(working_dir, "2-ATE/2_00-ATE_Estimation_Functions.R"))
set.seed(59102728)



ev_sepbl <- read.csv(paste0(working_dir, "1-IndividualLevel/exploration_validation_splitting.csv"))

ds <- read.csv(paste0(working_dir,"data/synthetic_data.csv")) %>% 
  tbl_df() %>%
  filter(ev_sepbl$validation) # only take the validation set here


subset_analyzer <- function(group_ids, Y, Z, X, summary_function, ...) {
  #' @group_ids vector of same length as Y,X,Z, the unique values of which
  #' correspond to distinct groups (cf. index argument in tapply)
  #' @Y outcome
  #' @Z trt
  #' @X tbl of covariates
  #' @summary_function function to apply to each subset. 
  #' @... arguments to be passed to summary_function
  out <- list()
  for(group in unique(group_ids)) {
    group_results <- list(summary_function(
      Y[group_ids == group], 
      Z[group_ids == group], 
      X[group_ids == group,], ...)
    )
    names(group_results) <- group
    out <- c(out,  group_results)
  }
  ns <- as.vector(table(group_ids))
  return(list(results = out, ns = ns))
}

test_ATE_difference <- function(ate1, ate1_se, ate2, ate2_se, type = "wald",
                                two_sided = TRUE) {
#" Runs a Wald test to estimate whether difference in ATE is significant
#'  @ate1 estimate of ATE in group 1 (can be AIPW, IPW, reg, ...)
#'  @ate2 estimate of ATE in group 2
#'  @ate1_se estimate of SE of ate1
#'  @ate2_se estimate of SE of ate2.
#'  @type type of test to conduct (currently only "wald" type tests are implemented)
if(type == "wald")
  z_diff_stat <- (ate1 - ate2)/sqrt(ate1_se^2 + ate2_se^2)
  p_value <- pnorm(-abs(z_diff_stat), 0, 1)
  if(two_sided) {
    p_value <- 2*p_value
  }
  return(unname(p_value))
}

compute_p_value <- function(result_obj, level_select = c(1,2)) {
  #' Wrapper function for test_ATE_difference
  #' @result_obj object returned by subset_analyzer on which we would like 
  #'             to run test
  #' @level_select index of groups for which comparison is required (eg if 
  #'              there are low, medium, and high groups we need to identify 
  #'              which are used in comparison. This function will print the 
  #'              names of the groups to avoid confusion)
  nn <- names(result_obj$results)
  cat("P value comparing ",  nn[level_select[1]], " and ", 
      nn[level_select[2]], ".\n", sep = "")
  test_ATE_difference(result_obj$results[[level_select[1]]]$AIPW[2],
                      result_obj$results[[level_select[1]]]$AIPW[4],
                      result_obj$results[[level_select[2]]]$AIPW[2],
                      result_obj$results[[level_select[2]]]$AIPW[4])
}

formatter <- function(num, digits) {
  #' Rounds a number and prints trailing zeros (if any)
  #' @num number to be roundd
  #' @digits number of significant digits to show
  formatC(round(num, digits), format='f', digits=digits)
}
printCI <- function(CIvec) {
  #' prints a confidence interval to in format suitable for manuscript
  paste0(
    formatter(CIvec[2],2), " (", 
    formatter(CIvec[1],2), "--", 
    formatter(CIvec[3],2), ")"
  )
}

printCIs <- function(results) {
  # Wrapper function to apply printCI to objects return by test_ATE_difference
  lapply(results[[1]], function(...) sapply(..., FUN = printCI))
}

## ------------------------------------------------------------------------

# Workshop analysis
X1groups <- ifelse(ds$X1 > 0.15, "large", "small")
X1groups_res <- subset_analyzer(X1groups, ds$Y, ds$Z, ds, ATE_estimation, 
                                non_smooth_vars = c("C2", "C3", "X1")) 

kruskal.test(ds$Y[ds$Z == 0], as.factor(X1groups)[ds$Z == 0])

X2groups <- ifelse(ds$X2 < -.8, "small", ifelse(ds$X2 < 1.1, "medium", "large"))
X2groups_res <- subset_analyzer(X2groups, ds$Y, ds$Z, ds, ATE_estimation, 
                                non_smooth_vars = c("C2", "C3", "X2", "XC"))

X2groups_merged <- ifelse(ds$X2 < -.8, "outside", 
                          ifelse(ds$X2 < 1.1, "inside", "outside"))
X2groups_merged_res <- subset_analyzer(X2groups_merged, ds$Y, ds$Z, ds, ATE_estimation, 
                                       non_smooth_vars = c("C2", "C3", "X2"))


# Printing CIs
printCIs(X1groups_res)
printCIs(X2groups_res)
printCIs(X2groups_merged_res)

# Computing p-values
compute_p_value(X1groups_res)
compute_p_value(X2groups_res,c(1,2))
compute_p_value(X2groups_res,c(1,3))
compute_p_value(X2groups_res,c(2,3))
compute_p_value(X2groups_merged_res)



## ------------------------------------------------------------------------

# Post workshop analysis
urbanicity3 <- ifelse(ds$XC  == "3", "urbanicity3", "urbanicityOther")
urbanicity3_res <- subset_analyzer(urbanicity3, ds$Y, ds$Z, ds, ATE_estimation,
                                   non_smooth_vars = c("C2", "C3", "XC"))
# Warnings arise because XC is collinear with the intercept term

selfexp4 <- ifelse(ds$S3  == "4", "selfexp4", "selfexpOther")
selfexp4_res <- subset_analyzer(selfexp4, ds$Y, ds$Z, ds, ATE_estimation, 
                                non_smooth_vars = c("C2", "C3", "S3"))
# Warnings arise because S3 is collinear with the intercept term

# post Workshop
printCIs(urbanicity3_res)
printCIs(selfexp4_res)

compute_p_value(urbanicity3_res)
compute_p_value(selfexp4_res)




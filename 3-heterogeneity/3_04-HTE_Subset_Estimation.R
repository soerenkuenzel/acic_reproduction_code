library(tidyverse)
library(reshape)
working_dir <- "~/Dropbox/ACIC_workshop_paper/Code/analysis/"
source(paste0(working_dir, "2-ATE/2_00-ATE_Estimation_Functions.R"))

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

test_ATE_difference <- function(ate1, ate1_se, ate2, ate2_se, type = "zasymp",
                                two_sided = TRUE) {
#'  @ate1 estimate of ATE in group 1 (can be AIPW, IPW, reg, ...)
#'  @ate2 estimate of ATE in group 2
#'  @ate1_se estimate of SE of ate1
#'  @ate2_se estimate of SE of ate2.
#'  @type type of test to conduct (currently only "Wald" type tests are implemented)
if(type == "zasymp")
  z_diff_stat <- (ate1 - ate2)/sqrt(ate1_se^2 + ate2_se^2)
  p_value <- pnorm(-abs(z_diff_stat), 0, 1)
  if(two_sided) {
    p_value <- 2*p_value
  }
  return(c(p_value = p_value))
}

# Workshop analysis
X1groups <- ifelse(ds$X1 > 0.15, "large", "small")
X1groups_res <- subset_analyzer(X1groups, ds$Y, ds$Z, ds, ATE_estimation, non_smooth_vars = c("C2", "C3", "X1")) 
#subset_analyzer(ds$X1 > 0.15, ds$Y, ds$Z, ds, matching_sensitivity) 

kruskal.test(ds$Y[ds$Z == 0], as.factor(X1groups)[ds$Z == 0])

X2groups <- ifelse(ds$X2 < -.8, "small", ifelse(ds$X2 < 1.1, "medium", "large"))
X2groups_res <- subset_analyzer(X2groups, ds$Y, ds$Z, ds, ATE_estimation, non_smooth_vars = c("C2", "C3", "X2"))

X2groups_merged <- ifelse(ds$X2 < -.8, "outside", ifelse(ds$X2 < 1.1, "inside", "outside"))
X2groups_merged_res <- subset_analyzer(X2groups_merged, ds$Y, ds$Z, ds, ATE_estimation, non_smooth_vars = c("C2", "C3", "X2"))


# Post workshop analysis
urbanicity4 <- ifelse(ds$XC  == "4", "urbanicity4", "urbanicityOther")
urbanicity4_res <- subset_analyzer(urbanicity4, ds$Y, ds$Z, ds, ATE_estimation, non_smooth_vars = c("C2", "C3", "XC"))

selfexp4 <- ifelse(ds$S3  == "4", "selfexp4", "selfexpOther")
selfexp4_res <- subset_analyzer(selfexp4, ds$Y, ds$Z, ds, ATE_estimation, non_smooth_vars = c("C2", "C3", "S3"))


# Constructing CIs for transfer to LaTeX
formatter <- function(num, digits) {
  formatC( round(num, digits), format='f', digits=digits)
}
printCI <- function(CIvec) {
  paste0(
    formatter(CIvec[2],2), " (", 
    formatter(CIvec[1],2), "--", 
    formatter(CIvec[3],2), ")"
  )
}

printCIs <- function(results) {
  lapply(results[[1]], function(...) sapply(..., FUN = printCI))
}

printCIs(X1groups_res)
printCIs(X2groups_res)
printCIs(X2groups_merged_res)

#post Workshop
printCIs(urbanicity4_res)
printCIs(selfexp4_res)

# Implementing test to see if there are differences in response 
# between subgroups.

tapply(ds$Y, X1groups, mean)
tapply(ds$Y, X1groups, sd)


compute_p_value <- function(result_obj) {
with(result_obj$results, 
     test_ATE_difference(small$AIPW[2],
                         small$AIPW[4],
                         large$AIPW[2],
                         large$AIPW[4])
)
  
  X1groups_res
  
with(X1groups_res$results, 
     test_ATE_difference(small$IPW[2],
                         small$IPW[4],
                         large$IPW[2],
                         large$IPW[4])
)

with(urbanicity4_res$results, 
     test_ATE_difference(urbanicity4$AIPW[2],
                         urbanicity4$AIPW[4],
                         urbanicityOther$AIPW[2],
                         urbanicityOther$AIPW[4])
)



with(selfexp4_res$results, 
     test_ATE_difference(selfexp4$AIPW[2],
                         selfexp4$AIPW[4],
                         selfexpOther$AIPW[2],
                         selfexpOther$AIPW[4])
)

with(X2groups_merged_res$results,
     test_ATE_difference(inside$AIPW[2],
                         inside$AIPW[4],
                         outside$AIPW[2],
                         outside$AIPW[4])
)
with(X2groups_res$results, 
     test_ATE_difference(small$AIPW[2],
                         small$AIPW[4],
                         results$medium$AIPW[2],
                         medium$AIPW[4])
)

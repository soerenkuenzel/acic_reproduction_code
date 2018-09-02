setwd("/Users/soeren/Dropbox/ACIC_workshop_paper/Code/analysis/")
library(tidyverse)
library(reshape)
nthread <- 8

ds <- read.csv("data/synthetic_data.csv") %>% tbl_df() %>%
  mutate(
    schoolid = factor(schoolid),
    C1 = factor(C1),
    C2 = factor(C2),
    C3 = factor(C3)
  ) 


# Y - (a continuous measure of achievement), 
# Z - a binary treatment variable Z indicating receipt of the intervention

# S3 - Students' self-reported expectations for success in the future, a proxy
# for prior achievement, measured prior to random assignment
# C1 - Categorical variable for student race/ethnicity
# C2 - Categorical variable for student identified gender
# C3 - Categorical variable for student first-generation status (i.e. first in 
# family to go to college)
# XC - School-level categorical variable for urbanicity of the school (i.e. 
# rural, suburban, etc.)
# X1 - School-level mean of students' fixed mindsets, reported prior to random 
# assignment
# X2 - School achievement level, as measured by test scores and college 
# preparation for the previous 4 cohorts of students
# X3 - School racial/ethnic minority composition -- i.e. % black, latino, or
# native/american
# X4 - School poverty concentration -- i.e. % of students who are from families
# whose incomes fall below the federal poverty line
# X5 - School size - Total # of students in all four grade levels in the school

# ------------------------------------------------------------------------------
# 1 a) Exclude School ID
#   b) Normalize by School ID
#   c) Use School ID as a feature

# 1a
feat <- ds[, c("S3", "C1", "C2", "C3", "XC", "X1", "X2", "X3", "X4", "X5")]
feat_schoolLevel <- ds[, c("S3", "C1", "C2", "C3", "schoolid")]
Y <- ds$Y
W <- ds$Z

source("1-IndividualLevel/1_01-define_estimatorsCT.R")
  
dir.create("1-IndividualLevel/estimates", showWarnings = FALSE)
for (slb in c(FALSE, TRUE)) {
  for (estimator_i in 1:length(estimator_grid)) {
    # estimator_i = 16; slb = TRUE
    if (slb) {
      feat_to_use <- feat_schoolLevel
      feat_var_name <- "_SchoolIdNoSchoolVars"
    } else {
      feat_to_use <- feat
      feat_var_name <- "_everythingButSchoolId"
    }
    estimates_i <- NULL
    
    estimator <- estimator_grid[[estimator_i]]
    estimator_name <- names(estimator_grid)[estimator_i]
    CATEpredictor <- CATEpredictor_grid[[estimator_name]]
    
    estimates_i <-
      tryCatch({
        L <- estimator(feat = feat_to_use,
                       W = W,
                       Yobs = Y)
        CATEpredictor(L, feat_to_use)
      },
      error = function(e) {
        print(e)
        warning(paste("Something went wrong with", estimator_name))
        return(NA)
      })
    
    estimates_i <- as.data.frame(estimates_i)
    
    colnames(estimates_i) <- paste0(estimator_name, feat_var_name)
    rownames(estimates_i) <- 1:nrow(feat)
    head(estimates_i)
    
    
    write.csv(
      estimates_i,
      file = paste0(
        "1-IndividualLevel/estimates/",
        estimator_name,
        feat_var_name, 
        ".csv"
      )
    )
    
    print(paste("      Done with ",
                estimator_name, slb))
  }
}



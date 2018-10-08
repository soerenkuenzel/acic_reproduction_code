if (file.exists("~/Dropbox/ACIC_workshop_paper/Code/analysis/")) {
  setwd("~/Dropbox/ACIC_workshop_paper/Code/analysis/")
} else if (file.exists("~/acic_reproduction_code/")) {
  setwd("~/acic_reproduction_code/")
} else if (file.exists("~/acic_reproduction_code/")) {
  setwd("~/ACIC-code/")
} else {
  stop("Cannot set working directory!")
}

if (FALSE) {
devtools::with_libpaths(
  new = "~/Rlibs",
  devtools::install_github("soerenkuenzel/causalToolbox", 
                           auth_token = "aa0ad95703c119244a0ff7d9661d3ec5592e3bb1"))
}
library(causalToolbox, lib.loc='~/Rlibs/')
source("1-IndividualLevel/1_02-define_estimatorsCT.R")
library(tidyverse)
library(reshape)
nthread <- parallel::detectCores(all.tests = FALSE, logical = TRUE)

ev_sepbl <- read.csv("1-IndividualLevel/exploration_validation_splitting.csv")

ds <- read.csv("data/synthetic_data.csv") %>% 
  tbl_df() %>%
  filter(ev_sepbl$exploration) %>% # only take the exploration set here
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
  
dir.create("1-IndividualLevel/estimates", showWarnings = FALSE)
dir.create("1-IndividualLevel/PDPestimates", showWarnings = FALSE)
for (estimator_i in 1:length(estimator_grid)) {
  for (slb in c(FALSE, TRUE)) {
    # estimator_i = 13; slb = FALSE
    print(paste("# Estimator = ", estimator_i))
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
    
    # Insample Predictions -----------------------------------------------------
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
    
    # PDP predictions ----------------------------------------------------------
    # This is needed to create the PDP plots we are using later.
    for (ft in colnames(feat_to_use)) {
      # ft = colnames(feat_to_use)[1]
      print(paste("Running", ft))
      ## Generate a data set which contains all units with all possible feature
      ## settings for ft.
      if (is.integer(feat_to_use[[ft]]) | is.factor(feat_to_use[[ft]])) {
        #
        grid <- sort(unique(feat_to_use[[ft]]))
        
      } else {
        # for a numeric go through all the points from min to max in 100 steps
        grid <- seq.int(from = min(feat_to_use[[ft]]), 
                        to = max(feat_to_use[[ft]]), 
                        length.out = 50)
      }
      
      pdp_feat_list <- list()
      for (val in grid) {
        # val = grid[2]
        pdp_feat_list[[as.character(val)]] <- feat_to_use
        pdp_feat_list[[as.character(val)]][,ft] <- val
      }
      pdp_feat <- do.call(rbind, pdp_feat_list)
      if (is.character(pdp_feat[[ft]])) {
        pdp_feat[[ft]] <- factor(pdp_feat[[ft]])
      }
      PDPestimates_i <-
        tryCatch({
          CATEpredictor(L, pdp_feat)
        },
        error = function(e) {
          print(e)
          warning(paste("Something went wrong with", estimator_name, 
                        "when doing the PDP stuff"))
          return(NA)
        })
      PDPestimates_i_summary <- cbind(pdp_feat[, ft], PDPestimates_i) %>% 
        group_by_(ft) %>% 
        summarise(estimate = mean(PDPestimates_i), 
                  sd = sd(PDPestimates_i))
      PDPestimates_i_summary <- as.data.frame(PDPestimates_i_summary)
      
      rownames(PDPestimates_i_summary) <- 1:nrow(PDPestimates_i_summary)
      head(PDPestimates_i_summary)
      
      
      write.csv(
        PDPestimates_i_summary,
        file = paste0(
          "1-IndividualLevel/PDPestimates/",
          ft,
          "_",
          estimator_name,
          feat_var_name,
          ".csv"
        )
      )
      
    }
    print(paste("      Done with ",
                estimator_name, slb))
  }
}



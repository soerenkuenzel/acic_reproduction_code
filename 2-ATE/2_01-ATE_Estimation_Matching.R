setwd("/Users/soeren/Dropbox/ACIC_workshop_paper/Code/analysis/")
library(tidyverse)
library(reshape)
library(Matching)

ds <- read.csv("data/synthetic_data.csv") %>% 
  tbl_df()   

feat <- ds[, c("S3", "C1", "C2", "C3", "XC", "X1", "X2", "X3", "X4", "X5")]
Y <- ds$Y
W <- ds$Z

# Matching exactly on School ---------------------------------------------------
mm <- Match(Y = ds$Y, 
            Tr = ds$Z, 
            X = ds[ ,c(1,4:13)], 
            exact = c(TRUE, rep(FALSE, 10)), 
            estimand = "ATE")

stopifnot(ds$schoolid[mm$index.control] == ds$schoolid[mm$index.treated])
fake_ite <- ds$Y[mm$index.treated] - ds$Y[mm$index.control]

(ATE <- mean(fake_ite))

mm$est
mm$se


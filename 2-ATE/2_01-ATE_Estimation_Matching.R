setwd("~/Dropbox/ACIC_workshop_paper/Code/analysis/")
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
            X = ds[ ,c(1,4:7)], 
            exact = c(TRUE, rep(FALSE, 10)), 
            estimand = "ATE", Weight = 2, replace = TRUE)

stopifnot(ds$schoolid[mm$index.control] == ds$schoolid[mm$index.treated])
fake_ite <- ds$Y[mm$index.treated] - ds$Y[mm$index.control]

(ATE <- mean(fake_ite))

mm$est
mm$se

c(
  lowerCI = mm$est - 1.96 * mm$se, 
  est = mm$est,
  upperCI = mm$est + 1.96 * mm$se
)


# Assessing sensitivity of matching estimators. 

library(sensitivitymv)

matched_dd <- data.frame(trt = ds$Y[mm$index.treated], ctrl = ds$Y[mm$index.control])

matched_pairs <- cbind(matched_dd$trt,  matched_dd$ctrl)

uniroot(function(gamma) 
  sensitivitymv::senmv(matched_pairs, gamma = gamma, trim = 2.5, method = "t")$pval - 0.05,
  lower = 1, upper = 50
)

amplify()


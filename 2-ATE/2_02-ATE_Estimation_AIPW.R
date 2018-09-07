setwd("~/Dropbox/ACIC_workshop_paper/Code/analysis/")
library(tidyverse)
library(reshape)
library(Matching)

ds <- read.csv("data/synthetic_data.csv") %>% 
  tbl_df()   

feat <- ds[, c("S3", "C1", "C2", "C3", "XC", "X1", "X2", "X3", "X4", "X5")]
Y <- ds$Y
W <- ds$Z

# AIPW estimate ----------------------------------------------------------------
library(CausalGAM)

################################################################################
# Is that the right way of computing the AIPW, I cannot use C2 and C3 here ...
# See below for an alternative way of doing it
################################################################################

es <- estimate.ATE(
  pscore.formula = 
    W ~ s(S3) + s(C1) + s(XC) + s(X1) + s(X2) + s(X3) + s(X4) + s(X5),
  pscore.family = binomial,
  outcome.formula.t = 
    Y ~ s(S3) + s(C1) +  s(XC) + s(X1) + s(X2) + s(X3) + s(X4) + s(X5),
  outcome.formula.c = 
    Y ~ s(S3) + s(C1)  +  s(XC) + s(X1) + s(X2) + s(X3) + s(X4) + s(X5),
  outcome.family = gaussian,
  treatment.var = "W",
  data = data.frame(feat %>% mutate(C1 = as.numeric(C1), 
                                    CC = as.numeric(C2) * as.numeric(C3)), 
                    W, 
                    Y),
  divby0.action = "t",
  divby0.tol = 0.01,
  var.gam.plot = FALSE,
  nboot = 500
)

list(
  AIPW = c(
    "lowerCI" = es$ATE.AIPW.hat - 2 * es$ATE.AIPW.asymp.SE,
    "mean" = es$ATE.AIPW.hat, 
    "upperCI" = es$ATE.AIPW.hat + 2 * es$ATE.AIPW.asymp.SE
  ),
  REG = c(
    "lowerCI" = es$ATE.reg.hat - 2 * es$ATE.reg.asymp.SE,
    "mean" = es$ATE.reg.hat, 
    "upperCI" = es$ATE.reg.hat + 2 * es$ATE.reg.asymp.SE
  ),
  IPW = c(
    "lowerCI" = es$ATE.IPW.hat - 2 * es$ATE.IPW.asymp.SE,
    "mean" = es$ATE.IPW.hat, 
    "upperCI" = es$ATE.IPW.hat + 2 * es$ATE.IPW.asymp.SE
  )
)


stop('Stop here!!!!!!!!!!!!!!!!!!')

# AIPW alternative way ---------------------------------------------------------
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

feat <- ds[, c("S3", "C1", "C2", "C3", "XC", "X1", "X2", "X3", "X4", "X5")]
Y <- ds$Y
W <- ds$Z
AIPW <- function(these_samples) {
  e <- ranger::ranger(y ~ ., 
                      data = data.frame(y = W[these_samples], 
                                        feat[these_samples,]),
                      write.forest = TRUE)
  
  m <- ranger::ranger(y ~ ., 
                      data = data.frame(y = Y[these_samples], 
                                        feat[these_samples,], 
                                        W[these_samples]),
                      write.forest = TRUE)
  these_treated <- W[these_samples] == 1
  
  return(
    (
      sum(
        Y[these_samples & W == 1] / e$predictions[these_treated] - 
          (1 - e$predictions[these_treated]) / e$predictions[these_treated] * 
          m$predictions[these_treated]
      ) + 
        sum(m$predictions[!these_treated])
    ) / sum(these_samples) - 
      (
        sum(Y[these_samples & W == 0] / (1 - e$predictions[!these_treated]) + 
              (e$predictions[!these_treated]) / 
              (1 - e$predictions[!these_treated]) * 
              m$predictions[!these_treated]
        ) - 
          sum(m$predictions[these_treated])
      ) / 
      sum(these_samples)
  )
}



# Random forest estimate of ATE:



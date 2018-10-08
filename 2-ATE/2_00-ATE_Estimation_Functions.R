library(sensitivitymv)
library(CausalGAM)
library(Matching)

ATE_estimation <- function(Y, Z, feat, nboot = 500, non_smooth_vars = c("C2", "C3")) {
  reg_vars <- c("C1", "C2", "C3","S3", "XC", "X1", "X2", "X3", "X4", "X5")
  match_X <- feat[, c("schoolid", "S3","C1", "C2", "C3")]
  full_X <- feat[, names(feat) %in% reg_vars]
  
  
  # Matching estimator
  # Matching exactly on School ----------------------------------------------------
  mm <- Match(Y = Y, 
              Tr = Z, 
              X = match_X, 
              exact = c(TRUE, rep(FALSE, 4)), 
              estimand = "ATE", Weight = 2, replace = TRUE)
  
  
  stopifnot(match_X$schoolid[mm$index.control] == match_X$schoolid[mm$index.treated])
  fake_ite <- Y[mm$index.treated] - Y[mm$index.control]
  
  (ATE <- mean(fake_ite))
  # CausalGAM estimators  ---------------------------------------------------------
  
  # AIPW estimate ----------------------------------------------------------------
  
  ################################################################################
  # Is that the right way of computing the AIPW, I cannot use C2 and C3 here ...
  # See below for an alternative way of doing it -- SRK
  # I think it is OK -- SW
  ################################################################################
  s_str <- function(var) {
    paste0("s(", var, ")")
  }
  make_formula <- function(response, non_smooth_vars, reg_vars) {
    smooth_vars <- setdiff(reg_vars, non_smooth_vars)
    gam_terms <- paste(c(sapply(smooth_vars, s_str), non_smooth_vars), sep = " + ")
    return(as.formula(paste0(response, " ~ ", gam_terms)))
  }
  
  es <- estimate.ATE(
    pscore.formula = make_formula("Z", non_smooth_vars, reg_vars),
    pscore.family = binomial,
    outcome.formula.t = 
      make_formula("Y", non_smooth_vars, reg_vars),
    outcome.formula.c = 
      make_formula("Y", non_smooth_vars, reg_vars),
    outcome.family = gaussian,
    treatment.var = "Z",
    data = data.frame(full_X %>% mutate(C1 = as.numeric(C1), 
                                      CC = as.numeric(C2) * as.numeric(C3)), 
                      Z, 
                      Y),
    divby0.action = "t",
    divby0.tol = 0.01,
    var.gam.plot = FALSE,
    nboot = nboot
  )
  
  # Combining and outputting estimators---------------------------------------------
  
  out <- list(
    AIPW = c(
      "lowerCI" = es$ATE.AIPW.hat - 2 * es$ATE.AIPW.asymp.SE,
      "mean" = es$ATE.AIPW.hat, 
      "upperCI" = es$ATE.AIPW.hat + 2 * es$ATE.AIPW.asymp.SE,
      se = es$ATE.AIPW.asymp.SE
    ),
    REG = c(
      "lowerCI" = es$ATE.reg.hat - 2 * es$ATE.reg.asymp.SE,
      "mean" = es$ATE.reg.hat, 
      "upperCI" = es$ATE.reg.hat + 2 * es$ATE.reg.asymp.SE,
      se = es$ATE.reg.asymp.SE
    ),
    IPW = c(
      "lowerCI" = es$ATE.IPW.hat - 2 * es$ATE.IPW.asymp.SE,
      "mean" = es$ATE.IPW.hat, 
      "upperCI" = es$ATE.IPW.hat + 2 * es$ATE.IPW.asymp.SE,
      se = es$ATE.IPW.asymp.SE
    ),
    Matching = c(
      c(
        "lowerCI" = mm$est - 2 * mm$se, 
        "mean" = mm$est,
        "upperCI" = mm$est + 2 * mm$se,
        se = mm$se
      )
      
    )
  )
  return(out)
}
# Assessing sensitivity of matching estimators. 


matching_sensitivity <- function(Y,Z,feat) {
  match_X <- feat[, c("schoolid", "S3", "C1", "C2", "C3")]
  mm <- Match(Y = Y, 
              Tr = Z, 
              X = match_X, 
              exact = c(TRUE, rep(FALSE, 4)), 
              estimand = "ATE", Weight = 2, replace = TRUE)
  stopifnot(match_X$schoolid[mm$index.control] == match_X$schoolid[mm$index.treated])
  matched_pairs <- cbind(Y[mm$index.treated], Y[mm$index.control])

  out <- uniroot(function(gamma) 
    sensitivitymv::senmv(matched_pairs, gamma = gamma, trim = 2.5, method = "t")$pval - 0.05,
    lower = 1, upper = 50
  )
  return(out)
}

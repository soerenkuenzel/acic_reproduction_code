library(causalToolbox)

# ------------------------------------------------------------------------------
# CF helper for numeric features:

make_one_hot <- function(feat_tmn) {
  numeric_cols <- NULL
  for (col_i in 1:ncol(feat_tmn)) {
    if (is.numeric(as.data.frame(feat_tmn)[, col_i])) {
      numeric_cols <- c(numeric_cols, col_i)
    }
  }
  factor_cols <- (1:ncol(feat_tmn))[-numeric_cols]
  feat_nm <- feat_tmn[ , numeric_cols]
  
  for (col_i in factor_cols) {
    # col_i = 2
    lvls <- levels(factor(as.data.frame(feat_tmn)[, col_i]))
    for (lvl in lvls) {
      # lvl = lvls[2]
      cname <- paste0(colnames(feat_tmn)[col_i], "_", lvl)
      vec <- as.numeric(feat_tmn[, col_i] == lvl)
      feat_nm <- cbind(feat_nm, vec)
      colnames(feat_nm)[ncol(feat_nm)] <- cname
    }
  }
  return(feat_nm)
}

# ------------------------------------------------------------------------------
# Define estimators:
estimator_grid <- list(
  "S_RF" = function(feat, W, Yobs)
    S_RF(feat, W, Yobs),
  "S_RF2" = function(feat, W, Yobs)
    S_RF(feat, W, Yobs, mtry = 1),
  "S_RF3" = function(feat, W, Yobs)
    S_RF(feat, W, Yobs, mtry = 5, alwaysTr = FALSE),
  "S_RF4" = function(feat, W, Yobs)
    S_RF(feat, W, Yobs, mtry = 3, alwaysTr = FALSE, splitratio = 1),

  "T_RF" = function(feat, W, Yobs)
    T_RF(feat, W, Yobs),
  "T_RF2" = function(feat, W, Yobs)
    T_RF(feat, W, Yobs, mtry = 1),
  "T_RF3" = function(feat, W, Yobs)
    T_RF(feat, W, Yobs, mtry = 1, splitratio = 0.5),
  "T_RF4" = function(feat, W, Yobs)
    T_RF(feat, W, Yobs, splitratio = 1),


  "X_RF" = function(feat, W, Yobs)
    X_RF(feat, W, Yobs, verbose = FALSE),
  "X_RF2" = function(feat, W, Yobs)
    X_RF(
      feat,
      W,
      Yobs,
      verbose = FALSE,
      mtry_first = 1,
      mtry_second = 1,
      min_node_size_ave_first = 1,
      min_node_size_spl_second = 1,
      min_node_size_ave_second = 1,
      min_node_size_spl_prop = 1,
      min_node_size_ave_prop = 1
    ),
  "X_RF3" = function(feat, W, Yobs)
    X_RF(
      feat,
      W,
      Yobs,
      verbose = FALSE,
      mtry_first = 3,
      mtry_second = 3,
      min_node_size_ave_first = 1,
      min_node_size_spl_second = 20,
      min_node_size_ave_second = 20,
      min_node_size_spl_prop = 20,
      min_node_size_ave_prop = 20
    ),
  "X_RF4" = function(feat, W, Yobs)
    X_RF(
      feat,
      W,
      Yobs,
      verbose = FALSE,
      sample_fraction_first = 1,
      sample_fraction_second = 1,
      sample_fraction_prop = 1
    ),
  "CF" = function(feat, W, Yobs) {
    # feat <- feat %>%  mutate(
    #   C1 = as.numeric(C1),
    #   C2 = as.numeric(C2),
    #   C3 = as.numeric(C3)
    # )
    feat <- make_one_hot(feat)
    feat <- as.matrix(feat)
    colnames(feat) <- NULL
    grf::causal_forest(
      X = feat,
      Y = Yobs,
      W = W,
      num.trees = 500,
      num.threads = nthread
    )
  },
  "CF2" = function(feat, W, Yobs) {
    # feat <- feat %>%  mutate(
    #   C1 = as.numeric(C1),
    #   C2 = as.numeric(C2),
    #   C3 = as.numeric(C3)
    # )
    feat <- make_one_hot(feat)
    feat <- as.matrix(feat)
    colnames(feat) <- NULL
    grf::causal_forest(
      X = feat,
      Y = Yobs,
      W = W,
      num.trees = 500,
      num.threads = nthread,
      mtry = 1,
      min.node.size = 1,
      honesty = FALSE
    )
  },
  "CF3" = function(feat, W, Yobs) {
    # feat <- feat %>%  mutate(
    #   C1 = as.numeric(C1),
    #   C2 = as.numeric(C2),
    #   C3 = as.numeric(C3)
    # )
    feat <- make_one_hot(feat)
    feat <- as.matrix(feat)
    colnames(feat) <- NULL
    grf::causal_forest(
      X = feat,
      Y = Yobs,
      W = W,
      num.trees = 500,
      num.threads = nthread,
      mtry = 10,
      min.node.size = 10,
      honesty = TRUE
    )
  },
  "S_BART" = function(feat, W, Yobs)
    S_BART(feat, W, Yobs, verbose = TRUE),
  "T_BART" = function(feat, W, Yobs)
    T_BART(feat, W, Yobs, verbose = TRUE),
  "X_BART" = function(feat, W, Yobs)
    X_BART(feat, W, Yobs),
  "R_BOOST" = function(feat, W, Yobs) {
    feat <- make_one_hot(feat)
    feat <- as.matrix(feat)
    colnames(feat) <- NULL
    W <- as.logical(W)
    rlearner::rboost(feat, W, Yobs,
                     nthread = nthread)
    }
)
CATEpredictor_grid <- list(
  "S_RF" = function(estimator, feat_te)
    causalToolbox::EstimateCate(estimator, feat_te),
  "S_RF2" = function(estimator, feat_te)
    causalToolbox::EstimateCate(estimator, feat_te),
  "S_RF3" = function(estimator, feat_te)
    causalToolbox::EstimateCate(estimator, feat_te),
  "S_RF4" = function(estimator, feat_te)
    causalToolbox::EstimateCate(estimator, feat_te),
  "T_RF" = function(estimator, feat_te)
    causalToolbox::EstimateCate(estimator, feat_te),
  "T_RF2" = function(estimator, feat_te)
    causalToolbox::EstimateCate(estimator, feat_te),
  "T_RF3" = function(estimator, feat_te)
    causalToolbox::EstimateCate(estimator, feat_te),
  "T_RF4" = function(estimator, feat_te)
    causalToolbox::EstimateCate(estimator, feat_te),
  "X_RF" = function(estimator, feat_te)
    causalToolbox::EstimateCate(estimator, feat_te),
  "X_RF2"  = function(estimator, feat_te)
    causalToolbox::EstimateCate(estimator, feat_te),
  "X_RF3" = function(estimator, feat_te)
    causalToolbox::EstimateCate(estimator, feat_te),
  "X_RF4" = function(estimator, feat_te)
    causalToolbox::EstimateCate(estimator, feat_te),
  "S_BART" = function(estimator, feat_te)
    causalToolbox::EstimateCate(estimator, feat_te),
  "T_BART" = function(estimator, feat_te)
    causalToolbox::EstimateCate(estimator, feat_te),
  "X_BART" = function(estimator, feat_te)
    causalToolbox::EstimateCate(estimator, feat_te),
  "CF" = function(estimator, feat_te)  {
    # feat_te <- feat_te %>%  mutate(
    #   C1 = as.numeric(C1),
    #   C2 = as.numeric(C2),
    #   C3 = as.numeric(C3)
    # )
    feat_te <- make_one_hot(feat_te)
    feat_te <- as.matrix(feat_te)
    colnames(feat_te) <- NULL
    return(predict(estimator, feat_te)$predictions)
  },
  "CF2" = function(estimator, feat_te)  {
    # feat_te <- feat_te %>%  mutate(
    #   C1 = as.numeric(C1),
    #   C2 = as.numeric(C2),
    #   C3 = as.numeric(C3)
    # )
    feat_te <- make_one_hot(feat_te)
    feat_te <- as.matrix(feat_te)
    colnames(feat_te) <- NULL
    return(predict(estimator, feat_te)$predictions)
  },
  "CF3" = function(estimator, feat_te)  {
    # feat_te <- feat_te %>%  mutate(
    #   C1 = as.numeric(C1),
    #   C2 = as.numeric(C2),
    #   C3 = as.numeric(C3)
    # )
    feat_te <- make_one_hot(feat_te)

    feat_te <- as.matrix(feat_te)
    colnames(feat_te) <- NULL
    return(predict(estimator, feat_te)$predictions)
  },
  "R_BOOST" = function(estimator, feat_te) {
    feat_te <- make_one_hot(feat_te)

    feat_te <- as.matrix(feat_te)
    colnames(feat_te) <- NULL
    return(rlearner:::predict.rboost(estimator, feat_te))
  }
)

# ------------------------------------------------------------------------------
# 


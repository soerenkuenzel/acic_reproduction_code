setwd("/Users/soeren/Dropbox/ACIC_workshop_paper/Code/analysis/")
library(tidyverse)
library(reshape)
#  Read in the data ------------------------------------------------------------
files <- dir("1-IndividualLevel/estimates/")
estimate_list <- list()
for (estim_file in files) {
  # estim_file = files[1]
  vec_toadd <- read.csv(paste0("1-IndividualLevel/estimates/", estim_file))
  
  estimate_list[[colnames(vec_toadd)[2]]] <- vec_toadd[, 2, drop = FALSE]
}
estimates <- do.call(cbind, estimate_list)

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



# Plot the Fixed Mindset Base Line ---------------------------------------------

for (this_feat in c("S3", "C1", "C2", "C3", "XC")) { # this_feat <- "XC"
  print(this_feat)

  # Generate marginal plot -----------------------------------------------------
  p1_marginal_pd <- cbind(ft = ds[[this_feat]], estimates) %>%
    melt(id = "ft") %>%
    dplyr::rename(Estimator = variable, 
                  CATE = value) %>%
    mutate(Estimator =
             factor(
               paste("Estimator", as.numeric(Estimator)),
               levels = paste("Estimator", 1:length(unique(Estimator)))
             )) %>% 
    tbl_df() 
  
  (p1_marginal <- p1_marginal_pd %>% 
    group_by(ft, Estimator) %>%
    summarize(
      CATEse = sd(CATE),
      CATE = mean(CATE)) %>%
    ungroup() %>%
    ggplot() +
    geom_line(aes(x = as.numeric(as.factor(ft)), y = CATE, color = Estimator)) +
    theme_minimal() +
    xlab(this_feat)  +
    theme(legend.position = "none") +
      ylab("marginal CATE")
  )
  

  # Generate density plot ------------------------------------------------------
  num_of_fktrs <- length(unique(ds[[this_feat]]))
  (p1_count <- p1_marginal_pd %>% 
      ggplot(aes(x = as.numeric(as.factor(ft)))) +
      geom_histogram(bins = 2 * num_of_fktrs - 1) +
      theme_minimal() +
      xlab(this_feat)  +
      theme(legend.position = "none") +
      scale_y_continuous(breaks = c(25000, 50000), labels = c("25", "50")) +
      ylab("Count in 1000"))
  
  # Generate PDP plot ----------------------------------------------------------
  
  files <- grep(paste0(this_feat, "_(.*)"), dir("1-IndividualLevel/PDPestimates/"), value = TRUE)
  estimate_list <- list()
  for (estim_file in files) {
    # estim_file = files[1]
    vec_toadd <- read.csv(paste0("1-IndividualLevel/PDPestimates/", estim_file))
    colnames(vec_toadd)[3] <- estim_file
    
    estimate_list[[estim_file]] <- vec_toadd[, 3, drop = FALSE]
  }
  PDPestimates <- do.call(cbind, estimate_list)
  
  p1_pdp_pd <- data.frame(ft = vec_toadd[ ,2], PDPestimates) %>% 
    melt(id = "ft") %>%
    dplyr::rename(Estimator = variable, CATE = value) %>%
    mutate(Estimator =
             factor(
               paste("Estimator", as.numeric(Estimator)),
               levels = paste("Estimator", 1:length(unique(Estimator)))
             )) %>%
    tbl_df()
  (
    p1_pdp <- p1_pdp_pd %>%
      group_by(ft, Estimator) %>%
      summarize(
        CATEse = sd(CATE),
        CATE = mean(CATE)) %>%
      ungroup() %>%
      ggplot() +
      geom_line(aes(x = as.numeric(as.factor(ft)), y = CATE, color = Estimator)) +
      theme_minimal() +
      xlab(this_feat)  +
      theme(legend.position = "none") +
      ylab("Partial Dependence CATE Plot")
  )
  
  
  
  
  library(gridExtra)
  library(grid)
  library(ggplot2)
  library(lattice)
  library(gtable)
  
  p1_marginalGP <- ggplotGrob(p1_marginal + 
                                theme(axis.title.x = element_blank(),
                                      axis.text.x = element_blank(),
                                      axis.ticks.x = element_blank()) +
                                coord_cartesian(xlim = c(1, num_of_fktrs)))
  p1_pdpGP <-  ggplotGrob(p1_pdp + 
                            theme(axis.title.x = element_blank(),
                                  axis.text.x = element_blank(),
                                  axis.ticks.x = element_blank()) +
                            coord_cartesian(xlim = c(1, num_of_fktrs)))
  
  
  xlabel <- switch(this_feat, 
           "XC" = "Urbanicity", 
           "S3" = "Students' self-reported expectations", 
           this_feat)
  
  x_tic_labels <- sort(unique(ds[[this_feat]]))
  names(x_tic_labels) <- 1:length(x_tic_labels)
  
  p1_countGP <- 
    ggplotGrob(p1_count +
                 coord_cartesian(xlim = c(1, num_of_fktrs)) +
                 xlab(xlabel) +
                 scale_x_continuous(breaks = as.numeric(names(x_tic_labels)),
                                    labels = x_tic_labels))
  
  maxWidth = grid::unit.pmax(p1_marginalGP$widths[2:5], 
                             p1_countGP$widths[2:5], 
                             p1_pdpGP$widths[2:5])
  p1_marginalGP$widths[2:5] <- as.list(maxWidth)
  p1_countGP$widths[2:5] <- as.list(maxWidth)
  p1_pdpGP$widths[2:5] <- as.list(maxWidth)
  
  pdf(file = paste0("3-heterogeneity/discrete_heterogeneity/", this_feat, "_cmb.pdf"),
      width = 5, height = 7)  
  grid.arrange(p1_marginalGP, p1_pdpGP, p1_countGP, ncol = 1, heights = c(3, 3, 1))
  dev.off()
}


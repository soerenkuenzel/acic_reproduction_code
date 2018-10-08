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

p1_marginal_pd <- cbind(X1 = ds$X1, estimates) %>%
  melt(id = "X1") %>%
  dplyr::rename(Estimator = variable, CATE = value) %>%
  mutate(Estimator =
           factor(
             paste("Estimator", as.numeric(Estimator)),
             levels = paste("Estimator", 1:length(unique(Estimator)))
           )) %>% 
  tbl_df() 
(
  p1_marginal <- p1_marginal_pd %>%
    ggplot() +
    geom_smooth(aes(
      x = X1, y = CATE, color = Estimator
    ), se = FALSE) +
    theme_minimal() +
    xlab("School-level mean of students' fixed mindsets")  +
    geom_vline(xintercept = c(0.15), linetype = 2) +
    coord_cartesian(ylim = c(0, .33)) +
    theme(legend.position = "none")
)

ggsave(plot = p1_marginal, 
       filename = "../../ACIC-paper/figure/Figure2-FixedMindesetCATE.pdf", 
       width = 8,
       height = 5)

# Plot SChool level achievement ------------------------------------------------
# cbind(X2 = seq(min(ds2$X2), max(ds2$X2), length.out = 100), estimates_ds) %>%

p2_marginal_pd <- cbind(X2 = ds$X2, estimates) %>% 
  reshape::melt(id = c("X2")) %>% 
  dplyr::rename(Estimator = variable, 
                CATE = value) %>%
  mutate(Estimator = factor(paste("Estimator", as.numeric(Estimator)), 
                            levels = paste("Estimator", 
                                           1:length(unique(Estimator))))) %>%
  tbl_df()
(
  p2_marginal <- p2_marginal_pd %>%  
    group_by(X2, Estimator) %>%
    summarize(CATE = mean(CATE)) %>%
  ggplot(aes(x = X2, y = CATE, color = Estimator)) +
    geom_smooth(se = FALSE) +
    # geom_point() +
    theme_minimal() +
    xlab("School achievement level") +
    coord_cartesian(ylim = c(.1,.3)) +
    geom_vline(xintercept = c(-.8, 1.1), linetype = 2) + 
    geom_text(x = -2, y = .11, label = 'low', color = 'black') +
    geom_text(x = .4, y = .11, label = 'medium', color = 'black') +
    geom_text(x = 1.6, y = .11, label = 'high', color = 'black') +
    theme(legend.position = "none")
)
ggsave(plot = p2_marginal, 
       "../../ACIC-paper/figure/Figure3-SchoolLevelAchievment.pdf", 
       width = 8,
       height = 5)

setwd("/Users/soeren/Dropbox/ACIC_workshop_paper/Code/analysis/")
library(tidyverse)
library(reshape)
# PDP for the Fixed Mindset Base Line ------------------------------------------

files <- grep("X1_(.*)", dir("1-IndividualLevel/PDPestimates/"), value = TRUE)
estimate_list <- list()
for (estim_file in files) {
  # estim_file = files[1]
  vec_toadd <- read.csv(paste0("1-IndividualLevel/PDPestimates/", estim_file))
  colnames(vec_toadd)[3] <- estim_file
  
  estimate_list[[estim_file]] <- vec_toadd[, 3, drop = FALSE]
}
estimates <- do.call(cbind, estimate_list)
p1_pdp_pd <- cbind(vec_toadd[ ,2, drop = FALSE], estimates) %>% 
  melt(id = "X1") %>%
  dplyr::rename(Estimator = variable, CATE = value) %>%
  mutate(Estimator =
           factor(
             paste("Estimator", as.numeric(Estimator)),
             levels = paste("Estimator", 1:length(unique(Estimator)))
           )) %>%
  tbl_df()
(
  p1_pdp <- p1_pdp_pd %>%
  ggplot() +
  geom_smooth(aes(x = X1, y = CATE, color = Estimator), se = FALSE) +
  theme_minimal() +
  xlab("School-level mean of students' fixed mindsets")  +
  geom_vline(xintercept = c(0.1), linetype = 2) +
  # coord_cartesian(ylim = c(0, .33)) +
  theme(legend.position = "none") + 
  ggtitle("Partial Dependence Plot")
)

ggsave(filename = "../../ACIC-paper/figure/Figure2b-FixedMindesetCATE_PDP.pdf", 
       width = 8,
       height = 5)


# PDP for the SChool level achievement -----------------------------------------

files <- grep("X2_(.*)", dir("1-IndividualLevel/PDPestimates/"), value = TRUE)
estimate_list <- list()
for (estim_file in files) {
  # estim_file = files[1]
  vec_toadd <- read.csv(paste0("1-IndividualLevel/PDPestimates/", estim_file))
  colnames(vec_toadd)[3] <- estim_file
  
  estimate_list[[estim_file]] <- vec_toadd[, 3, drop = FALSE]
}
estimates <- do.call(cbind, estimate_list)

p2_pdp_pd <- cbind(vec_toadd[ ,2, drop = FALSE], estimates) %>% 
  melt(id = "X2") %>%
  dplyr::rename(Estimator = variable, CATE = value) %>%
  mutate(Estimator =
           factor(
             paste("Estimator", as.numeric(Estimator)),
             levels = paste("Estimator", 1:length(unique(Estimator)))
           )) %>%
  tbl_df()
(
  p2_pdp <- p2_pdp_pd %>%
  ggplot() +
  geom_smooth(aes(x = X2, y = CATE, color = Estimator), se = FALSE) +
  theme_minimal() +
  xlab("School achievement level") +
  geom_vline(xintercept = c(-.3, 1.1), linetype = 2) + 
  geom_text(x = -2, y = .11, label = 'low', color = 'black') +
  geom_text(x = .4, y = .11, label = 'medium', color = 'black') +
  geom_text(x = 1.6, y = .11, label = 'high', color = 'black') +
  # coord_cartesian(ylim = c(0, .33)) +
  theme(legend.position = "none") + 
  ggtitle("Partial Dependence Plot")
)

ggsave("../../ACIC-paper/figure/Figure3b-SchoolLevelAchievment_PDP.pdf", 
       width = 8,
       height = 5)



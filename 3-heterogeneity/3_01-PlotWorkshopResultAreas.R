#  Read in the data ------------------------------------------------------------
files <- dir("1-IndividualLevel/estimates/")
estimate_list <- list()
for (estim_file in files) {
  # estim_file = files[1]
  vec_toadd <- read.csv(paste0("1-IndividualLevel/estimates/", estim_file))
  
  estimate_list[[colnames(vec_toadd)[2]]] <- vec_toadd[, 2, drop = FALSE]
}
estimates <- do.call(cbind, estimate_list)

ds <- read.csv("data/synthetic_data.csv") %>% tbl_df() %>%
  mutate(
    schoolid = factor(schoolid),
    C1 = factor(C1),
    C2 = factor(C2),
    C3 = factor(C3)
  ) 

# Plot the Fixed Mindset Base Line ---------------------------------------------

cbind(X1 = ds$X1, estimates) %>%
  melt(id = "X1") %>%
  dplyr::rename(Estimator = variable, CATE = value) %>%
  mutate(Estimator =
           factor(
             paste("Estimator", as.numeric(Estimator)),
             levels = paste("Estimator", 1:length(unique(Estimator)))
           )) %>%
  ggplot() +
  geom_smooth(aes(x = X1, y = CATE, color = Estimator), se = FALSE) +
  theme_minimal() +
  xlab("School-level mean of students' fixed mindsets")  +
  geom_vline(xintercept = c(0.1), linetype = 2) +
  coord_cartesian(ylim = c(0, .33)) +
  theme(legend.position = "none")

ggsave("../../ACIC-paper/figure/Figure2-FixedMindesetCATE.pdf", 
       width = 8,
       height = 5)
       
# Plot SChool level achievement ------------------------------------------------
# cbind(X2 = seq(min(ds2$X2), max(ds2$X2), length.out = 100), estimates_ds) %>%
cbind(X2 = ds$X2, estimates) %>% 
  reshape::melt(id = c("X2")) %>% 
  dplyr::rename(Estimator = variable, CATE = value, School_achievement_level = X2) %>%
  mutate(Estimator = factor(paste("Estimator", as.numeric(Estimator)), 
                            levels = paste("Estimator", 1:length(unique(Estimator))))) %>% 
  tbl_df() %>%
  group_by(School_achievement_level, Estimator) %>%
  summarize(CATE = mean(CATE)) %>%
  ggplot(aes(x = School_achievement_level, y = CATE, color = Estimator)) +
  geom_smooth(se = FALSE) +
  # geom_point() +
  theme_minimal() +
  xlab("School achievement level") +
  coord_cartesian(ylim = c(.1,.3)) +
  geom_vline(xintercept = c(-.3, 1.1), linetype = 2) + 
  geom_text(x = -2, y = .11, label = 'low', color = 'black') +
  geom_text(x = .4, y = .11, label = 'medium', color = 'black') +
  geom_text(x = 1.6, y = .11, label = 'high', color = 'black') +
  theme(legend.position = "none")

ggsave("../../ACIC-paper/figure/Figure3-SchoolLevelAchievment.pdf", 
       width = 8,
       height = 5)




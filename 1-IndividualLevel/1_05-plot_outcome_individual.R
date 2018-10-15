setwd("/Users/soeren/Dropbox/ACIC_workshop_paper/Code/analysis/")

library(tidyverse)
library(reshape)
# ------------------------------------------------------------------------------
# Read in all the files 
files <- dir("1-IndividualLevel/estimates/") 
estimate_list <- list() 
for (estim_file in files) {
  # estim_file = files[1]
  vec_toadd <- read.csv(paste0("1-IndividualLevel/estimates/", estim_file))
  
  estimate_list[[colnames(vec_toadd)[2]]] <-  vec_toadd[ , 2, drop = FALSE]
}
estimates <- do.call(cbind, estimate_list)

unsure_units <- sort(c(head(order(apply(estimates, 1, sd)), 6), 
                       tail(order(apply(estimates, 1, mean)), 5)))

unsure_units <- sort(c(3942, 671, 2857, 10107, 1746, 7313, 2319, 10251, 692, 4932))
unsure_units <- sort(c(326, 491, 2387, 4626, 1915, 1011, 995, 2416, 3482, 3822))


prepared_data <- data.frame(id = 1:nrow(estimates),
           estimates)  %>% 
  melt(id = "id") %>%
  filter(id %in% unsure_units) %>%
  mutate(id = factor(id)) %>%
  dplyr::rename(Estimator = variable) %>%
  mutate(noschoolid = grepl("everythingButSchoolId", Estimator), 
         learner = gsub("(.*)_everythingButSchoolId", "\\1", Estimator), 
         learner = gsub("(.*)_SchoolIdNoSchoolVars", "\\1", learner), 
         learnermain = gsub("(.*)[23456]", "\\1", learner)
         ) %>%
  tbl_df()


# Figure -- raw
prepared_data %>%
  ggplot(aes(x = id, y = value)) +
  geom_point() +
  ylab("CATE estimate") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  coord_cartesian(ylim = c(-1, 1))
ggsave("1-IndividualLevel/Estimator_disagreement_individual_raw.pdf", 
       width = 8, height = 4)
ggsave("../../ACIC-paper/figure/Estimator_disagreement_individual_raw.pdf", 
       width = 8, height = 4)

prepared_data %>%
  ggplot(aes(x = id, y = value, color = noschoolid, shape = substr(learnermain,1,1))) +
  geom_point() +
  ylab("CATE estimate") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
  # theme(legend.position = "none") +
ggsave("1-IndividualLevel/Estimator_disagreement1_individual.png", 
       width = 8, height = 4) 



setwd("/Users/soeren/Dropbox/ACIC_workshop_paper/Code/analysis/")
library(tidyverse)

source("3-heterogeneity/3_01-PlotWorkshopResultMarginalPlots.R")
source("3-heterogeneity/3_02-PlotWorkshopResultPDP.R")
p1_marginal
p1_pdp

p2_marginal
p2_pdp

# plot jointly Version 1 -------------------------------------------------------
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)
p <- qplot(1,1)
p2 <- xyplot(1~1)
r <- rectGrob(gp = gpar(fill = "grey90"))
t <- textGrob("text")
pdf(file = "../../ACIC-paper/figure/hte_figues_cmb1.pdf", 
    width = 8, 
    height = 8)
grid.arrange(p1_marginal, p2_marginal, 
             p1_pdp,      p2_pdp, 
             ncol = 2)
dev.off()


# plot jointly Version 2 -------------------------------------------------------
(
p1_cmb <- rbind(p1_marginal_pd %>% mutate(type = "Emperical"), 
      p1_pdp_pd %>% mutate(type = "Partial Dependence Plot"))  %>%
  ggplot() +
  geom_smooth(aes(
      x = X1, y = CATE, color = Estimator
  ), se = FALSE) +
  theme_minimal() +
  xlab("School-level mean of students' fixed mindsets")  +
  geom_vline(xintercept = c(0.15), linetype = 2) +
  coord_cartesian(ylim = c(0, .33)) +
  theme(legend.position = "none") + 
  facet_grid(type~.) + theme(strip.text.y = element_blank())
)
  

p2_cmb <- rbind(p2_marginal_pd %>% mutate(type = "Emperical"), 
      p2_pdp_pd %>% mutate(type = "Partial Dependence Plot"))  %>%
  ggplot() +
  geom_smooth(aes(
    x = X2, y = CATE, color = Estimator
  ), se = FALSE) +
  theme_minimal() +
  xlab("School achievement level") +
  geom_vline(xintercept = c(-.3, 1.1), linetype = 2) + 
  geom_text(x = -2, y = .11, label = 'low', color = 'black') +
  geom_text(x = .4, y = .11, label = 'medium', color = 'black') +
  geom_text(x = 1.6, y = .11, label = 'high', color = 'black') +
  theme(legend.position = "none") + 
  facet_grid(type~.) +
  theme(axis.title.y = element_blank())

pdf(file = "../../ACIC-paper/figure/hte_figues_cmb2.pdf", 
    width = 8, 
    height = 8)
grid.arrange(p1_cmb, p2_cmb, 
             ncol = 2)
dev.off()

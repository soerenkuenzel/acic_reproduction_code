set.seed(734324633)
ds <- read.csv("data/synthetic_data.csv") 

eset_bl <- sample(c(TRUE, FALSE), size = nrow(ds), replace = TRUE)
vset_bl <- !eset_bl

ev_seperation <- 
  data.frame(
    idx = 1:nrow(ds), 
    exploration = eset_bl,
    validation = vset_bl
  )

write.csv(ev_seperation, 
          file = "1-IndividualLevel/exploration_validation_splitting.csv", 
          row.names = FALSE)

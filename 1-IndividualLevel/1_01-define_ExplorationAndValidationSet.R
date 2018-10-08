if (file.exists("~/Dropbox/ACIC_workshop_paper/Code/analysis/")) {
  setwd("~/Dropbox/ACIC_workshop_paper/Code/analysis/")
} else if (file.exists("~/acic_reproduction_code/")) {
  setwd("~/acic_reproduction_code/")
} else if (file.exists("~/acic_reproduction_code/")) {
  setwd("~/ACIC-code/")
} else {
  stop("Cannot set working directory!")
}
set.seed(734324633)
ds <- read.csv("data/synthetic_data.csv") 

schoolid_grid <- sort(unique(ds$schoolid))
exploration_set_school_ids <- 
  sample(x = schoolid_grid, size = round(length(schoolid_grid) / 2))

eset_bl <- ds$schoolid %in% exploration_set_school_ids
vset_bl <- !eset_bl

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

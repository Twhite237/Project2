nflData = read.csv(file = "data/nfldata.csv")
# Remove any cols with prob:


saveRDS(nflData, "data/nflDataClean.rds")


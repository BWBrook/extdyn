## Generation of simulated datasets based on JR2014 (function dataset.gen) ##
## iter = numer of simulated datasets generated, rel = type of reliability vector, based on set.rel function
## sight.type = type of sighting record (1 - constant detectability, 2 - declining detectability)
## rel.thresh = minimum sighting reliability accepted for standard extinction approaches
source("BBJ_sensitivity_functions_v2.2.r")
dat <- dataset.gen(iter=100, rel="A", sight.type=1, rel.thresh=0.8)

sight_records <- dat[[1]]; sight_reliab <- dat[[2]]; ext_year <- dat[[3]]

## Testing for extinction based on simulated datasets ##
## iter = numer of simulated datasets evaluated, iter.b = number of simulations used per each BBJ2019 run ##
## rel = type of reliability vector, based on set.rel function, rel.thresh = minimum sighting reliability accepted for standard extinction approaches ##
SA.results <- te.inference(iter=1000,iter.b=1000, rel.thresh=0.8, rel="A")
print(c(colMeans(SA.results[,1:4], na.rm=T), colMeans(sqrt(SA.results[,5:8]^2), na.rm=T), colMeans(SA.results[,9:12], na.rm=T)))
total <- c(colMeans(SA.results[,1:4], na.rm=T), colMeans(sqrt(SA.results[,5:8]^2), na.rm=T), colMeans(SA.results[,9:12], na.rm=T))
write.csv(SA.results, file="SA_scenario.csv")
write.csv(total, file="SA_results.csv")

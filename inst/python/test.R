
## ---load packages and initialize----
library(sarp.snowprofile.pyface)
library(reticulate)
# use_condaenv("py-snowpack")  ## usually reticulate finds the correct environment itself!

## load local python code
pypath <- system.file("python", package = "sarp.snowprofile.pyface")
pyreadProfile <- import_from_path("readProfile", path = pypath)
## ... using two convenience wrappers:
import_pyunstable()
import_RFmodel()

## ---example: read snow profiles----
## read profiles with R function from package sarp.snowprofile
profiles <- snowprofilePro(system.file("extdata/snowprofile.pro", package = "sarp.snowprofile.pyface"), remove_soil = TRUE, suppressWarnings = TRUE)
pro <- profiles[[1]]


## read profiles with local python function loaded earlier
pyprofiles <- pyreadProfile$read_profile(system.file("extdata/snowprofile.pro", package = "sarp.snowprofile.pyface"), remove_soil = TRUE)
pypro <- pyprofiles$data[[1]]


## ---example: compute p_unstable----
## with convenient R function
## ... applied to one profile:
pro <- computeSLABrho(pro)
pro <- computePunstable(pro)
## ... applied to a set of profiles
profiles_new <- computePunstable(profiles)

## manually via python modules
pyfeatures <- pyunstable$comp_features(pypro, 0)
p_unstable <- pyunstable$comp_rf_probability(pyfeatures, RFmodel)


## ---benchmark timing----
## 5 replications of computing p_unstable for 1 profile
rbenchmark::benchmark(
  R = computePunstable(pro),
  py = pyunstable$comp_rf_probability(pyfeatures, RFmodel), replications = 4)
## 1 calculation of snowprofileSet with 4 profiles: waaaay faster!!
system.time(computePunstable(profiles))

## tests to optimize computation speed for calling python function for p_unstable
## compare (i) 10 repetitions vs (ii) a 10-fold rbind features data frame
feats <- pyfeatures
feats[nrow(feats), ] <- feats[nrow(feats)-1, ]
featsX5 <- rbind(feats, feats, feats, feats, feats)
featsX10 <- rbind(featsX5, featsX5)
rbenchmark::benchmark(rpeat10 = pyunstable$comp_rf_probability(feats, RFmodel), replications = 10)
rbenchmark::benchmark(x10rbind = pyunstable$comp_rf_probability(featsX10, RFmodel), replications = 1)
## result: x10rbind one order of magnitude faster

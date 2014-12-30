library(unitizer)
dir <- system.file(package="alike")

unitize(paste0(dir, "/tests/unitizer/alike.R"))
unitize(paste0(dir, "/tests/unitizer/helper.R"))

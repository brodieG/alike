library(unitizer)
unitize("unitizer/alike.R")
unitize("unitizer/internal.R")
unitize("unitizer/classes.R")
unitize("unitizer/type.R")
unitize("unitizer/type.R")
library(data.table)
unitize("unitizer/datatable.R", search.path.clean=FALSE)
detach("package:data.table", unload=FALSE)  # data.table issue #990

# Remove DLLs when package is unloaded
# nocov start

.onUnload <- function(libpath) {
  library.dynam.unload("alike", libpath)
}

# nocov end

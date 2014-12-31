unitizer_sect("data.table", {
  library(data.table)

  dt.1 <- data.table(a=1:10, b=1:10)
  dt.2 <- data.table(a=1:10, b=letters[1:10], c=letters[11:20])
  dt.3 <- data.table(a=integer(), b=integer())

  alike(dt.1, dt.2)
  alike(dt.1, dt.3)
  alike(dt.3, dt.1)
} )

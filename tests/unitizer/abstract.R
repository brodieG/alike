library(alike)

unitizer_sect("Time Series", {

  y <- ts(runif(12), start=1970, freq=12)
  attr(abstract(y), "ts")
  attr(abstract(y, "start"), "ts")
  attr(abstract(y, "end"), "ts")
  attr(abstract(y, "frequency"), "ts")
  attr(abstract(y, c("start", "frequency")), "ts")

  # Errors

  abstract(y, "boom")
  alike:::abstract.ts(1:12)
})

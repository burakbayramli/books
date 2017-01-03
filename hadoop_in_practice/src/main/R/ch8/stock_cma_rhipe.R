#! /usr/bin/env Rscript
library(Rhipe)

rhinit(TRUE,TRUE)

map <- expression({
  process_line <- function(currentLine) {
    fields <- unlist(strsplit(currentLine, ","))
    lowHigh <- c(as.double(fields[3]), as.double(fields[6]))
    rhcollect(fields[1], toString(mean(lowHigh)))
  }
  lapply(map.values, process_line)
})

reduce <- expression(
  pre = {
    means <- numeric(0)
  },
  reduce = {
    means <- c(means, as.numeric(unlist(reduce.values)))
  },
  post = {
    rhcollect(reduce.key, toString(mean(means)))
  }
)

input_file <- "/tmp/stocks.txt"
output_dir <- "/tmp/output"

job <- rhmr(
  jobname  = "Rhipe CMA",
  map      = map,
  reduce   = reduce,
  ifolder  = input_file,
  ofolder  = output_dir,
  inout    = c("text", "sequence")
)

rhex(job)

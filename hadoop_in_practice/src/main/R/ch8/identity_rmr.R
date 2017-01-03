#! /usr/bin/env Rscript
library(rmr)

map <- function(k, v) {
  keyval(k, v)
}

reduce <- function(k,vv) {
  lapply(vv, function(v) keyval(k, v))
}

mapreduce(
  input = "stocks.txt",
  output = "output",
  textinputformat = rawtextinputformat,
  map = map,
  reduce = reduce)


#! /usr/bin/env Rscript
options(warn=-1)
sink("/dev/null")
input <- file("stdin", "r")

while(length(currentLine <- readLines(input, n=1, warn=FALSE)) > 0) {

   fields <- unlist(strsplit(currentLine, ","))

   lowHigh <- c(as.double(fields[3]), as.double(fields[6]))

   stock_mean <- mean(lowHigh)

   sink()

   cat(fields[1], fields[2], stock_mean, "\n", sep="\t")

   sink("/dev/null")

}
close(input)
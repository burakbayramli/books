# Preliminary code to load before start
# clear everything to start
rm(list = ls())
# initial customizations
seed <- 42
set.seed(seed)
options(width = 60)
options(useFancyQuotes = FALSE)
cexlab <- 1.5
# load packages needed by the book
library("actuar")
library("aplpack")
library("boot")    # already recommended package
library("coin")
library("combinat")
library("diagram")
library("distrEx")
library("e1071")
library("ggplot2")
library("HH")
library("Hmisc")
library("lattice") # already recommended package
library("lmtest")
library("mvtnorm") # dependency of coin
library("prob")
library("qcc")
library("RcmdrPlugin.IPSUR")
library("reshape")  # dependency of ggplot2
library("scatterplot3d")
library("stats4")
library("TeachingDemos")

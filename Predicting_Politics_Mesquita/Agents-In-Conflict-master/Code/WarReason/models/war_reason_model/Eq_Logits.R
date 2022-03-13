require(nnet)
require(stargazer)
setwd("~/Programming/DissertationResearch/consul/models/war_reason_model")

# Observed data
Full_Data <- read.csv("~/Programming/DissertationResearch/consul/models/war_reason_model/Full_Data.csv")
Truncated_Data <- subset(Full_Data, year>1850)
mn <- multinom(Outcome ~ Equilibrium, data=Full_Data)

# RL Data
data <- read.csv("RL_Subset.csv")
data_subset = subset(data, year>1850)
mn <- multinom(Outcome ~ Model_Outcome, data=data_subset, maxit=1000)
stargazer(mn, type="text")



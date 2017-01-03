#  Example 17.4

library("fEcofin")
data("DowJones30")
pcaDJ = prcomp(DowJones30[,2:31])
summary(pcaDJ)
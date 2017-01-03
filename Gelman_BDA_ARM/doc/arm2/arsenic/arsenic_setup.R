# Set up cleaned dataset for Bangladesh well-switching

# Read in the data

library (foreign)
all <- read.dta ("all.dta", convert.factors=F)

# For simplicity, pull out all wells with missing data in the variables that we
# will be using in our analysis

missing <- is.na (all[,"func"] + all[,"as"] + all[,"distnearest"] + all[,"assn"] + all[,"ed"])
table (missing)

# Include only the wells that are functioning (func==1) and "unsafe" (as>50)

keep <- all[,"func"]==1 & all[,"as"]>50
attach.all (all[!missing & keep,])

# Give convenient names to the variables

switch <- switch
arsenic <- as/100
dist <- distnearest
assoc <- ifelse (assn>0,1,0)
educ <- ed

wells.data <- cbind (switch, arsenic, dist, assoc, educ)
write.table (wells.data, "wells.dat")

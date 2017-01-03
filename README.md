Source codes for various books I collected over the years.

Dumping data from R

From alr4 package dumping Forbes

data("Forbes",package="alr4")
write.csv(Forbes, '/tmp/alr4/Forbes.csv')

library("DAAG")
data()

How to convert from dta (Stata) files into csv

Install foreign package

library("foreign")
df <- read.dta("xxx.dta")
write.csv(df, "xxx.csv")

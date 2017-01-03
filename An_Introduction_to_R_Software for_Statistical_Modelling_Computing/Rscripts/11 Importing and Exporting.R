######################## © CSIRO Australia 2005 ###############################
# Session 11:  Importing and Exporting                                        #
# Authors:     Petra Kuhnert & Bill Venables                                  #
#              CSIRO Mathematical and Information Sciences                    #
# Date:        28 November 2005                                               #
###############################################################################

####################
# Getting stuff in

# scan
vec <- scan()
22 35 1.7 2.5e+01 77
vec
chr <- scan(what="",sep="\n")
This is the first string
This is the second
and another
that's all we need for now

chr
lis <- scan(what = list(flag = "", x = 0, y = 0))
a 10 3.6
a 20 2.4
a 30 1.2
b 10 5.4
b 20 3.7
b 30 2.4

dat <- as.data.frame(lis)
dat

# read.table
samp1 <- read.csv("samp1.csv")
samp1[1:3,]

# read.fwf
dat.ff <- tempfile()
cat(file=dat.ff,"12345678","abcdefgh",sep="\n")
read.fwf(dat.ff,width=c(2,4,1,1))
unlink(dat.ff)  # clean up afterwards ...

# editing data
fix(samp1)
samp1.new <- edit(samp1)

# Reading in large datafiles
library(RODBC)
con <- odbcConnectExcel("ExampleData.xls")  # open the connection
con
sqlTables(con)
##
## note: to be readible, table names must have no spaces in them
samp1 <- sqlFetch(con, "samp1")
samp2 <- sqlFetch(con, "samp2")
odbcCloseAll()                            # close the connection

####################
# Getting stuff out

# cat
cat("Hello World\n")
cat("Hello World\n",file="output.txt")
pval <- 1-pchisq(2,1)
pval
cat("Test for Independence: p-value=",round(pval,3),"\n")

# sink
sink("output.txt")
sample(1:100,100,replace=T)
letters[1:10]
sink()

# write.table
con <- file("myData.csv", "w+")
write.table(myData, con, sep = ",")
close(con)

write.table(myData, "myData.txt")

# write.matrix
library(MASS)
write.matrix(myData,file="myData.csv",sep=",",blocksize=1000)

####################
# Getting out graphics

postscript("graph.ps",paper="a4")
hist(rnorm(10000))
dev.off()









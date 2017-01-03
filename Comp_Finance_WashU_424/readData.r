# readData.r
#
# author: E. Zivot
# created: Sept 24, 2008
# revised: October 21, 2008
#
# comments
# 1. use packages zoo and xts for representing irregularly spaced time series
#    note: only need to load xts since xts requires zoo
# 
# R functions used
#
# as.Date()			coerce to Date object
# as.numeric()		coerce to numeric object
# class()			return or set class of object
# colnames()		extract column names
# format()			format output
# head()			show fist few rows of data object
# library()			load package
# read.csv()		read comma separated file into R
# seq()			create sequence
# tail()			show last few rows of data object
#
# R package zoo function sued
#
# read.zoo()		read text file data and create zoo object
# zoo()			create zoo object
#
# load the zoo and xts packages
# note: xts requires zoo
#
library("xts")

#
# read .csv files containing Yahoo! monthly adjusted closing price data on sbux 
# and msft from March, 1993 through March 2008. The files sbuxPrices.csv and 
# msftPrices are assumed to be in the directory C:\classes\econ424\fall2008
#

# read the sbux prices
sbux.df = read.csv("C:/classes/econ424/fall2008/sbuxPrices.csv", 
                   header=TRUE, stringsAsFactors=FALSE)
# sbux.df is a data.frame object
class(sbux.df)
head(sbux.df)
colnames(sbux.df)
class(sbux.df$Date)
class(sbux.df$Adj.Close)

# now read the msft prices
msft.df = read.csv("C:/classes/econ424/fall2008/msftPrices.csv", 
                   header=TRUE, stringsAsFactors=FALSE)


# notice how dates are not the end of month dates. We will fix that below

# find indices associated with the dates 3/1/1994 and 3/1/1995
which(sbux.df$Date == "3/1/1994")
which(sbux.df$Date == "3/1/1995")
# extract prices between 3/1/1994 and 3/1/1995
sbux.df[13:25,]

#
# representing time series as ts objects
#

# create ts object
# this is appropriate since monthly data is equally (regularly) spaced
?ts
sbux.ts = ts(data=sbux.df$Adj.Close, frequency = 12,
             start=c(1993,3), end=c(2008,3))
class(sbux.ts)
str(sbux.ts)

msft.ts = ts(data=msft.df$Adj.Close, frequency = 12,
             start=c(1993,3), end=c(2008,3))

# special functions for ts objects
head(sbux.ts)
start(sbux.ts)
end(sbux.ts)
frequency(sbux.ts)
deltat(sbux.ts)
time(sbux.ts)
tsp(sbux.ts)

# note: dates are not displayed with ts objects

#
# there are only a few methods implemented for ts objects and one cannot
# subset a ts object using dates. In general, subsetting creates a vector or matrix
#

# subset first 5 obvs. Extracted subset is not a ts object!
tmp = sbux.ts[1:5]
class(tmp)
tmp

# subsetting using the window() command retains the ts object information
?window
# extract first 5 obvs. using window
tmp = window(sbux.ts, start=c(1993, 3), end=c(1993,8))
class(tmp)
tmp

# merging ts objects

sbuxmsft.ts = cbind(sbux.ts, msft.ts)
class(sbuxmsft.ts)
window(sbuxmsft.ts, start=c(1993, 3), end=c(1993,7))

# plot the time series
plot(sbux.ts, col="blue", lwd=2, ylab="Adjusted close",
     main="Monthly closing price of SBUX")
# add a legend
# note the x coordinate is in units of time and the y coordinate is in the 
# units of the data
legend(1995,30, legend="SBUX", lty=1, lwd=2, col="blue")


# plot the last 8 years
plot(window(sbux.ts, start=c(2000,3), end=c(2008,3)), ylab="Adjusted close",
     col="blue", lwd=2, main="Monthly closing price of SBUX")

# plot multiple time series
plot(sbuxmsft.ts)

plot(sbuxmsft.ts, plot.type="single", 
     main="Monthly closing prices on SBUX and MSFT",
     ylab="Adjusted close price",
     col=c("blue", "red"), lty=1:2)
legend(1995, 45, legend=c("SBUX","MSFT"), col=c("blue", "red"), lty=1:2)

# manipulating ts data

# lags
# notice how the lag shifts the time series forward
lag(sbux.ts)
lag(sbux.ts, k=12)
cbind(sbux.ts, lag(sbux.ts))

# to get usual lag, use negative k
lag(sbux.ts, k=-1)
lag(sbux.ts, k=-12)
cbind(sbux.ts, lag(sbux.ts, k=-1))


# differences
diff(sbux.ts)
diff(sbux.ts, lag=12)
cbind(sbux.ts, diff(sbux.ts))

# compute simple returns
n = length(sbux.ts)
tmp = (sbux.ts[2:n] - sbux.ts[1:(n-1)])/sbux.ts[1:(n-1)]
tmp2 = diff(sbux.ts)/lag(sbux.ts, k=-1)

sbuxRetSimple.ts = diff(sbux.ts)/lag(sbux.ts, k=-1)
msftRetSimple.ts = diff(msft.ts)/lag(msft.ts, k=-1)
window(cbind(sbuxRetSimple.ts, msftRetSimple.ts), 
       start=c(1993,4), end=c(1993,7))

# 12-period simple return
diff(sbux.ts, lag=12)/lag(sbux.ts, k=-12)
 
# compute cc returns

sbuxRet.ts = diff(log(sbux.ts))
msftRet.ts = diff(log(msft.ts))
window(cbind(sbuxRet.ts, msftRet.ts), start=c(1993,4), end=c(1993,7))

# 12-period cc return
diff(log(sbux.ts), lag=12)

#
# representing time series as zoo objects
#
?zoo

#
# Date class
# Internally, Date objects are represented as the number of days since
# January 1, 1970. Default date format is YYYY/m/d or YYYY-m-d
# main functions
# as.Date()			coerce character string to Date object
# class()			convert integer to Date object
# format()			

my.date = as.Date("1970/1/1")
my.date
class(my.date)
as.numeric(my.date)

# use the format argument to specify in the input format
as.Date("1/1/1970", format="%m/%d/%Y")
as.Date("January 1, 1970", format="%B %d, %Y")
as.Date("01JAN70", format="%d%b%y")

# change the display format of a Date object

my.date
format(my.date, "%m/%d/%Y")

# convert integer variable to Date object

my.date = 0
class(my.date) = "Date"
my.date

# extract date components

weekdays(my.date)
months(my.date)
quarters(my.date)

# create a sequence of dates form March 1993 through March 2003
# dates are represented as year/month/day 

td = seq(as.Date("1993/3/1"), as.Date("2008/3/1"), "months")
class(td)
head(td)

# alternatively, coerce the character dates to Date objects
# note: not the same since td starts every month on the first
# day of the month.

td2 = as.Date(sbux.df$Date, format="%m/%d/%Y")
head(td2)


# note: simple date arithmetic is available
td[2] - td[1]

#
# create zoo object from time index and data
#

sbux.z = zoo(x=sbux.df$Adj.Close, order.by=td)
msft.z = zoo(x=msft.df$Adj.Close, order.by=td)

class(sbux.z)
str(sbux.z)
head(sbux.z)

# extract time index and data
index(sbux.z)
coredata(sbux.z)

# show start and end dates
start(sbux.z)
end(sbux.z)

# zoo objects have certain advantages over ts objects
# subsetting does not strip away the object information

sbux.z[1:5]
class(sbux.z[1:5])

# subsetting can be done with a date index
sbux.z[as.Date(c("2003/3/1", "2004/3/1"))]

# subset using window
window(sbux.z, start=as.Date("2003/3/1"), end=as.Date("2004/3/1"))

# merge sbux.z and msft.z. Use cbind() when zoo objects have the same
# time index and use merge() when they have possibly different indices

sbuxmsft.z = cbind(sbux.z, msft.z)
class(sbuxmsft.z)
head(sbuxmsft.z)


# Change the date index to an object of class yearmon. This is more
# appropriate for monthly time series
# index(sbux.z) = as.yearmon(index(sbux.z))
# sbux.z

# plotting zoo objects
# plot one series at a time and add a legend
plot(sbux.z, col="blue", lty=1, lwd=2, ylim=c(0,50))
lines(msft.z, col="red", lty=2, lwd=2)
legend(x="topleft", legend=c("SBUX","MSFT"), col=c("blue","red"), lty=1:2)

# plot multiple series at once
plot(sbuxmsft.z, plot.type="single", col=c("blue","red"), lty=1:2, lwd=2)
legend(x="topleft", legend=c("SBUX","MSFT"), col=c("blue","red"), lty=1:2)

# import data directly using read.zoo()
?read.zoo
sbux.z2 = read.zoo("C:/classes/econ424/fall2008/sbuxPrices.csv", 
                   format="%m/%d/%Y", sep=",", header=T)
# convert index to yearmon
index(sbux.z2) = as.yearmon(index(sbux.z2))
head(sbux.z2)



# convert ts object to zooreg object
# note: as.zoo() automatically creates zooreg object from ts object
sbux.zr = as.zoo(sbux.ts)
class(sbux.zr)
str(sbux.zr)
head(sbux.zr)
# extract the core data
coredata(sbux.zr)
# extract the time index
index(sbux.zr)
class(index(sbux.zr))

# zooreg objects have certain advantages over ts objects
# subsetting does not strip away the object information

sbux.zr[1:5]
class(sbux.zr[1:5])

# one can subset using date index information
# but with numeric index must use I() function
sbux.zr[I(c(1993.250, 1994.250, 1995.250))]

# It is better to have a Date or POSIXct object as the date index

# coerce date index to class "Date"
index(sbux.zr) <- as.Date(sbux.ts)
str(sbux.zr)
head(sbux.zr)

# now subscript with Date index
sbux.zr[as.Date(c("2003-03-01", "2004-03-01"))]


#
# importing data from Yahoo!
#

# install and load the tseries package
library(tseries)
help("tseries")

# get daily data for sbux between 3/1/1993 and 3/1/2008

SBUX.z = get.hist.quote(instrument="sbux", start="1993-03-01",
                        end="2008-03-01", quote="AdjClose",
                        provider="yahoo", origin="1970-01-01",
                        compression="d", retclass="zoo")
class(SBUX.z)
class(index(SBUX.z))
head(SBUX.z)
start(SBUX.z)
end(SBUX.z)

plot(SBUX.z, main="Daily closing prices on SBUX", ylab="Adjusted close price",
     col="blue")




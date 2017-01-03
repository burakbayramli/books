# 424project2008.r		script file for econ 424 Project calculations
#
# author: Eric Zivot
# created: November 1, 2008
# revised: November 1, 2008
#
# comments:
# Data for the project are in the Excel file projectDataFall2007.xls, 
# which contains monthly closing price data on 10 Vanguard 
# mutual funds:
#
# 1. S&P 500 index (vfinx)
# 2. European stock index (veurx)
# 3. Emerging markets fund (veiex)
# 4. Long term bond index (vbltx)
# 5. Short term bond index (vbisx)
# 6. Pacific stock index (vpacx)

# load packages
library("TSA")
library("tseries")
library("zoo")

#
# 1. load data from Yahoo!
#

# get monthly adjusted closing price data on Vanguard mutual fund data from Yahoo
# using the tseries function get.hist.quote. Set sample to Jan 1998 through
# Dec 2007. Note: if you are not careful with the start and end dates
# or if you set the retclass to "ts" then results might look weird

asset.names = c("vfinx","veurx","veiex","vbltx","vbisx","vpacx")

vfinx.prices = get.hist.quote(instrument="vfinx", start="2003-09-01",
                             end="2008-9-30", quote="AdjClose",
                             provider="yahoo", origin="1970-01-01",
                             compression="m", retclass="zoo")
veurx.prices = get.hist.quote(instrument="veurx", start="2003-09-01",
                             end="2008-9-30", quote="AdjClose",
                             provider="yahoo", origin="1970-01-01",
                             compression="m", retclass="zoo")
veiex.prices = get.hist.quote(instrument="veiex", start="2003-09-01",
                             end="2008-9-30", quote="AdjClose",
                             provider="yahoo", origin="1970-01-01",
                             compression="m", retclass="zoo")
vbltx.prices = get.hist.quote(instrument="vbltx", start="2003-09-01",
                             end="2008-9-30", quote="AdjClose",
                             provider="yahoo", origin="1970-01-01",
                             compression="m", retclass="zoo")
vbisx.prices = get.hist.quote(instrument="vbisx", start="2003-09-01",
                             end="2008-9-30", quote="AdjClose",
                             provider="yahoo", origin="1970-01-01",
                             compression="m", retclass="zoo")
vpacx.prices = get.hist.quote(instrument="vpacx", start="2003-09-01",
                             end="2008-9-30", quote="AdjClose",
                             provider="yahoo", origin="1970-01-01",
                             compression="m", retclass="zoo")

projectPrices.z = merge(vfinx.prices,veurx.prices,veiex.prices,vbltx.prices,
                        vbisx.prices,vpacx.prices)
colnames(projectPrices.z) = asset.names

#
# 2. compute cc returns
#

projectReturns.z = diff(log(projectPrices.z))

#
# 3. plot data
#

plot(projectPrices.z)
plot(projectReturns.z)


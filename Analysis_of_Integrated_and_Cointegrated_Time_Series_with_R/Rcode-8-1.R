library(urca)
data(finland)
str(finland)
# utilisation of time series class 'ts' in base package
fin.ts <- ts(finland, start=c(1958, 2), end=c(1984, 3), frequency=4)
str(fin.ts)
# time series properties of fin.ts
tsp(fin.ts)
time(fin.ts)[1:10]
# Creating a sub sample
finsub.ts <- window(fin.ts, start=c(1960, 2), end=c(1979, 3))
tsp(finsub.ts)

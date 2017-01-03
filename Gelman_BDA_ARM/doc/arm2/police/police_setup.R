# Data from NYC police stops

# Read in data

stops <- read.table ("nypdstops2.csv", sep=",", header=T)
attach.all (stops)
n <- nrow(stops)
y <- COUNT
eth <- match(RACE,c("B","H","W"))
precinct.original <- PCT
precinct.number <- unique(precinct.original)
n.precinct <- length(precinct.number)
precinct <- rep(NA,n)
for (i in 1:n.precinct) precinct[precinct.original==precinct.number[i]] <- i
month <- monstop
arrests <- arr
n.eth <- max(eth)
n.crime <- max(crime)
n.month <- max(month)
pblack <- pb[month==1&eth==1&crime==1]

# Read in compressed data that are averaged over months

frisk <- read.table ("frisk.dat", header=T)
attach.all (frisk)
n.precinct <- max (precinct)
n.eth <- max (eth)
n.crime <- max(crime)
dcjs <- log(arrests*15/12)

frisk <- as.data.frame (cbind (y, eth, precinct, crime, precinct.category, arrests, dcjs, pop))

# define precinct categories based on %black

precinct.category <- ifelse (pblack < .1, 1, ifelse (pblack < .4, 2, 3))
n.precinct.category <- max (precinct.category)


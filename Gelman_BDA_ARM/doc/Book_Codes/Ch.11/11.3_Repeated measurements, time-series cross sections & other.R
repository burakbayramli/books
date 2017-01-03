## Read in the data
# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/?
data?
library ("arm")

## Overall rate of smoking (figure 11.5)


## Pull out the smoking outcome: single data matrix
y <- data[,seq(6,12,2)]
female <- ifelse (data[,2]=="f", 1, 0)
mom.smoke <- ifelse (data[,3]=="Y", 1, 0)
dad.smoke <- ifelse (data[,4]=="Y", 1, 0)
psmoke <- mom.smoke + dad.smoke

## Pull out the smoking outcome: two data matrices
y <- obs.data[,2]
person <- obs.data[,3]
wave <- obs.data[,4]
female <- ifelse (person.data[,2]=="f", 1, 0)
mom.smoke <- ifelse (person.data[,3]=="Y", 1, 0)
dad.smoke <- ifelse (person.data[,4]=="Y", 1, 0)
psmoke <- mom.smoke + dad.smoke




######################## © CSIRO Australia 2005 ###############################
# Session 05:  Manipulating Data in R                                         #
# Authors:     Petra Kuhnert & Bill Venables                                  #
#              CSIRO Mathematical and Information Sciences                    #
# Date:        28 November 2005                                               #
###############################################################################

#########################
# Sorting
# Sorting: order
x <- sample(1:5, 20, rep=T)
y <- sample(1:5, 20, rep=T)
z <- sample(1:5, 20, rep=T)
xyz <- rbind(x, y, z)
dimnames(xyz)[[2]] <- letters[1:20]
xyz
o <- order(x, y, z)
xyz[, o]
xyz		# reminder
# Sorting: sort
sort(x)
sort(x,decreasing=T)
sort(x,partial=c(3,4))
# Sorting: rank
rank(x)
rank(x, ties="first")	    # first occurrence wins
rank(x, ties="random")	    # ties broken at random
rank(x, ties="min")	    # typical sports ranking


##############################
# Dates and Times
myBday <- strptime("18-Apr-1973", "%d-%b-%Y")
class(myBday)
myBday
weekdays(myBday)

Sys.time()
Sys.time() - myBday

as.numeric(Sys.time())
as.numeric(myBday)
as.numeric(as.POSIXct(myBday))
as.numeric(Sys.time()) - as.numeric(as.POSIXct(myBday))

###############################
# Tabulation
attach(quine)
table(Age)
table(Sex,Age)

###############################
# Split Function
split(Days,Sex)
# Graphical displays of "split" data
boxplot(split(Days,Sex),ylab=“Days Absent”)
library(lattice)		# trellis graphics
trellis.par.set(col.whitebg())
bwplot(Days ~ Age | Sex) 	# implicit split

###############################
# with, subset and transform functions
with(Cars93,plot(Weight,100/MPG.highway))
Vans <- subset(Cars93,Type=="Van")
Cars93T <- transform(Cars93,WeightT=Weight/1000)

###############################
# The "apply" functions
# apply
iris[1:4,]
apply(iris[,-5],2,mean)
# tapply
quine[1:5,]
tapply(Days,Age,mean)
tapply(Days,list(Sex,Age),mean)
# lapply and sapply
l <- list(Sex=Sex,Eth=Eth)
lapply(l,table)
l <- list(Sex=Sex,Eth=Eth)
sapply(l,table)


###################################
# Programming examples
# 5 for loops
mat <- matrix(rnorm(100000),ncol=4)
program1 <- function(mat){
  col.scale <- matrix(NA,nrow(mat),ncol(mat))	# create an empty matrix
  m <- rep(0,ncol(mat))				# create a vector, m and
							                #   set it to zero
  for(j in 1:ncol(mat)){
      for(i in 1:nrow(mat)) {
          m[j] <- m[j] + mat[i,j]			# compute the sum of
							                        #   elements in each
							                        #   column
      }
  }
  for(j in 1:ncol(mat))
      m[j] <- m[j]/nrow(mat)			# compute the mean
  for(i in 1:nrow(mat)){
      for(j in 1:ncol(mat)){
          col.scale[i,j] <- mat[i,j]-m[j]		# centre each column by
							                              #  the mean
      }
   }
  col.scale					# print the scaled matrix
}

# 3 for loops
program2 <- function(mat){
  col.scale <- matrix(NA,nrow(mat),ncol(mat))	# create an empty matrix
  m <- NULL					                          # initialise the vector,m
  for(j in 1:ncol(mat))
      m[j] <- mean(mat[,j])			              # compute the mean of
							                                #   each column
  for(i in 1:nrow(mat)){
      for(j in 1:ncol(mat)){
          col.scale[i,j] <- mat[i,j]-m[j] 		# centre columns
      }
  }
  col.scale					                          # print the scaled matrix
}

# No for loops
program3 <- function(mat){
    apply(mat,2,scale,scale=F)		# apply to the matrix (mat)
						                      #   the function scale
						                      #   specifying that centring
						                      #   only be performed.
						                      # print the scaled matrix
}

# Performance
system.time(v1 <- program1(mat))
system.time(v2 <- program2(mat))
system.time(v3 <- program3(mat))
system.time(v4 <- scale(mat,scale=F))
check <- function(v1,v2) abs(diff(range(v1-v2)))
check(v1,v2) + check(v2,v3) + check(v3,v4)













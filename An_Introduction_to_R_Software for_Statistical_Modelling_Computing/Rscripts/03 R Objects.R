######################## © CSIRO Australia 2005 ###############################
# Session 03:  R Objects                                                      #
# Authors:     Petra Kuhnert & Bill Venables                                  #
#              CSIRO Mathematical and Information Sciences                    #
# Date:        28 November 2005                                               #
###############################################################################

################
# Creating Vectors: c function
value.num <- c(3,4,2,6,20)
value.char <- c("koala","kangaroo","echidna")
value.logical.1 <- c(F,F,T,T)
value.logical.2 <- c(FALSE,FALSE,TRUE,TRUE)

# Creating Vectors: rep and seq functions
value <- rep(5,6)
value
seq(from=2,to=10,by=2)
seq(from=2,to=10,length=5)
1:5
seq(along=value)

# Creating Vectors: c, rep and seq functions
value <- c(1,3,4,rep(3,4),seq(from=1,to=6,by=2))
value
c(1:3,"a","b","c")

# Creating Vectors: scan function
value <- scan()
3  4  2  6  20
value

################
# Basic computation with numerical vectors
x <- runif(10)
x
y < 2*x + 1		# recycling short vectors
y
z <- (x-mean(x))/sd(x)	# see also 'scale'
z
mean(z)
sd(z)

###################
# Creating Matrices: dim and matrix functions
value <- rnorm(6)
dim(value) <- c(2,3)
value
dim(value) <- NULL
matrix(value,2,3)
matrix(value,2,3,byrow=T)

# Creating Matrices: rbind and cbind functions
value <- matrix(rnorm(6),2,3,byrow=T)
value2 <- rbind(value,c(1,1,2))
value2
value3 <- cbind(value2,c(1,1,2))

# Creating Matrices: data.frame function
value3 <- data.frame(value3)
value3
value4 <- data.frame(rnorm(3),runif(3))
value4

# Specifying row and column names
names(value3)
names(value3) <- c("C1","C2","C3","C4")

row.names(value3)
row.names(value3) <- c("R1","R2","R3")
data.frame(C1=rnorm(3),C2=runif(3),row.names=c("R1","R2","R3")

#################
# Manipulating Data: An Example
dimnames(iris3)
Snames <- dimnames(iris3)[[3]]
# Convert into a 150 x 3 matrix
iris.df <- rbind(iris3[,,1],iris3[,,2],iris3[,,3])
# Coerce the matrix into a data frame and check the variable names
iris.df <- as.data.frame(iris.df)
names(iris.df)
# Make a species factor and bind it to the columns of this matrix
iris.df$Species <- factor(rep(Snames,rep(50,3)))
# Look at first 5 rows of the data frame
iris.df[1:5,]
# Produce a scatterplot of the data
pairs(iris.df[1:4],main = "Anderson's Iris Data",
    pch = 21,bg = c("red","green3","blue")[unclass(iris$Species)])
    
##################
# Accessing elements of a vector or matrix

# Indexing vectors
x <- sample(1:5, 20, rep=T)
x
x == 1
ones <- (x == 1)	# parentheses unnecessary
x[ones] <- 0
x
others <- (x > 1)	# parentheses unnecessary
y <- x[others]
y
which(x > 1)

# Indexing matrices and dataframe: indexing by columns
value3
value3[,"C1"] <- 0
value3

# Indexing matrices and dataframes: indexing by rows
value["R1",] <- 0
value3
value3[] <- 1:12
value3

# Accessing the first two rows of the matrix/dataframe
value3[1:2,]
# Accessing the first two columns of the matrix/dataframe
value3[,1:2]
# Obtaining any element with a value greater than 5
as.vector(value3[value3>5])

####################
# Creating Lists: list function
L1 <- list(x = sample(1:5, 20, rep=T),
          y = rep(letters[1:5], 4), z = rpois(20, 1))
L1
# Accessing the first component
L1[["x"]]
L1$x
L1[[1]]
# A sublist consisting of the first component only
L1[1]
# Length of a list
length(L1)
# Naming
names(L1) <- c("Item1","Item2","Item3")
# Indexing lists
L1$Item1[L1$Item1>2]
# Joining two lists using the c function
L2 <- list(x=c(1,5,6,7),y=c("apple","orange","melon","grapes"))
c(L1,L2)
# Joining two lists using the append function
append(L1,L2,after=2)
# Adding elements to a list
L1$Item4 <- c("apple","orange","melon","grapes")
L1[[4]] <- c("apple","orange","melon","grapes")
names(L1)[4] <- c("Item4")
L1[["Item4"]] <- c("apple","orange","melon","grapes")

#######################
# Comparison between 500kg and 1000kg density estimates for weight: Cars93 Data
attach(Cars93)
windows()
par(family="mono")
dw5 <- spline(density(Weight, width=500))  # list
dw10 <- spline(density(Weight,width=1000)) # list
rx <- range(dw5$x,dw10$x)
ry <- range(dw5$y,dw10$y)
par(mar=c(5,5,2,2)+0.1) -> oldpar
plot(dw5,type="n",xlim=rx,ylim=ry,cex=1.5,xlab="Weight",ylab="Density")
lines(dw5,lty=1,col="blue")
lines(dw10,lty=2,col="red")
pu <- par("usr")[3:4] # actual y limits
segments(Weight,pu[1],Weight,0,col="green")
legend(locator(1),c("500kg window","1000kg   window"),lty=1:2)
detach("Cars93")













######################## © CSIRO Australia 2005 ###############################
# Session 13: Programming                                                     #
# Authors:  Petra Kuhnert & Bill Venables                                     #
#           CSIRO Mathematical and Information Sciences                       #
# Date:     28 November 2005                                                  #
###############################################################################

###############
# Introductory Example

rvball <- function(d){
    # Taking logs and then exponentiating
    exp(d/2 * log(pi) - d * log(2) - lgamma(d/2 + 1))
}
structure(floor(1/rvball(1:10)), names = 1:10)

# Simulation
mcrvball <-
	function(d, N = 10000, blocksize = 10000) {
    	n2 <- inside <- 0
    	while(n2 < N) {
    		n1 <- n2
    		n2 <- min(n2 + blocksize, N)
    		No <- n2 - n1
    		samp <- matrix(runif(No * d, -1, 1), No, d)
    		inside <- inside +
    		  sum((samp * samp) %*% rep(1, d) < 1)

    	}
    	res <- list(dimensions = d, inside = inside,
    		total = N, call = match.call())
    	oldClass(res) <- "mcrvball"
    	res
}

# Writing a print method for this function
print.mcrvball <- function(x, ...) {
	cat("Dim.:", x$d,
	"Estimated:", signif(x$inside/x$total, 4),
	"Actual:", signif(rvball(x$dim), 4), "\n")
	invisible(x)
}

Ops.mcrvball <- function(e1, e2) {
	if(!is.null(class(e1)) && class(e1) == "mcrvball")
	e1 <- e1$inside/e1$total
	if(!missing(e2) && !is.null(class(e2)) &&
		class(e2) == "mcrvball")
	e2 <- e2$inside/e2$total
	NextMethod()
}

# Test Run
for(i in 4:10) print(mcrvball(i, 1000000))
X <- matrix(NA, 7, 2)
X[,2] <- floor(1/rvball(4:10))

for(i in 4:10)
		 X[i-3,1] <- floor(1/mcrvball(i, 1000000))
dimnames(X) <- list(4:10, c("Monte Carlo", "Actual"))
X


# The Call Component and updating
p1 <- mcrvball(10)
floor(1/p1)
p2 <- update(p1, N = 200000)
floor(1/p2)

# Combining Two Estimates
"%+%" <- function(e1, e2) 	UseMethod("%+%")

"%+%.mcrvball" <- function(e1, e2) {
	if(e1$dimensions != e2$dimensions)
		stop("ball dimensions differ!")
		res <- list(dimensions = e1$dimensions,
			inside = e1$inside + e2$inside,
			total = e1$total + e2$total,
			call = e1$call)
		oldClass(res) <- "mcrvball"
		res
}
p1 %+% p2
floor(1/(p1 %+% p2))
# collecting results
p0 <- p1
for(i in 1:10) p0 <- p0 %+% print(update(p1))
p0
floor(1/p0)
unlist(sapply(p0, as.character))


####################
# Special Functions

# Tridiagonalisation
tridiag <- function(r, v) {
    	cind <- as.vector(outer(-1:1, 1:r, "+"))
    	rind <- rep(1:r, each = 3)
    	browser()
    	mind <- cbind(rind, cind)
    	mind <- mind[ -c(1, 3*r), ]
    	X <- matrix(0, r, r)
    	X[mind] <- rep(v, r)[ -c(1, 3*r)]
    	X
    }
tridiag(4, c(1,2,1))

# Direct Calculation
X <- matrix(0, 5, 5)
X[row(X) == col(X)] <- 2
X[abs(row(X) - col(X)) == 1] <- 1
X

##############
# Compiled Code and packages

# VR Convolve function
convolve3 <- function(a, b) {
  if(!is.loaded(symbol.C("VR_convolve")))
    dyn.load("VR_convolve.dll")
  storage.mode(a) <- "double"
  storage.mode(b) <- "double"

  .C("VR_convolve",
     a,
     length(a),
     b, length(b),
     ab = double(length(a) + length(b) - 1))$ab
}

# 3 S Code Contenders
convolve0 <- function(a, b) {
  ab <- rep(0, length(a) + length(b) - 1)
  for(i in 1:length(a))
    for(j in 1:length(b))
      ab[i+j-1] <- ab[i+j-1] + a[i]*b[j]
  ab
}

convolve1 <- function(a, b) {
  ab <- rep(0, length(a) + length(b) - 1)
  ind <- 1:length(a)
  for(j in 1:length(b)) {
    ab[ind] <- ab[ind] + a*b[j]
    ind <- ind + 1
  }
  ab
}

convolve2 <- function(a, b)
  tapply(outer(a, b), outer(seq(along = a), seq(along = b), "+"), sum)

# Testing
a <- rep(1/1000, 1000)
b <- dbinom(0:1000, 1000, 0.5)
system.time(ab0 <- convolve1(a,b))
system.time(ab1 <- convolve1(a, b))
system.time(ab2 <- convolve2(a, b))
system.time(ab3 <- convolve3(a, b))
range(abs(ab0-ab1)+abs(ab1-ab2)+abs(ab2-ab3))







    

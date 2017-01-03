###########################################################
###### Code for solutions to homework 3         ###########
###### 36-462, spring 2009                      ###########
###########################################################

############### Problem 1

### Functions copied over from lecture 1

logistic.map <- function(x,r) {
  return(4*r*x*(1-x))
}

logistic.map.ts <- function(timelength,r,initial.cond=NULL) {
  x <-vector(mode="numeric",length=timelength)
  if(is.null(initial.cond)) {
    x[1] <-runif(1)
  } else {
    x[1] <-initial.cond
  }
  for (t in 2:timelength) {
    x[t] = logistic.map(x[t-1],r)
  }
  return(x)
}

########### Problem 1a ##############

### Apply the generating partition of the logistic map to a time series
    # Return "L" at each position where x < 0.5 and "R" elsewhere
# Input: vector of reals (x)
# Output: vector of characters
logistic.genpart <- function(x) {
  ifelse(x<0.5,'L','R')
}


### Create a symbol sequence from an orbit of the logistic map at specified r
# Input: length of sequence (timelength), r
# Calls: logistic.genpart, logistic.map.ts
# Output: vector of characters

logistic.symbseq <- function(timelength,r) {
  x <- logistic.map.ts(timelength,r) # Creates logistic map trajectory
  s <- logistic.genpart(x) # Applies generating partition
  return(s)
}

########### Problem 1b ##############

### Take a symbol sequence and return a vector of the (overlapping) blocks of
### length L it contains
  # Slide a window of length L along the sequence, call the contents of that
  # window a block, collapse them into a single string, move on a step.
# Inputs: vector of characters (s), length of blocks (L)
# Assumes: L is an integer > 0
#          s contains characters or things useful coercable into characters
# 
# Output: vector of character strings representing the successive overlapping
#         blocks

symbseq.to.blocks <- function(s,L) {
  n <- length(s)
  # We need to repeatedly take a block from the sequence and collapse it into
  # a single string; make this operation a local function
  collapser <- function(i) {paste(s[i:(i+L-1)],collapse="")}
  # A length L block can't start at any position whose index exceeds n-L+1,
  # though it could start there.
  max.index <- n -L+1
    # Should really check that this is < n, and return an error if not!
  # Now: chop into blocks, collapse each block, put the strings in a vector
    # could do this by iteration, but R is happier and much faster with this
    # vectorized approach; also, this is shorter than a for loop!
  blocks <- sapply(1:max.index,collapser)
  return(blocks)
}

### Find the pairs of successive blocks of length L in a symbol sequence
# Inputs: sequence vector (s), block length (L)
# Assumes: L is an integer > 0
#          s contains characters or things meaningfully coerced into characters
#          length(s) >= 2*L
# Calls: symbseq.to.blocks
# Output: list with two components, each a vector of character blocks,
#         giving the matched "leader" and "follower" blocks at a given location
#         in the input sequence

symbseq.to.successive.blocks <- function(s,L) {
  n <- length(s)
  # Produce the complete list of length-L blocks
  all.blocks <- symbseq.to.blocks(s,L)
  # The "leaders" begin at positions 1, 2, ... n-2L+1 (because there
  # needs to be another, following block of length L after each of them)
  max.index.leaders <- n-2*L+1
  # The "followers" begin at positions L+1, L+2, ... n-L+1 (because there
  # needs to be a "leader" block of length L before each of them)
  min.index.followers <- L+1
  max.index.followers <- n-L+1
  leaders <- all.blocks[1:max.index.leaders]
  followers <- all.blocks[min.index.followers:max.index.followers]
  return(list(leaders=leaders,followers=followers))
}


### Test whether the symbolic dynamics of the logistic map are IID at given r
    # Generate a symbol string, break it into successive blocks of length L,
    # apply chi-squared test for independence
# Inputs: block length L, time series length n, r
# Assumes: L and n are positive integers > 0
# Calls: logistic.symbseq, symbseq.to.successive.blocks
# Output: list containing p.value, full test output, contingency table

logistic.map.independence.test <- function(L,n=min(1e4,10*(2^(2*L))),r=1) {
  s <- logistic.symbseq(n,r)
  successive.blocks <- symbseq.to.successive.blocks(s,L)
  my.tab <- table(successive.blocks)
  my.test <- chisq.test(my.tab)
  return(list(p.value=my.test$p.value,test=my.test,count.table=my.tab))
}


####### Problem 1c ########

# Requires no further code





############# Problem 2a ########################

# Estimate topological entropy rate for the logistic map: division method
# Inputs: r, block length L, time-series length n
# Calls: logistic.symbseq, symbseq.to.blocks
# Output: numerical topological entropy rate estimate

logistic.TER.division <- function(r,L,n=10*(2^L)) {
  s <- logistic.symbseq(n,r)
  blocks <- symbseq.to.blocks(s,L)
  word.table <- table(blocks)
  W.L <- dim(word.table) # Counts number of distinct allowed words
  return(log(W.L)/L)
}


# Estimate topological entropy rate for the logistic map: regression method
# Inputs: r, block length L, time-series length n
# Calls: logistic.symbseq, symbseq.to.blocks
# Output: numerical topological entropy rate estimate

logistic.TER.regression <- function(r,L,n=10*(2^L)) {
  s <- logistic.symbseq(n,r)
  W <- vector(mode="numeric",length=L)
  for (i in (1:L)) {
    blocks <- symbseq.to.blocks(s,i)
    W[i] <- dim(table(blocks))
  }
  my.regression <- lm(logcounts ~ lengths, 
                      data.frame(logcounts=log(W),lengths=(1:L)))
  return(as.vector(my.regression$coefficients[2]))
}

# Estimate topological entropy rate for the logistic map: differencing method
# Inputs: r, block length L, time-series length n
# Calls: logistic.symbseq, symbseq.to.blocks
# Output: numerical topological entropy rate estimate

logistic.TER.difference <- function(r,L,n=10*(2^L)) {
  s <- logistic.symbseq(n,r)
  lastW <- dim(table(symbseq.to.blocks(s,L)))
  nextotlastW <- dim(table(symbseq.to.blocks(s,L-1)))
  return(log(lastW) - log(nextotlastW))
}


########### Problem 3 ########################

# Estimate the transition matrix of a Markov chain
# Input: symbol sequence
# Calls: symbseq.to.successive.blocks
# Output: list containing the transition matrix and the log likelihood

markov.mle.1 <- function(s) {
  blocks <- symbseq.to.successive.blocks(s,1)
  counts <- table(blocks)
  # prop.table converts a count table to proportions, either by rows
  # or by columns, depending on 2nd argument - see its help file
  mle <- prop.table(counts,1)
  log.like <- sum(counts[counts>0]*log(mle[mle>0]))
  return(list(transition.matrix=mle,log.like=log.like))
}


# Simulate a binary Markov chain
# Inputs: time-series length (n), probability of going from 0 to 1 (p01),
#         probability of going from 1 to 1 (p11), probability of starting
#         in state 1 (p1start, defaults to invariant distribution)
# Output: vector of 0s and 1s

rbinmarkov <- function(n, p01, p11, p1start=NULL) {
  # n is the length of the output.
  # A binary Markov chain has two free parameters in its transition
  # matrix, which can be chosen in several ways --- here the
  # probability that "0" is followed by "1", and that "1" is followed
  # by "1".
  # Also need the probability of starting in state 1 (which implicitly gives
  # the probability of starting in state 0, as well)
  # The default is to calculate this from the stationary distribution
  if (is.null(p1start)) {
    P = matrix(c(1-p01,p01,1-p11,p11),nrow=2)
    # The invariant distribution is the proportional to the first eigenvector
    first.eigenvec = eigen(P)$vectors[,1]
    # but the eigen routine returns vectors whose norm is 1, so the components
    # don't sum to 1; we need to fix that to get a probability
    P.inv = first.eigenvec/sum(first.eigenvec)
    # "0" is the first symbol, so the probability of 1 is the 2nd component
    p1start = P.inv[2]
  }
  s = vector(length=n)
  s[1] = rbinom(1,1,p1start)
  for (i in 2:n) {
    s[i] = rbinom(1,1,ifelse(s[i-1]<1,p01,p11))
  }
  return(s)
}

# Back to the US in 1977

# Do a one-factor analysis and compute factor scores
state.fa1 <- factanal(state.x77,factors=1,scores="regression")

# Have R tell us about it
state.fa1
  # Compare the factor loadings here to the first principal component we got
  # last time

# Make a map again
  # Function is a repeat from last time

# Plot the state abbrevations in position, with scaled sizes
  # Linearly scale the sizes from the given minimum to the maximum
# Inputs: vector of raw numbers, minimum size for plot,
  # maximum size
# Outputs: Rescaled sizes (invisible)
plot.states_scaled <- function(sizes,min.size=0.4,max.size=2,...) {
  out.range = max.size - min.size
  in.range = max(sizes)-min(sizes)
  scaled.sizes = out.range*((sizes-min(sizes))/in.range)
  sizes = scaled.sizes + min.size
  plot(state.center,type="n",...)
  text(state.center,state.abb,cex=sizes)
  invisible(sizes)
}

# Actual map-making
plot.states_scaled(state.fa1$score[,1],min.size=0.3,max.size=1.5,
                   xlab="longitude",ylab="latitude")

# 3D visualization
  # You need the "scatterplot3d" library from CRAN for this
require(scatterplot3d)
# Make a matrix with the x,y,z values (z=factor scores)
state.xyz <- cbind(state.center$x,state.center$y,
                   state.fa1$scores[,1])
colnames(state.xyz)=c("x","y","z")
# Plot it!
state.3d <- scatterplot3d(state.xyz,type="h",
                          xlab="longitude",
                          ylab="latitude",
                          zlab="factor score",
                          cex.symbol=0.01,color="grey")
# Add labels
text(state.3d$xyz.convert(state.xyz),state.abb)



# How good is the factor model, though?

# Try models with 1--4 factors and get the p-value for each
pvalues <- sapply(1:4,function(q){factanal(state.x77,factors=q)$PVAL})
# What are these p-values?
signif(pvalues,2)

# Plot vs. the nominal 5% level
plot(1:4,pvalues,xlab="q (number of factors)", ylab="pvalue",
     log="y",ylim=c(1e-11,0.04))
abline(h=0.05,lty=2)

# Another example: sleep in mammals
sleep <- read.csv("sleep.txt")
# The fifth column is the sum of the 3rd and 4th columns, which makes everyone
# very unhappy!
sleep <- sleep[,-5]
# There are missing observations, so we need to calculate the covariance
# matrix outside factanal
sleep.cov <- cov(sleep,use="pairwise.complete.obs")
# Try one factor
sleep.fa1 <- factanal(sleep,factors=1,covmat=sleep.cov)
sleep.fa1
  # Try to tell a story about this factor based on the loadings
  # Notice that it won't give us a p-value because of the missing observations

# Try two factors
sleep.fa2 <- factanal(sleep,factors=2,covmat=sleep.cov)
sleep.fa2
  # Notice that the first factor has changed
  # Try to tell a story about these two factors


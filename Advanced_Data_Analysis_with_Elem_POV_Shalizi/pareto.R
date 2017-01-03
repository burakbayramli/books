#### Functions for continuous power law or Pareto distributions
# Revision history at end of file

### Standard R-type functions for distributions:
# dpareto		Probability density
# ppareto		Probability distribution (CDF)
# qpareto		Quantile function
# rpareto		Random variable generation
### Functions for fitting:
# pareto.fit			Fit Pareto to data
# .pareto.fit.threshold		Determine scaling threshold and then fit
#				--- not for direct use, call pareto.fit instead
# .pareto.fit.ml			Fit Pareto to data by maximum likelihood
#                               --- not for direct use, call pareto.fit instead
# pareto.loglike		Calculate log-likelihood under Pareto
# .pareto.fit.regression.cdf	Fit Pareto data by linear regression on
#				log-log CDF (disrecommended)
#                               --- not for direct use, call pareto.fit instead
# loglogslope			Fit Pareto via regression, extract scaling
#				exponent
# loglogrsq			Fit Pareto via regression, extract R^2
### Functions for testing:
#
### Functions for visualization:
# plot.eucdf.loglog		Log-log plot of the empirical upper cumulative
#				distribution function, AKA survival function
# plot.survival.loglog		Alias for plot.eucdf.loglog
### Back-stage functions, not intended for users:
# .ks.dist.for.pareto		Find Kolmogorov-Smirnov distance between fitted
#				and empirical distribution; called by
#				.pareto.fit.threshold
# .ks.dist.fixed.pareto		Find K-S distance between given Pareto and
#				empirical distribution

# Probability density of Pareto distributions
# Gives NA on values below the threshold
# Input: Data vector, lower threshold, scaling exponent, "log" flag
# Output: Vector of (log) probability densities
dpareto <- function(x, threshold = 1, exponent, log=FALSE) {
  # Avoid doing limited-precision arithmetic followed by logs if we want
  # the log!
  if (!log) {
    prefactor <- (exponent-1)/threshold
    f <- function(x) {prefactor*(x/threshold)^(-exponent)}
  } else {
    prefactor.log <- log(exponent-1) - log(threshold)
    f <- function(x) {prefactor.log -exponent*(log(x) - log(threshold))}
  }
  d <- ifelse(x<threshold,NA,f(x))
  return(d)
}

# Cumulative distribution function of the Pareto distributions
# Gives NA on values < threshold
# Input: Data vector, lower threshold, scaling exponent, usual flags
# Output: Vector of (log) probabilities
ppareto <- function(x, threshold=1, exponent, lower.tail=TRUE, log.p=FALSE) {
  if ((!lower.tail) && (!log.p)) {
    f <- function(x) {(x/threshold)^(1-exponent)}
  }
  if ((lower.tail) && (!log.p)) {
    f <- function(x) { 1 - (x/threshold)^(1-exponent)}
  }
  if ((!lower.tail) && (log.p)) {
    f <- function(x) {(1-exponent)*(log(x) - log(threshold))}
  }
  if ((lower.tail) && (log.p)) {
    f <- function(x) {log(1 - (x/threshold)^(1-exponent))}
  }
  p <- ifelse(x < threshold, NA, f(x))
  return(p)
}

# Quantiles of Pareto distributions
# Input: vector of probabilities, lower threshold, scaling exponent, usual flags
# Output: Vector of quantile values
qpareto <- function(p, threshold=1, exponent, lower.tail=TRUE, log.p=FALSE) {
  # Quantile function for Pareto distribution
  # P(x) = 1 - (x/xmin)^(1-a)
  # 1-p = (x(p)/xmin)^(1-a)
  # (1-p)^(1/(1-a)) = x(p)/xmin
  # xmin*((1-p)^(1/(1-a))) = x(p)
  # Upper quantile:
  # U(x) = (x/xmin)^(1-a)
  # u^(1/(1-a)) = x/xmin
  # xmin * u^(1/(1-a)) = x
  # log(xmin) + (1/(1-a)) log(u) = log(x)
  if (log.p) {
    p <- exp(p)
  }
  if (lower.tail) {
    p <- 1-p
  }
  # This works, via the recycling rule
  # q<-(p^(1/(1-exponent)))*threshold
  q.log <- log(threshold) + (1/(1-exponent))*log(p)
  q <- exp(q.log)
  return(q)
}

# Generate Pareto-distributed random variates
# Input: Integer size, lower threshold, scaling exponent
# Output: Vector of real-valued random variates
rpareto <- function(n, threshold=1, exponent) {
  # Using the transformation method, because we know the quantile function
  # analytically
  # Consider replacing with a non-R implementation of transformation method
  ru <- runif(n)
  r<-qpareto(ru,threshold,exponent)
  return(r)
}

# Estimate parameters of Pareto distribution
# A wrapper for functions implementing actual methods
# Input: data vector, lower threshold (or "find", indicating it should be found
#        from the data), method (likelihood or regression, defaulting to former)
# Output: List indicating type of distribution ("pareto"), parameters,
#         information about fit (depending on method), OR a warning and NA
#         if method is not recognized
pareto.fit <- function(data, threshold, method="ml") {
  if (threshold == "find") {
    return(.pareto.fit.threshold(data,method=method))
  }
  switch(method,
    ml = { return(.pareto.fit.ml(data,threshold)) },
    regression.cdf = { return(.pareto.fit.regression.cdf(data,threshold)) },
    { cat("Unknown method\n"); return(NA)}
  )
}

# Estimate lower threshold of Pareto distribution
# Use the method in Clauset, Shalizi and Newman (2007): consider each distinct
# data value as a possible threshold, fit using that threshold, and then find
# the Kolmogorov-Smirnov distance between estimated and empirical distributions.
# Pick the threshold which minimizes this distance.  Then function then returns
# the output of one of the fixed-threshold estimators.
# Input: data vector, method (defaulting to ML)
# Output: List indicating type of distribution ("pareto"), parameters,
#         information about fit (depending on method)
.pareto.fit.threshold <- function(data, method="ml") {
  possibles <- unique(data)
  ks.distances <- sapply(possibles,.ks.dist.for.pareto,data=data,method=method)
  min.index = which.min(ks.distances)
  min = possibles[min.index]
  return(pareto.fit(data,threshold=min,method=method))
}

# Calculate the KS discrepancy between a data set and its fit Pareto
# distribution, assuming a given threshold.  Not intended for users but rather
# for the .pareto.fit.threshold function.
# N.B., this KS statistic CANNOT be plugged in to the usual tables to find valid
# p-values, as the exponent has been estimated from the data.
# Input: real threshold, data vector, method flag
# Output: real-valued KS statistic
.ks.dist.for.pareto <- function(threshold,data,method="ml") {
  model <- pareto.fit(data,threshold=threshold,method=method)
  return(model$ks.dist)
}

# Calculate KS distanced between a data set and given Pareto distribution
# Not intended for users
# Input: real threshold, real exponent, data vector
# Output: real-valued KS statistic
.ks.dist.fixed.pareto <- function(data,threshold,exponent) {
  data <- data[data>=threshold]
  d <- suppressWarnings(ks.test(data,ppareto,threshold=threshold,exponent=exponent))
    # ks.test complains about p-values when there are ties, we don't care
  return(as.vector(d$statistic))
}


# Estimate scaling exponent of Pareto distribution by maximum likelihood
# Input: Data vector, lower threshold
# Output: List giving distribution type ("pareto"), parameters, log-likelihood
.pareto.fit.ml <- function (data, threshold) {
  data <- data[data>=threshold]
  n <- length(data)
  x <- data/threshold
  alpha <- 1 + n/sum(log(x))
  loglike = pareto.loglike(data,threshold,alpha)
  ks.dist <- .ks.dist.fixed.pareto(data,threshold=threshold,exponent=alpha)
  fit <- list(type="pareto", exponent=alpha, xmin=threshold, loglike = loglike,
              ks.dist = ks.dist, samples.over.threshold=n)
  return(fit)
}

# Calculate log-likelihood under a Pareto distribution
# Input: Data vector, lower threshold, scaling exponent
# Output: Real-valued log-likelihood
pareto.loglike <- function(x, threshold, exponent) {
  L <- sum(dpareto(x, threshold = threshold, exponent = exponent, log = TRUE))
  return(L)
}

# Log-log plot of the survival function (empirical upper CDF) of a data set
# Input: Data vector, lower limit, upper limit, graphics parameters
# Output: None (returns NULL invisibly)
plot.survival.loglog <- function(x,from=min(x),to=max(x),...) {
	plot.eucdf.loglog(x,from,to,...)
}
plot.eucdf.loglog <- function(x,from=min(x),to=max(x),type="l",...) {
   # Use the "eucdf" function (below)
   x <- sort(x)
   x.eucdf <- eucdf(x)
   # This is nice if the number of points is small...
   plot(x,x.eucdf(x),xlim=c(from,to),log="xy",type=type,...)
   # Should check how many points and switch over to a curve-type plot when
   # it gets too big
   invisible(NULL)
}

# Calculate the upper empirical cumulative distribution function of a
# one-dimensional data vector
# Uses the standard function ecdf
# Should, but does not yet, also produce a function of class "stepfun"
# (like ecdf)
# Input: data vector
# Output: a function
eucdf <- function(x) {
  # Exploit built-in R function to get ordinary (lower) ECDF, Pr(X<=x)
  x.ecdf <- ecdf(x)
  # Now we want Pr(X>=x) = (1-Pr(X<=x)) + Pr(X==x)
  # If x is one of the "knots" of the step function, i.e., a point with
  # positive probability mass, should add that in to get Pr(X>=x)
  # rather than Pr(X>x)
  away.from.knot <- function(y) { 1 - x.ecdf(y) }
  at.knot.prob.jump <- function(y) {
    x.knots = knots(x.ecdf)
    # Either get the knot number, or give zero if this was called
    # away from a knot
    k <- match(y,x.knots,nomatch=0)
    if ((k==0) || (k==1)) { # Handle special cases
      if (k==0) {
        prob.jump = 0 # Not really a knot
      } else {
        prob.jump = x.ecdf(y) # Special handling of first knot
      }
    } else {
      prob.jump = x.ecdf(y) - x.ecdf(x.knots[(k-1)]) # General case
    }
    return(prob.jump)
  }

  # Use one function or the other
  x.eucdf <- function(y) {
    baseline = away.from.knot(y)
    jumps = sapply(y,at.knot.prob.jump)
    ifelse (y %in% knots(x.ecdf), baseline+jumps, baseline)
  }
  return(x.eucdf)
}

# Calculate valid p-value for the goodness of fit of a power-law
# tail to a data set, via simulation
# Input: data vector (x), number of replications (m)
# Output: p-value
pareto.tail.ks.test <- function(x,m) {
  x.pt <- pareto.fit(x,threshold="find")
  x0 <- x.pt$xmin # extract parameters of fitted dist.
  alpha <- x.pt$exponent
  ntail <- sum(x>=x0) # How many samples in the tail?
  n <- length(x)
  ptail <- ntail/n # Total prob. of the tail
  # Carve out the non-tail data points
  body <- x[x < x0]
  # Observed value of KS distance:
  d.ks <- x.pt$ks.dist
  # KS statistics of resamples:
  r.ks <- replicate(m,.ks.resimulate.pareto.tail(n,ptail,x0,alpha,body))
  p.value <- sum(r.ks >= d.ks)/m
  return(p.value)
}

# Resimulate from a data set with a Pareto tail, estimate on
# the simulation and report the KS distance
# Inputs: Size of sample (n), probability of being in the tail (tail.p),
#         threshold for tail (threshold), power law exponent (exponent),
#         vector giving values in body (data.body)
# Output: KS distance
.ks.resimulate.pareto.tail <- function(n,tail.p,threshold,exponent,data.body) {
  # Samples come from the tail with probability ptail, or else from the body
  # decide randomly how many samples come from the tail
  tail.samples <- rbinom(1,n,tail.p)
  # Draw the samples from the tail
  rtail <- rpareto(tail.samples,threshold,exponent)
  # Draw the samples from the body (with replacement!)
  rbody <- sample(data.body,n-tail.samples,replace=TRUE)
  b <- c(rtail,rbody)
  b.ks <- pareto.fit(b,threshold="find")$ks.dist
  return(b.ks)
}



### The crappy linear regression way to fit a power law
# The common procedure is to fit to the binned density function, which is even
# crappier than to fit to the complementary distribution function; this
# currently only implements the latter

# First, produce the empirical complementary distribution function, as
# a pair of lists, {x}, {C(x)}
# Then regress log(C) ~ log(x)
# and report the slope and the R^2
# Input: Data vector, threshold
# Output: List with distributional parameters and information about the
#         fit
.pareto.fit.regression.cdf <- function(x,threshold=1) {
  # Discard data under threshold
  x <- x[x>=threshold]
  n <- length(x)
  # We need the different observed values of x, in order
  distinct_x <- sort(unique(x))
  x.eucdf <- eucdf(x)
  upper_probs <- x.eucdf(distinct_x)
  loglogfit <- lm(log(upper_probs) ~ log(distinct_x))
  intercept <- as.vector(coef(loglogfit)[1]) # primarily useful for plotting
  slope <- as.vector(-coef(loglogfit)[2]) # Remember sign of parameterization
  # But that's the exponent of the CDF, that of the pdf is one larger
  # and is what we're parameterizing by
  slope <- slope+1
  r2 <- summary(loglogfit)$r.squared
  loglike <- pareto.loglike(x, threshold, slope)
  ks.dist <- .ks.dist.fixed.pareto(x,threshold=threshold,exponent=slope)
  result <- list(type="pareto", exponent = slope, rsquare = r2,
                 log_x = log(distinct_x), log_p = log(upper_probs),
                 intercept = intercept, loglike = loglike, xmin=threshold,
                 ks.dist = ks.dist, samples.over.threshold=n)
  return(result)
}

# Wrapper function to just get the exponent estimate
loglogslope <- function(x,threshold=1) {
  llf <- .pareto.fit.regression.cdf(x,threshold)
  exponent <- llf$exponent
  return(exponent)
}

# Wrapper function to just get the R^2 values
loglogrsq <- function(x,threshold=1) {
  llf <- .pareto.fit.regression.cdf(x,threshold)
  r2 <- llf$rsquare
  return(r2)
}





# Revision history:
# no release	2003		First draft
# v 0.0		2007-06-04	First release
# v 0.0.1	2007-06-29	Fixed "not" for "knot" typo, thanks to
#				Nicholas A. Povak for bug report
# v 0.0.2	2007-07-22	Fixed bugs in plot.survival.loglog, thanks to
#				Stefan Wehrli for report
# v 0.0.3	2008-03-02	Realized R has a "unique" function; added
#				estimating xmin via method in minimal KS dist.
# v 0.0.4	2008-04-24	Made names of non-end-user functions start
#				with period, hiding them in workspace
# v 0.0.5       2011-02-03      Suppressed the warning ks.test produces about
#                               not being able to calculate p-values in the
#                               presence of ties
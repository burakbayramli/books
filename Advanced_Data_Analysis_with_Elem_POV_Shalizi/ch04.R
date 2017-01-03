# Code to accompany the chapter "Smoothing in Nonparametric Regression"

# Multi-fold cross-validation for univariate kernel regression
# Inputs: vector of input feature values (x)
  # vector of response values (y)
  # vector of bandwidths (bandwidths)
  # number of folds (num.folds)
# Calls: npreg from the np package
# Output: list with components of: best bandwidth, vector of
  # cross-validated  MSEs, array of MSE for each bandwidth on each fold
cv_bws_npreg <- function(x,y,bandwidths=(1:50)/50,num.folds=10) {
  require(np)
  n = length(x)
  # Sanity-check inputs
  require(np)
  n <- length(x)
  stopifnot(n> 1, length(y) == n)
  stopifnot(length(bandwidths) > 1)
  stopifnot(num.folds > 0, num.folds==trunc(num.folds))

  # Make a matrix to store MSEs for each fold/bandwidth combination
  fold_MSEs <- matrix(0,nrow=num.folds,ncol=length(bandwidths))
  # Name the columns after bandwidths for easy reference later
    # coerces the numerical bandwidths into character strings
  colnames(fold_MSEs) = bandwidths

  # Divide the data randomly into "folds" (non-overlapping testing sets)
  # Assign each data point to one of the "folds", in order (so that size is
    # as nearly equal as possible)
  case.folds <- rep(1:num.folds,length.out=n)
  # Now randomly permute the order (see help(sample) for how this works)
  case.folds <- sample(case.folds)

  for (fold in 1:num.folds) {
    train.rows = which(case.folds==fold)
    # Separate training and testing sets
    x.train = x[train.rows]
    y.train = y[train.rows]
    x.test = x[-train.rows]
    y.test = y[-train.rows]
    # Cycle over bandwidths
    for (bw in bandwidths) {
      fit <- npreg(txdat=x.train,tydat=y.train,
                   exdat=x.test,eydat=y.test,bws=bw)
        # Invoking npreg this way tells it to fit on the training data (txdat,
        # tydat), but evaluate on different data (exdat, eydat)
        # See help(npreg) for more
      fold_MSEs[fold,paste(bw)] <- fit$MSE
        # Here MSE is the MSE on the evaluation data
        # paste(bw): turns numerical bandwidth to type character, so R knows
        # it's the name of a column, not a column index
    }
  }
  # Average over folds
  CV_MSEs = colMeans(fold_MSEs)
  # Find the best bandwidth
  best.bw = bandwidths[which.min(CV_MSEs)]
  return(list(best.bw=best.bw,CV_MSEs=CV_MSEs,fold_MSEs=fold_MSEs))
}


# Code for figures omitted, see the text!
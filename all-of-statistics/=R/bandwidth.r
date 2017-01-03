### Cari Kaufman's modified bandwidth function.
### To use this:
### temp <- bw.fun(x)
### temp$h.optimal  is the cross-validation bandwidth
### plot(temp$bandwidths,temp$risk) to see the estimated risk function


bw.fun <- function (x, nb = 1000, lower = 0.1 * hmax, upper = hmax) {

  fucv <- function(h, x, n, d) .C("band_ucv_bin", as.integer(n), 
        as.integer(length(x)), as.double(d), x, as.double(h), 
        u = double(1), PACKAGE = "base")$u

  if (!is.numeric(x) || !length(x)) 
        stop("invalid x")

  n <- length(x)
  
  hmax <- 1.144 * sqrt(var(x)) * n^(-1/5)
  
  storage.mode(x) <- "double"

  Z <- .C("band_den_bin", as.integer(n), as.integer(nb), d = double(1), 
        x, cnt = integer(nb), PACKAGE = "base")
  
  d <- Z$d
  
  cnt <- as.integer(Z$cnt)
  
  h <- optimize(fucv, c(lower, upper), tol = 0.1 * lower, x = cnt, 
        n = n, d = d)$minimum

  if (h < 1.1 * lower | h > upper - 0.1 * lower) 
      warning("minimum occurred at one end of the range")

    h.vec <- seq(lower, upper, length=200)
    u.vec <- rep(NA, 200)
    for(i in 1:200)  u.vec[i] <- fucv(h.vec[i], x=cnt, n, d)

   list(h.optimal=h, bandwidths=h.vec, risk=u.vec)
}


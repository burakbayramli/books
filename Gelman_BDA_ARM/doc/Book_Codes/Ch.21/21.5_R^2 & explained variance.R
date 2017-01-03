## Setting up computations
rsquared <- 1 - mean (apply (e.y, 1, var)) / var (y)
e.a <- E.B[,,1]
e.b <- E.B[,,2]
rsquared.a <- 1 - mean (apply (e.a, 1, var)) / mean (apply (a, 1, var))
rsquared.b <- 1 - mean (apply (e.b, 1, var)) / mean (apply (b, 1, var))

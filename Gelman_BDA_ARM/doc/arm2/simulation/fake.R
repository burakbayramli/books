# fake data simulation for chapter 8

# simulate fake data and fit a simple regression

a <- 2.3
b <- 0.4
sigma <- 2.2
x <- 1:5
n <- length(x)
y <- a + b*x + rnorm (n, 0, sigma)
lm.1 <- lm (y ~ x)
display (lm.1)

# check coverage of 68% and 95% intervals for b

b.hat <- beta.hat(lm.1)[2]  # "b" is the 2nd coef in the model
b.se <- beta.se(lm.1)[2]
cover.68 <- abs (b - b.hat) < b.se   # this will be TRUE or FALSE
cover.95 <- abs (b - b.hat) < 2*b.se # this will be TRUE or FALSE
cat (paste ("cover.68: ", cover.68, "\n"))
cat (paste ("cover.95: ", cover.95, "\n"))

# do 1000 simulations

n.sims <- 1000
cover.68 <- rep (NA, n.sims)
cover.95 <- rep (NA, n.sims)
for (k in 1:n.sims){
  y <- a + b*x + rnorm (n, 0, sigma)
  lm.1 <- lm (y ~ x)
  b.hat <- beta.hat(lm.1)[2]
  b.se <- beta.se(lm.1)[2]
  cover.68[k] <- abs (b - b.hat) < b.se
  cover.95[k] <- abs (b - b.hat) < 2*b.se
}
cat (paste ("cover.68: ", mean(cover.68), "\n"))
cat (paste ("cover.95: ", mean(cover.95), "\n"))

# whoa!  do it again using the t interval!

n.sims <- 1000
cover.68 <- rep (NA, n.sims)
cover.95 <- rep (NA, n.sims)
t.68 <- qt (.83, n-2)
t.95 <- qt (.975, n-2)
for (k in 1:n.sims){
  y <- a + b*x + rnorm (n, 0, sigma)
  lm.1 <- lm (y ~ x)
  b.hat <- beta.hat(lm.1)[2]
  b.se <- beta.se(lm.1)[2]
  cover.68[k] <- abs (b - b.hat) < t.68*b.se
  cover.95[k] <- abs (b - b.hat) < t.95*b.se
}
cat (paste ("cover.68: ", mean(cover.68), "\n"))
cat (paste ("cover.95: ", mean(cover.95), "\n"))

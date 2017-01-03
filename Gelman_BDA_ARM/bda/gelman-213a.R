y <- c(24,25,31,31,22,21,26,20,16,22)

# simulation
theta <- rgamma(1000,sum(y))/10
y1986 <- rpois(1000,theta)
print (sort(y1986)[c(25,976)])


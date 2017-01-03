# some simulations for chapter 7

# #girls in 400 births

n.sims <- 1000
n.girls <- rbinom (n.sims, 400, 0.488)
postscript ("c:/books/multilevel/girls1.ps", height=2.8, width=3.5)
par (oma=c(0,0,0,0))
hist (n.girls, main="", xaxt="n", yaxt="n", mgp=c(1.5,.5,0))
axis (1, seq(150,250,25), mgp=c(1.5,.5,0))
axis (2, seq(0,200,100), mgp=c(1.5,.5,0))
dev.off()

# #girls in 400 birth events, allowing for the possibility of twins


n.babies <- rep (NA, n.sims)
n.girls <- rep (NA, n.sims)
for (i in 1:n.sims){
  birth.type <- sample (c ("fraternal twin", "identical twin", "single birth"),
    size=400, replace=TRUE, prob=c(1/125, 1/300, 1 - 1/125 - 1/300))
  babies <- ifelse (birth.type=="fraternal twin", 2,
    ifelse (birth.type=="identical twin", 2, 1))
  girls <- ifelse (birth.type=="fraternal twin", rbinom (400, 2, 0.495),
    ifelse (birth.type=="identical twin", 2*rbinom (400, 1, 0.495),
    rbinom (400, 1, 0.488)))
  n.babies[i] <- sum (babies)
  n.girls[i] <- sum (girls)
}

postscript ("c:/books/multilevel/girls2a.ps", height=3, width=3)
par (oma=c(0,0,0,0))
hist (n.girls, main="", xaxt="n", yaxt="n", mgp=c(1.5,.5,0))
axis (1, seq(150,250,25), mgp=c(1.5,.5,0))
axis (2, seq(0,200,100), mgp=c(1.5,.5,0))
dev.off()

postscript ("c:/books/multilevel/girls2b.ps", height=3, width=3)
par (oma=c(0,0,0,0))
hist (n.babies, main="", xaxt="n", yaxt="n", mgp=c(1.5,.5,0))
axis (1, seq(350,450,25), mgp=c(1.5,.5,0))
axis (2, seq(0,200,100), mgp=c(1.5,.5,0))
dev.off()

postscript ("c:/books/multilevel/girls2c.ps", height=3, width=3)
par (oma=c(0,0,0,0))
plot (n.babies, n.girls, main="", xaxt="n", yaxt="n", mgp=c(1.5,.5,0))
axis (1, seq(350,450,25), mgp=c(1.5,.5,0))
axis (2, seq(150,250,25), mgp=c(1.5,.5,0))
dev.off()


# avg height of 10 adults

n.sims <- 1000
avg.height <- rep (NA, n.sims)
for (s in 1:n.sims){
  sex <- rbinom (10, 1, 0.52)
  height <- ifelse (sex==0, rnorm (10, 69.5, 2.9), rnorm (10, 64.5, 2.7))
  avg.height[s] <- mean (height)
}
hist (avg.height)

# using functions

height.sim <- function (n.adults){
  sex <- rbinom (n.adults, 1, 0.52)
  height <- ifelse (sex==0, rnorm (10, 69.5, 2.9), rnorm (10, 64.5, 2.7))
  return (list (avg=mean(height), max=max(height)))
}

#heights <- sapply (rep(10,1000), height.sim)

heights <- replicate (1000, height.sim (10))

n.adults <- 10
n.sims <- 1000
avg.height <- rep (NA, n.sims)
max.height <- rep (NA, n.sims)
for (s in 1:n.sims){
  simulation <- height.sim (n.adults)
  avg.height[s] <- simulation$avg
  max.height[s] <- simulation$max
}


# approx earnings of 68-inch-tall man

n.sims <- 1000
pred <- exp (rnorm (n.sims, 9.95, 0.88))
postscript ("c:/books/multilevel/sim.hist.a.ps", height=2.8, width=3.5)
par (oma=c(0,0,0,0))
hist (log(pred), main="", xaxt="n", yaxt="n", mgp=c(1.5,.5,0), xlab="log (earnings)")
axis (1, seq(6,14,2), mgp=c(1.5,.5,0))
axis (2, seq(0,200,100), mgp=c(1.5,.5,0))
dev.off()
postscript ("c:/books/multilevel/sim.hist.b.ps", height=2.8, width=3.5)
par (oma=c(0,0,0,0))
hist (pred, main="", xaxt="n", yaxt="n", mgp=c(1.5,.5,0), breaks=100000*seq(0,ceiling(max(pred)/100000),.25),
      xlab="earnings")
axis (1, seq(0,600000,200000), c("0", "200,000", "400,000", "600,000"), mgp=c(1.5,.5,0))
axis (2, seq(0,600,200), mgp=c(1.5,.5,0))
dev.off()

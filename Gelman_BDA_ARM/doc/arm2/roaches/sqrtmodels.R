# square root models

baseline <- read.csv ("IPM_BASELINE_R2_032006.csv")
attach.all (baseline)

sqrt.roach1 <- sqrt (roach1)
sqrt.roach2 <- sqrt (roach2)

M1 <- lm (sqrt.roach2 ~ sqrt.roach1 + treatment)
display (M1)

M1a <- lm (sqrt.roach2 ~ sqrt.roach1 + treatment + factor(building))
display (M1a)

M2 <- lmer (sqrt.roach2 ~ sqrt.roach1 + treatment + (1 | building))
display (M2)

M3 <- lmer (sqrt.roach2 ~ sqrt.roach1 + treatment + senior + (1 | building))
display (M3)

# effects of -1.27

# if it were 10 under the control
print ((sqrt(10) - 1.27)^2)

# start at 50
print ((sqrt(50) - 1.27)^2)

# start at 100
print ((sqrt(100) - 1.27)^2)

# start at 1000
print ((sqrt(1000) - 1.27)^2)


# effects of -1.27 - 2*.59 = -2.45

# if it were 10 under the control
print ((sqrt(10) - 2.45)^2)
# etc



sqrt.diff <- function (start, change){
  return (new.value=(sqrt(start) + change)^2)
}

diffs <- function (beta.hat){ 
  cat ("assuming beta.hat =", beta.hat, "\n")
  for (start in c(0,5,10,50,100)){
    cat ("instead of", start, ", it's", sqrt.diff (start, beta.hat),
         "(on average)\n")
  }
}

diffs (fixef(M3)["treatment"])
diffs (fixef(M3)["treatment"] + 2*se.fixef(M3)["treatment"])
diffs (fixef(M3)["treatment"] - 2*se.fixef(M3)["treatment"])

# redo in terms of roach1

b.hat <- fixef (M3)
s.hat <- sqrt (sigma.hat(M3)$sigma$data^2 + sigma.hat(M3)$sigma$building^2)

diffs2 <- function (senior.value, beta, sigma){
  r1 <- median(roach1[senior==senior.value])*c(.5,1,2)
  N <- length (r1)
  pred.T <- rep (NA, N)
  pred.C <- rep (NA, N)
  for (i in 1:N){
    x.T <- cbind (1, sqrt(r1[i]), 1, senior.value)  # treatment
    x.C <- cbind (1, sqrt(r1[i]), 0, senior.value)  # control
    pred.T[i] <- mean (rnorm (500, x.T %*% beta, sigma)^2)
    pred.C[i] <- mean (rnorm (500, x.C %*% beta, sigma)^2)
  }
  output <- cbind (r1, pred.T, pred.C, pred.C - pred.T)
  dimnames(output) <- list (c ("low", "median", "high"),
                            c ("r1", "pred.T", "pred.C", "pred.T - pred.C"))
  return (output)
}

diffs2 (0, b.hat, s.hat)
diffs2 (1, b.hat, s.hat)

# now embed in a simulation

n.sims <- 200
beta.sim <- sim (M3, n.sims)$unmodeled

diffs2.sim <- function (senior.value, beta.sim, sigma){
  n.sims <- nrow (beta.sim)
  output <- array (NA, c(n.sims,3,4))
  for (s in 1:n.sims){
    output[s,,] <- diffs2 (senior.value, beta.sim[s,], sigma)
  }
  for (j in 1:3){
    r1 <- mean (output[,j,1])
    mean.T <- mean (output[,j,2])
    mean.C <- mean (output[,j,3])
    mean.diff <- mean (output[,j,4])
    i50.diff <- quantile (output[,j,4], c(.25, .75))
    i95.diff <- quantile (output[,j,4], c(.025, .975))
    cat ("Senior.value = ", senior.value, ":  If roach1 = ", fround(r1,1),
         ", then mean.T = ", fround(mean.T,1), ", mean.C = ", fround(mean.C,1),
         ",\n  and 50% and 95% conf intervals for diff are [",
         fround(i50.diff[1], 1), ", ", fround(i50.diff[2], 1), "] and [",
         fround(i95.diff[1], 1), ", ", fround(i95.diff[2], 1), "]\n", sep="")
  }
}

diffs2.sim (0, beta.sim, s.hat)
diffs2.sim (1, beta.sim, s.hat)



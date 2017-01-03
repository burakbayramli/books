## A simple example of discrete predictive simulations

n.girls <- rbinom (1, 400, .488)
print (n.girls)

n.sims <- 1000
n.girls <- rep (NA, n.sims)
for (s in 1:n.sims){
  n.girls[s] <- rbinom (1, 400, .488)
}
hist (n.girls, main="")

 # equivalently

n.sims <- 1000
n.girls <- rbinom (n.sims, 400, .488)
hist (n.girls, main="")

## Accounting for twins

birth.type <- sample (c("fraternal twin", "identical twin", "single birth"),
  size=400, replace=TRUE, prob=c(1/25, 1/300, 1 - 1/25 - 1/300))
girls <- rep (NA, 400)
for (i in 1:400){
  if (birth.type[i]=="single birth"){
   girls[i] <- rbinom (1, 1, .488)}
  else if (birth.type[i]=="identical twin"){
   girls[i] <- 2*rbinom (1, 1, .495)}
  else if (birth.type[i]=="fraternal twin"){
   girls[i] <- rbinom (1, 2, .495)}
}
n.girls <- sum (girls)

 # putting in a loop

n.sims <- 1000
n.girls <- rep (NA, n.sims)
for (s in 1:n.sims){
 birth.type <- sample (c("fraternal twin", "identical twin", "single birth"),
   size=400, replace=TRUE, prob=c(1/25, 1/300, 1 - 1/25 - 1/300))
 girls <- rep (NA, 400)
 for (i in 1:400){
  if (birth.type[i]=="single birth"){
   girls[i] <- rbinom (1, 1, .488)}
  else if (birth.type[i]=="identical twin"){
   girls[i] <- 2*rbinom (1, 1, .495)}
  else if (birth.type[i]=="fraternal twin"){
   girls[i] <- rbinom (1, 2, .495)}
}
n.girls[s] <- sum (girls)
}

 # or

girls <- ifelse (birth.type=="single birth", rbinom (400, 1, .488),
 ifelse (birth.type=="identical twin", 2*rbinom (400, 1, .495),
 rbinom (400, 2, .495)))

## A simple example of continuos predictive simulations

woman <- rbinom (10, 1, .52)
height <- ifelse (woman==0, rnorm (10, 69.1, 2.9), rnorm (10, 64.5, 2.7))
avg.height <- mean (height)
print(avg.height)

 # simulation & Figure 7.1

n.sims <- 1000
avg.height <- rep (NA, n.sims)
for (s in 1:n.sims){
  sex <- rbinom (10, 1, .52)
  height <- ifelse (sex==0, rnorm (10, 69.1, 2.9), rnorm (10, 64.5, 2.7))
  avg.height[s] <- mean (height)
}
hist (avg.height, main="Average height of 10 adults") 

 # simulation for the maximum height

n.sims <- 1000
max.height <- rep (NA, n.sims)
for (s in 1:n.sims){
  sex <- rbinom (10, 1, .52)
  height <- ifelse (sex==0, rnorm (10, 69.1, 2.9), rnorm (10, 64.5, 2.7))
  max.height[s] <- max (height)
}
hist (max.height, main="Maximum height of 10 adults")

## Simulation using custom-made functions

Height.sim <- function (n.adults){
  sex <- rbinom (n.adults, 1, .52)
  height <- ifelse (sex==0, rnorm (10, 69.1, 2.9), rnorm (10, 64.5, 2.7))
  return (mean(height))
}

avg.height <- replicate (1000, Height.sim (n.adults=10))
hist (avg.height, main="Average height of 10 adults")

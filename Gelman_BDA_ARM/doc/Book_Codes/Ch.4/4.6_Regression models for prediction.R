## Read and rename the data

library("arm")
mesquite <- read.table("mesquite.dat",header=TRUE)
attach(mesquite)
# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/mesquite

 # Rename variables
weight <- LeafWt
canopy.height <- CanHt
total.height <- TotHt
density <- Dens
group <- ifelse (Group == "MCD", 0, 1)
diam1 <- Diam1
diam2 <- Diam2

## First model
fit.1 <- lm (weight ~ diam1 + diam2 + canopy.height + total.height + density + group)
display(fit.1)

## Data summary
summary(mesquite)
IQR(diam1)
IQR(diam2)
IQR(canopy.height)
IQR(total.height)
IQR(density)
IQR(group)

## Other models

 # Log model
fit.2 <- lm (log(weight) ~ log(diam1) + log(diam2) + log(canopy.height) + log(total.height) + 
   log(density) + group)
display(fit.2)

 # Volume model
canopy.volume <- diam1*diam2*canopy.height

fit.3 <- lm (log(weight) ~ log(canopy.volume))
display(fit.3)

 # Volume, area & shape model
canopy.area <- diam1*diam2
canopy.shape <- diam1/diam2

fit.4 <- lm (log(weight) ~ log(canopy.volume) + log(canopy.area) + log(canopy.shape) + 
  log(total.height) + log(density) + group)
display(fit.4)

 # Last two models
fit.5 <- lm (log(weight) ~ log(canopy.volume) + log(canopy.area) + group)
display(fit.5)

fit.6 <- lm (log(weight) ~ log(canopy.volume) + log(canopy.area) + log(canopy.shape) + 
  log(total.height) + group)
display(fit.6)
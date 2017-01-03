# The data came as an Excel file (ProfEvaltnsBeautyPublic.xls)
# I went into Excel and saved it as a .csv file (comma-separated version)

# Read the data into R, including the variable names (headers)
beauty.data <- read.table ("ProfEvaltnsBeautyPublic.csv", header=T, sep=",")

# Attach the data so they are accessible using their variable names
attach.all (beauty.data)

###############################################################################
# Do more beautiful profs get higher evaluations?
###############################################################################

# Rename the two variables for convenience
beauty <- btystdave
eval <- courseevaluation

# Make a scatterplot
plot (beauty, eval)

# Make a scatterplot, labeling the axes
plot (beauty, eval, xlab="beauty", ylab="average teaching evaluation")

# Fit a linear regression
lm.1 <- lm (eval ~ beauty)
display (lm.1)

# Display the regression line, added onto the scatterplot (add=TRUE)
curve (lm.1$coef[1] + lm.1$coef[2]*x, add=T)

# Add dotted lines to show +/- 1 standard deviation
curve (lm.1$coef[1] + lm.1$coef[2]*x + summary(lm.1)$sigma, lty=2, add=T)
curve (lm.1$coef[1] + lm.1$coef[2]*x - summary(lm.1)$sigma, lty=2, add=T)


###############################################################################
# Do things differ for male and female profs?  Parallel regression lines
###############################################################################

lm.2 <- lm (eval ~ beauty + female)
display (lm.2)

# Set up a 2x2 grid of plots
par (mfrow=c(2,2))

# Make separate plots for men and women
plot (beauty[female==0], eval[female==0], xlim=range(beauty), ylim=range(eval),
      xlab="beauty", ylab="average teaching evaluation", main="Men")
curve (lm.2$coef[1] + lm.2$coef[2]*x + lm.2$coef[3]*0, add=T)
plot (beauty[female==1], eval[female==1], xlim=range(beauty), ylim=range(eval),
      xlab="beauty", ylab="average teaching evaluation", main="Women")
curve (lm.2$coef[1] + lm.2$coef[2]*x + lm.2$coef[3]*1, add=T)

# Display both sexes on the same plot
# First make the plot with type="n" (which displays axes but does not plot
#   the points), then plot the points and lines separately for each sex
plot (beauty, eval, xlab="beauty", ylab="average teaching evaluation",
      main="Both sexes", type="n")
points (beauty[female==0], eval[female==0], col="blue")
curve (lm.2$coef[1] + lm.2$coef[2]*x + lm.2$coef[3]*0, add=T, col="blue")
points (beauty[female==1], eval[female==1], col="red")
curve (lm.2$coef[1] + lm.2$coef[2]*x + lm.2$coef[3]*1, add=T, col="red")

###############################################################################
# Do things differ for male and female profs?  Non-parallel regression lines
###############################################################################

lm.3 <- lm (eval ~ beauty + female + beauty*female)
display (lm.3)

# Set up a new 2x2 grid of plots
par (mfrow=c(2,2))

# Display the parallel regression lines in gray and the non-parallel lines
#   in heavy black
plot (beauty[female==0], eval[female==0], xlim=range(beauty), ylim=range(eval),
      xlab="beauty", ylab="average teaching evaluation", main="Men")
curve (lm.2$coef[1] + lm.2$coef[2]*x + lm.2$coef[3]*0,
       lwd=.5, col="gray", add=T)
curve (lm.3$coef[1] + lm.3$coef[2]*x + lm.3$coef[3]*0 + lm.3$coef[4]*x*0,
       lwd=2, col="black", add=T)

plot (beauty[female==1], eval[female==1], xlim=range(beauty), ylim=range(eval),
      xlab="beauty", ylab="average teaching evaluation", main="Women")
curve (lm.2$coef[1] + lm.2$coef[2]*x + lm.2$coef[3]*1,
       lwd=.5, col="gray", add=T)
curve (lm.3$coef[1] + lm.3$coef[2]*x + lm.3$coef[3]*1 + lm.3$coef[4]*x*1,
       lwd=2, col="black", add=T)

###############################################################################
# More models
###############################################################################

lm.4 <- lm (eval ~ beauty + female + age)
display (lm.4)

lm.5 <- lm (eval ~ beauty + female + minority)
display (lm.5)

lm.6 <- lm (eval ~ beauty + female + nonenglish)
display (lm.6)

lm.7 <- lm (eval ~ beauty + female + nonenglish + lower)
display (lm.7)

###############################################################################
# Go back to simple model, add course indicators
###############################################################################

# Create the course index variable
courses <- beauty.data[,18:47]   # (indicators for the 30 courses)
n <- nrow (beauty.data)
J <- ncol (courses) + 1
course.id <- rep (0, n)
for (i in 1:n){
  for (j in 1:30){
    if (courses[i,j]==1) course.id[i] <- j
  }
}

# include course indicators in a regression

lm.8 <- lm (eval ~ beauty + factor(course.id))
display (lm.8)

# fit a multilevel model

data <- list ("eval", "beauty", "course.id", "n", "J")
inits <- function(){
  list (beta.0=rnorm(1), beta.beauty=rnorm(1), alpha=rnorm(J),
        sigma.eval=runif(1), sigma.alpha=runif(1))}
parameters <- c ("eval.hat", "beta.0", "beta.beauty", "alpha", "sigma.eval", "sigma.alpha")
bugs.1 <- bugs (data, inits, parameters, "beauty.bug", n.chains=3, n.iter=1000)

print (bugs.1)
plot (bugs.1)

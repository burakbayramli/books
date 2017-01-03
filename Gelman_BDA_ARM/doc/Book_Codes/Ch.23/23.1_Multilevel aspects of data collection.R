## Read the data
# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/electric.company

# The R codes & data files should be saved in the same directory for
# the source command to work

source("9.3_Randomized experiments.R") # where data was cleaned

## Hierarchical model including pair indicators
pair <- rep (1:nrow(electric), 2)
grade.pair <- grade[1:nrow(electric)]
y <- post.test
n <- length(y)
n.grade <- max(grade)
n.pair <- max(pair)

# fitting all 4 grades at once (without controlling for pretest)
data <- c("n", "y", "n.grade", "grade", "grade.pair", "treatment", "n.pair", "pair")
inits <- function(){
  list (mu.a=rnorm(n.grade), theta=rnorm(n.grade), a=rnorm(n.pair),
        sigma.y=runif(n.grade), sigma.a=runif(n.grade))}
params <- c("theta","a","mu.a","sigma.y","sigma.a")

electric.ch23.1a <- bugs (data, inits, params, "electric.ch23.1a.bug", n.iter=5000, 
         bugs.directory="c:/.../", working.directory=NULL, clearWD=TRUE, debug=TRUE )
plot (electric.ch23.1a)

# simple model controlling for pre-test
data <- c("n", "y", "pre.test", "treatment", "n.pair", "pair")
inits <- function(){
  list (mu.a=rnorm(1), theta=rnorm(1), a=rnorm(n.pair),
        sigma.y=runif(1), sigma.a=runif(1),
        b.pre.test=rnorm(1))}
params <- c("theta","a","sigma.y","sigma.a","b.pre.test")

electric.ch23.1b <- bugs (data, inits, params, "electric.ch23.1b.bug", n.iter=5000,
     bugs.directory="c:/.../", working.directory=NULL, clearWD=TRUE, debug=TRUE )
plot (electric.ch23.1b)

# fitting all 4 grades at once (controlling for pretest)
data <- c("n", "y", "n.grade", "grade", "grade.pair", "treatment", "n.pair", "pair", "pre.test")
inits <- function(){
  list (mu.a=rnorm(n.grade), theta=rnorm(n.grade), a=rnorm(n.pair),
        sigma.y=runif(n.grade), sigma.a=runif(n.grade), b.pre.test=rnorm(n.grade))}
params <- c("theta","a","mu.a","sigma.y","sigma.a", "b.pre.test")

electric.ch23.1c <- bugs (data, inits, params, "electric.ch23.1c.bug", n.iter=5000, 
     bugs.directory="c:/.../", working.directory=NULL, clearWD=TRUE, debug=TRUE )
plot (electric.ch23.1c)

## Plot Figure 23.1
est3 <- electric.ch23.1a$mean$theta
se3 <- electric.ch23.1a$sd$theta
est4 <- electric.ch23.1c$mean$theta
se4 <- electric.ch23.1c$sd$theta

 # function to make a graph out of the regression coeffs and se's
regression.2tables <- function (name, est3, est4, se3, se4, label1, label2, file,
     bottom=FALSE){
  J <- length(name)
  name.range <- .6
  x.range <- range (est3+2*se3, est3-2*se3, est4+2*se4, est4-2*se4)
  A <- -x.range[1]/(x.range[2]-x.range[1])
  B <- 1/(x.range[2]-x.range[1])
  height <- .6*J
  width <- 8*(name.range+1)
  gap <- .4
  par (mar=c(4,0,0,0))
  plot (c(-name.range,2+gap), c(3,-J-2), bty="n", xlab="", ylab="", xaxt="n",
     yaxt="n", xaxs="i", yaxs="i", type="n")
  text (-name.range, 2, "Subpopulation", adj=0, cex=.9)
  text (.5, 2, label1, adj=.5, cex=.9)
  text (1+gap+.5, 2, label2, adj=.5, cex=.9)
  lines (c(0,1), c(0,0))
  lines (1+gap+c(0,1), c(0,0))
  lines (c(A,A), c(0,-J-1), lty=2, lwd=.5)
  lines (1+gap+c(A,A), c(0,-J-1), lty=2, lwd=.5)
  ax <- pretty (x.range)
  ax <- ax[(A+B*ax)>0 & (A+B*ax)<1]
  segments (A + B*ax, -.1, A + B*ax, .1, lwd=.5)
  segments (1+gap+A + B*ax, -.1, 1+gap+A + B*ax, .1, lwd=.5)
  text (A + B*ax, .7, ax, cex=.9)
  text (1+gap+A + B*ax, .7, ax, cex=.9)
  text (-name.range, -(1:J), name, adj=0, cex=.9)
  points (A + B*est3, -(1:J), pch=20, cex=.9)
  points (1+gap+A + B*est4, -(1:J), pch=20, cex=.9)
  segments (A + B*(est3-se3), -(1:J), A + B*(est3+se3), -(1:J), lwd=3)
  segments (1+gap+A + B*(est4-se4), -(1:J), 1+gap+A + B*(est4+se4), -(1:J), lwd=3)
  segments (A + B*(est3-2*se3), -(1:J), A + B*(est3+2*se3), -(1:J), lwd=.5)
  segments (1+gap+A + B*(est4-2*se4), -(1:J), 1+gap+A + B*(est4+2*se4), -(1:J), 
     lwd=.5)
  if (bottom){
    lines (c(0,1), c(-J-1,-J-1))
    lines (1+gap+c(0,1), c(-J-1,-J-1))
    segments (A + B*ax, -J-1-.1, A + B*ax, -J-1+.1, lwd=.5)
    segments (1+gap+A + B*ax, -J-1-.1, 1+gap+A + B*ax, -J-1+.1, lwd=.5)
    text (A + B*ax, -J-1-.7, ax, cex=.9)
    text (1+gap+A + B*ax, -J-1-.7, ax, cex=.9)
  } 
}

regression.2tables (paste ("Grade", 1:4), est3, est4, se3, se4, "Regression on treatment indicator,\ncontrolling for pairs", "Regression on treatment indicator,\ncontrolling for pairs and pre-test")

## Figure 23.2 comparing all the se's

 # define est1, est2, se1, se2 (from models on chapter 9)
est1 <- rep(NA,4)
est2 <- rep(NA,4)
se1 <- rep(NA,4)
se2 <- rep(NA,4)
for (k in 1:4){
  lm.1 <- lm (post.test ~ treatment, subset=(grade==k))
  lm.2 <- lm (post.test ~ treatment + pre.test, subset=(grade==k))
  est1[k] <- lm.1$coef[2]
  est2[k] <- lm.2$coef[2]
  se1[k] <- summary(lm.1)$coef[2,2]
  se2[k] <- summary(lm.2)$coef[2,2]
}

 # make the plot

# analyze replace/supplement
supp <- c(as.numeric(electric[,"Supplement."])-1, rep(NA,nrow(electric)))
# supp=0 for replace, 1 for supplement, NA for control

par (mfrow=c(2,2), mar=c(5,6.5,4,2)+.1)
ses <- cbind(se1,se2,se3,se4)
for (k in 1:4){
  ok <- (grade==k)&(!is.na(supp))
  se.grade <- ses[k,]
  plot (1:4, se.grade, xlim=c(.8,4.2), ylim=c(0,max(ses)*1.1), xlab="",
        ylab="s.e. of estimated\ntreatment effect", yaxs="i",
        main=paste("grade", k),
        cex.axis=1.1, cex.lab=1.1, cex.main=1.2, xaxt="n", pch=19)
  axis (1, 1:4, c("--\n","pre\n","pair\n","pre,\npair"), cex.axis=1.1,
        tck=0, mgp=c(3,1.8,0))
  abline (0, 0, lty=2, lwd=.5)
}

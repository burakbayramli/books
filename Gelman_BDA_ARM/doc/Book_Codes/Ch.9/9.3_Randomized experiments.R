## Read in the data
# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/electric.company

library ("arm")
electric <- read.table ("electric.dat", header=T)
attach.all (electric)

## Plot of the raw data (Figure 9.4)

onlytext <- function(string){
  plot(0:1, 0:1, bty='n', type='n', xaxt='n', yaxt='n', xlab='', ylab='')
  text(0.5, 0.5, string, cex=1.2, font=2)
}
nf <- layout (matrix(c(0,1:14), 5, 3, byrow=TRUE), c(5, 10, 10),
             c(1, 5, 5, 5, 5), TRUE)
par (mar=c(.2,.2,.2,.2))
onlytext ('Test scores in control classes')
onlytext ('Test scores in treated classes')

par (mar=c(1,1,1,1), lwd=0.7)
for (j in 1:4){
  onlytext(paste ('Grade', j))
  hist (control.Posttest[Grade==j], breaks=seq(0,125,5), xaxt='n', yaxt='n',
    main=NULL, col="gray", ylim=c(0,10))
  axis(side=1, seq(0,125,50), line=-.25, cex.axis=1, mgp=c(1,.2,0), tck=0)
  text (2, 6.5, paste ("mean =", round (mean(control.Posttest[Grade==j]))), adj=0)
  text (2, 5, paste ("  sd =", round (sd(control.Posttest[Grade==j]))), adj=0)

  hist (treated.Posttest[Grade==j], breaks=seq(0,125,5), xaxt='n', yaxt='n',
    main=NULL, col="gray", ylim=c(0,10))
  axis(side=1, seq(0,125,50), line=-.25, cex.axis=1, mgp=c(1,.2,0), tck=0)
  text (2, 6.5, paste ("mean =", round (mean(treated.Posttest[Grade==j]))), adj=0)
  text (2, 5, paste ("  sd =", round (sd(treated.Posttest[Grade==j]))), adj=0)
}

## Basic analysis of a completely randomized experiment

post.test <- c (treated.Posttest, control.Posttest)
pre.test <- c (treated.Pretest, control.Pretest)
grade <- rep (Grade, 2)
treatment <- rep (c(1,0), rep(length(treated.Posttest),2))
n <- length (post.test)

for (k in 1:4){
  display (lm (post.test ~ treatment, subset=(grade==k)))
}

## Plot of the regression results (Figure 9.5)

est1 <- rep(NA,4)
est2 <- rep(NA,4)
se1 <- rep(NA,4)
se2 <- rep(NA,4)
for (k in 1:4){
  lm.1 <- lm (post.test ~ treatment, subset=(grade==k))
  lm.2 <- lm (post.test ~ treatment + pre.test, subset=(grade==k))
  est1[k] <- coef(lm.1)[2]
  est2[k] <- coef(lm.2)[2]
  se1[k] <- se.coef(lm.1)[2]
  se2[k] <- se.coef(lm.2)[2]
}

 # function to make a graph out of the regression coeffs and se's

regression.2tables <- function (name, est1, est2, se1, se2, label1, label2,
                                file, bottom=FALSE){
  J <- length(name)
  name.range <- .6
  x.range <- range (est1+2*se1, est1-2*se1, est2+2*se2, est1-2*se2)
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
  points (A + B*est1, -(1:J), pch=20, cex=.9)
  points (1+gap+A + B*est2, -(1:J), pch=20, cex=.9)
  segments (A + B*(est1-se1), -(1:J), A + B*(est1+se1), -(1:J), lwd=3)
  segments (1+gap+A + B*(est2-se2), -(1:J), 1+gap+A + B*(est2+se2), -(1:J),
            lwd=3)
  segments (A + B*(est1-2*se1), -(1:J), A + B*(est1+2*se1), -(1:J), lwd=.5)
  segments (1+gap+A + B*(est2-2*se2), -(1:J), 1+gap+A + B*(est2+2*se2), -(1:J),
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

                  # graphs on Figure 9.5

par (mfrow=c(1,1))
regression.2tables (paste ("Grade", 1:4), est1, est2, se1, se2, 
  "Regression on treatment indicator", "Regression on treatment indicator,
  \ncontrolling for pre-test")

## Controlling for pre-treatment predictors (Figure 9.6)

par (mfrow=c(1,4), pty="s")
x.range <- cbind (c(5,40,40,40), c(25,125,125,125))
for (j in 1:4){
  ok <- Grade==j
  x <- c (treated.Pretest[ok], control.Pretest[ok])
  y <- c (treated.Posttest[ok], control.Posttest[ok])
  t <- rep (c(1,0), rep(sum(ok),2))
#  plot (x.range[j,], c(40,125), type="n", main=paste("grade",j), xaxs="i", yaxs="i",
   plot (c(0,125), c(0,125), type="n", main=paste("grade",j), xaxs="i",
         yaxs="i", xlab=expression(paste("pre-test, ",x[i])),
         ylab=expression(paste("post-test, ",y[i])),
         cex.axis=1.0, cex.lab=1.1, cex.main=1.2, mgp=c(2.5,.7,0))
  lm.1 <- lm (y ~ x + t)
  abline (coef(lm.1)[1], coef(lm.1)[2], lwd=.5, lty=2)
  abline (coef(lm.1)[1] + coef(lm.1)[3], coef(lm.1)[2], lwd=.5)
  points (control.Pretest[ok], control.Posttest[ok], pch=20, cex=1.1)
  points (treated.Pretest[ok], treated.Posttest[ok], pch=21, cex=1.1)
}

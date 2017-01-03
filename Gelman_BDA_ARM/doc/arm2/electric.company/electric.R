# analysis of "electric company" data

electric <- read.table ("electric.dat", header=T)
attach.all (electric)


# plot of raw data for mlm book

postscript ("c:/books/multilevel/electricdata.ps", horizontal=F, height=7, width=6)
onlytext<-function(string){
  plot(0:1, 0:1, bty='n', type='n', xaxt='n', yaxt='n', xlab='', ylab='')
  text(0.5, 0.5, string, cex=1.2, font=2)
}
nf<-layout(matrix(c(0,1:14), 5, 3, byrow=TRUE), c(5, 10, 10), c(1, 5, 5, 5, 5), TRUE)
#layout.show(nf)

par(mar=c(.2, .2, .2, .2))

onlytext('Test scores in control classes')
onlytext('Test scores in treated classes')

par(mar=c(1, 1, 1, 1), lwd=0.7)

attach.all (electric)
for (j in 1:4){
  onlytext(paste ('Grade', j))

  hist (control.Posttest[Grade==j], breaks=seq(0,125,5), xaxt='n', yaxt='n', main=NULL, col="gray", ylim=c(0,10))
  axis(side=1, seq(0,125,50), line=-.25, cex.axis=1, mgp=c(1,.2,0), tck=0)
  text (2, 6.5, paste ("mean =", round (mean(control.Posttest[Grade==j]))), adj=0)
  text (2, 5, paste ("  sd =", round (sd(control.Posttest[Grade==j]))), adj=0)

  hist (treated.Posttest[Grade==j], breaks=seq(0,125,5), xaxt='n', yaxt='n', main=NULL, col="gray", ylim=c(0,10))
  axis(side=1, seq(0,125,50), line=-.25, cex.axis=1, mgp=c(1,.2,0), tck=0)
  text (2, 6.5, paste ("mean =", round (mean(treated.Posttest[Grade==j]))), adj=0)
  text (2, 5, paste ("  sd =", round (sd(treated.Posttest[Grade==j]))), adj=0)
}

dev.off()

# plot the data the other way

postscript ("c:/books/multilevel/electricdata.horizontal.ps", horizontal=F, height=6, width=7)
onlytext<-function(string){
  plot(0:1, 0:1, bty='n', type='n', xaxt='n', yaxt='n', xlab='', ylab='')
  text(0.5, 0.5, string, cex=1.2, font=2)
}
nf<-layout(matrix(c(0,1:14), 3, 5, byrow=FALSE), c(5, 10, 10, 10, 10), c(1, 5, 5), TRUE)
#layout.show(nf)

par(mar=c(.2, .2, .2, .2))

onlytext('Control\nclasses')
onlytext('Treated\nclasses')

par(mar=c(.2,.4,.2,.4), lwd=.5)

attach.all (electric)
for (j in 1:4){
  onlytext (paste ('Grade', j))

  hist (control.Posttest[Grade==j], breaks=seq(40,125,5), xaxt='n', yaxt='n', main=NULL, col="gray", ylim=c(0,14))
  axis(side=1, seq(50,100,25), line=-.25, cex.axis=1, mgp=c(1,.2,0), tck=0, lty="blank")
  lines (rep(mean(control.Posttest[Grade==j]),2), c(0,11), lwd=2)

  hist (treated.Posttest[Grade==j], breaks=seq(40,125,5), xaxt='n', yaxt='n', main=NULL, col="gray", ylim=c(0,14))
  axis(side=1, seq(50,100,25), line=-.25, cex.axis=1, mgp=c(1,.2,0), tck=0, lty="blank")
  lines (rep(mean(treated.Posttest[Grade==j]),2), c(0,11), lwd=2)
}

dev.off()



attach.all (electric)
postscript ("c:/books/multilevel/electricscatter1a.ps", horizontal=T, height=4)
par (mfrow=c(1,4), pty="s")
x.range <- cbind (c(5,40,40,40), c(25,125,125,125))
for (j in 1:4){
  ok <- Grade==j
  x <- c (treated.Pretest[ok], control.Pretest[ok])
  y <- c (treated.Posttest[ok], control.Posttest[ok])
  t <- rep (c(1,0), rep(sum(ok),2))
#  plot (x.range[j,], c(40,125), type="n", main=paste("grade",j), xaxs="i", yaxs="i",
  plot (c(0,125), c(0,125), type="n", main=paste("grade",j), xaxs="i", yaxs="i",
        xlab=expression(paste("pre-test, ",x[i])),
        ylab=expression(paste("post-test, ",y[i])),
        cex.axis=1.5, cex.lab=1.5, cex.main=1.8, mgp=c(2.5,.7,0))
  lm.1 <- lm (y ~ x + t)
  abline (lm.1$coef[1], lm.1$coef[2], lwd=.5, lty=2)
  abline (lm.1$coef[1]+lm.1$coef[3], lm.1$coef[2], lwd=.5)
  points (control.Pretest[ok], control.Posttest[ok], pch=20, cex=1.2)
  points (treated.Pretest[ok], treated.Posttest[ok], pch=21, cex=1.2)
}
dev.off()

postscript ("c:/books/multilevel/electricscatter1b.ps", horizontal=T, height=4)
par (mfrow=c(1,4), pty="s")
for (j in 1:4){
  ok <- Grade==j
  x <- c (treated.Pretest[ok], control.Pretest[ok])
  y <- c (treated.Posttest[ok], control.Posttest[ok])
  t <- rep (c(1,0), rep(sum(ok),2))
#  plot (x,y, type="n", main=paste("grade",j),
    plot (c(0,125),c(0,125), type="n", main=paste("grade",j),
        xlab=expression(paste("pre-test, ",x[i])),
        ylab=expression(paste("post-test, ",y[i])),
        cex.axis=1.5, cex.lab=1.5, cex.main=1.8, mgp=c(2.5,.7,0))
  lm.1 <- lm (y ~ x + t)
  abline (lm.1$coef[1], lm.1$coef[2], lwd=.5, lty=2)
  abline (lm.1$coef[1]+lm.1$coef[3], lm.1$coef[2], lwd=.5)
  points (control.Pretest[ok], control.Posttest[ok], pch=20, cex=1.2)
  points (treated.Pretest[ok], treated.Posttest[ok], pch=21, cex=1.2)
}
dev.off()

postscript ("c:/books/multilevel/electricscatter2.ps", horizontal=T, height=4)
par (mfrow=c(1,4), pty="s")
for (j in 1:4){
  ok <- Grade==j
  x <- c (treated.Pretest[ok], control.Pretest[ok])
  y <- c (treated.Posttest[ok], control.Posttest[ok])
  t <- rep (c(1,0), rep(sum(ok),2))
#  plot (x,y, type="n", main=paste("grade",j), xaxs="i", yaxs="i",
  plot (c(0,125),c(0,125), type="n", main=paste("grade",j), xaxs="i", yaxs="i",
        xlab=expression(paste("pre-test, ",x[i])),
        ylab=expression(paste("post-test, ",y[i])),
        cex.axis=1.5, cex.lab=1.5, cex.main=1.8, mgp=c(2.5,.7,0))
  lm.1 <- lm (y ~ x + t + x*t)
  abline (lm.1$coef[1], lm.1$coef[2], lwd=.5, lty=2)
  abline (lm.1$coef[1]+lm.1$coef[3], lm.1$coef[2]+lm.1$coef[4], lwd=.5)
  points (control.Pretest[ok], control.Posttest[ok], pch=20, cex=1.2)
  points (treated.Pretest[ok], treated.Posttest[ok], pch=21, cex=1.2)
}
dev.off()

attach.all (electric)
post.test <- c (treated.Posttest, control.Posttest)
pre.test <- c (treated.Pretest, control.Pretest)
grade <- rep (Grade, 2)
treatment <- rep (c(1,0), rep(length(treated.Posttest),2))
n <- length (post.test)

display (lm (post.test ~ treatment + pre.test + treatment:pre.test, subset=(grade==4)))

lm.4 <- lm(formula = post.test ~ treatment + pre.test + treatment * pre.test, subset = (grade==4))
n.sims <- 1000
sim.4 <- sim (lm.4, n.sims)
postscript ("c:/books/multilevel/grade4.interactions.ps", horizontal=T, height=3.8, width=5)
plot (0, 0, xlim=range (pre.test[grade==4]), ylim=c(-5,10),
       xlab="pre-test", ylab="treatment effect", main="treatment effect in grade 4")
abline (0, 0, lwd=.5, lty=2)
for (i in 1:20)
  curve (sim.4$beta[i,2] + sim.4$beta[i,4]*x, lwd=.5, col="gray", add=T)
curve (lm.4$coef[2] + lm.4$coef[4]*x, lwd=.5, add=T)
dev.off()

effect <- array (NA, c(n.sims, sum(grade==4)))
for (i in 1:n.sims)
  effect[i,] <- sim.4$beta[i,2] + sim.4$beta[i,4]*pre.test[grade==4]
mean.effect <- rowMeans(effect)

# plot regression results

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
regression.2tables (paste ("Grade", 1:4), est1, est2, se1, se2, "Regression on treatment indicator", "Regression on treatment indicator,\ncontrolling for pre-test", "c:/books/multilevel/electric.ests.ps")

# analyze replace/supplement

supp <- c(as.numeric(electric[,"Supplement."])-1, rep(NA,nrow(electric)))
# supp=0 for replace, 1 for supplement, NA for control

jitter.binary <- function (a, jitt=.05){
  a + (1-2*a)*runif(length(a), 0, jitt)
}

postscript ("c:/books/multilevel/electricsupp1.ps", horizontal=T, height=2.6)
par (mfrow=c(1,4))
for (k in 1:4){
  cat (paste("grade",k,":\n"))
  ok <- (grade==k)&(!is.na(supp))
  glm.supp <- glm (supp ~ pre.test, family=binomial(link="logit"), subset=ok)
  display(glm.supp)
  sims.glm.supp <- sim (glm.supp)
  plot (range(pre.test[ok]), c(0,1), type="n", xlab="Pre-test score",
        ylab="", main=paste("grade", k),
        cex.axis=1.5, cex.lab=1.5, cex.main=1.8, mgp=c(2.5,.7,0), yaxt="n")
  axis (2, c(0,1), c("     Replace","Supp    "), cex.axis=1.5, mgp=c(2.5,.7,0))
  for (l in 1:20)
      curve (invlogit (sims.glm.supp$beta[l,1] + sims.glm.supp$beta[l,2]*x), lwd=.5, col="gray", add=T)
  points (pre.test[supp==1&ok], jitter.binary(supp[supp==1&ok]), pch=21) #  supp:  open circle
  points (pre.test[supp==0&ok], jitter.binary(supp[supp==0&ok]), pch=20)# replace:  dot
  curve (invlogit (glm.supp$coef[1] + glm.supp$coef[2]*x), lwd=.5, add=T)
  lm.supp <- lm (post.test ~ supp + pre.test, subset=((grade==k)&!is.na(supp)))
  display(lm.supp)
  est1[k] <- lm.supp$coef[2]
  se1[k] <- summary(lm.supp)$coef[2,2]
}
dev.off()

regression.2tablesA (paste ("Grade", 1:4), est1, se1, "Estimated effect of supplement,\ncompared to replacement", "c:/books/multilevel/electricsupp2.ps")

# hierarchical model including pair indicators

pair <- rep (1:nrow(electric), 2)
grade.pair <- grade[1:nrow(electric)]
y <- post.test
n <- length(y)
n.grade <- max(grade)
n.pair <- max(pair)

data <- c("n", "y", "n.grade", "grade", "grade.pair", "treatment", "n.pair", "pair")
inits <- function(){
  list (mu.a=runif(n.grade), theta=runif(n.grade), a=runif(n.pair),
        sigma.y=runif(n.grade), sigma.a=runif(n.grade))}
params <- c("theta","a","mu.a","sigma.y","sigma.a")
paired.0 <- bugs (data, inits, params, "paired.0.new.bug", n.iter=5000, debug=TRUE)
postscript ("c:/books/multilevel/electric.mlm.output.ps", horizontal=TRUE, height=5, width=7)
plot (paired.0)
dev.off()

data <- c("n", "y", "pre.test", "n.grade", "grade", "grade.pair", "treatment", "n.pair", "pair")
inits <- function(){
  list (b.0=runif(n.grade), theta=runif(n.grade), a=runif(n.pair),
        sigma.y=runif(n.grade), sigma.a=runif(n.grade),
        b.pre.test=runif(n.grade))}
params <- c("b.0","theta","a","sigma.y","sigma.a","b.pre.test")
paired.1 <- bugs (data, inits, params, "paired.1.bug", n.iter=5000)

dum <- bugs (data, inits, params, "paired.0.new.bug", n.iter=500, n.burnin=0, debug=TRUE)
postscript ("c:/books/multilevel/gibbs.converge.poor.ps", height=6, horizontal=TRUE)
par (mar=c(5,5,3,2)+.1)
plot (c(0,200), c(0,26), xlab="iteration",
      ylab="", yaxt="n", xaxs="i", cex.lab=1.8, cex.axis=1.8, type="n")
axis (2, c(0,10,20), c(0,1,2), cex.axis=1.8)
mtext (expression(phi), 2, 3, cex=2) 
for (j in 1:3){
  lines (1:203, dum$sims.array[131:333,j,"sigma.a[1]"], lwd=.5)
}
dev.off()

est3 <- paired.0$mean$theta
se3 <- paired.0$sd$theta
est4 <- paired.1$mean$theta
se4 <- paired.1$sd$theta
regression.2tables (paste ("Grade", 1:4), est3, est4, se3, se4, "Regression on treatment indicator,\ncontrolling for pairs", "Regression on treatment indicator,\ncontrolling for pairs and pre-test", "c:/books/multilevel/electric.mlm.ps")

# comparing all the se's

postscript ("c:/books/multilevel/electric.se.ps", horizontal=T, height=2.2, width=10.5)
par (mfrow=c(1,4), mar=c(5,6.5,4,2)+.1)
ses <- cbind(se1,se2,se3,se4)
for (k in 1:4){
  ok <- (grade==k)&(!is.na(supp))
  se.grade <- ses[k,]
  plot (1:4, se.grade, xlim=c(.8,4.2), ylim=c(0,max(ses)*1.1), xlab="",
        ylab="s.e. of estimated\ntreatment effect", yaxs="i",
        main=paste("grade", k),
        cex.axis=1.5, cex.lab=1.5, cex.main=1.8, xaxt="n", pch=19)
  axis (1, 1:4, c("--\n","pre\n","pair\n","pre,\npair"), cex.axis=1.4,
        tck=0, mgp=c(3,1.8,0))
  abline (0, 0, lty=2, lwd=.5)
}
dev.off()

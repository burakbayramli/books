######################## © CSIRO Australia 2005 ###############################
# Session 04: Graphics: An Introduction                                       #
# Authors:    Petra Kuhnert & Bill Venables                                   #
#             CSIRO Mathematical and Information Sciences                     #
# Date:       28 November 2005                                                #
###############################################################################

##################
# High Level Graphics

 make.high <- function(){
 # Produces high level plots
 old.par <- par(mfrow=c(3,3))
 on.exit(par(old.par))

 # creating simulated data
 set.seed(12)
 x <- runif(100)
 y <- 4*x+rnorm(100)
 grp <- factor(rep(1:5,20))

 # dotchart
 dotchart(x[1:8],main="dotchart",labels=letters[1:8])

 # hist
 hist(y,density=10,main="hist")

 # barplot
 barplot(table(y,grp),angle = 15+10*1:5, density = 20, main="barplot")

 # boxplot
 boxplot(split(y,grp),names=paste("Grp ",1:5,sep=""),main="boxplot")

 # plot
 plot(x,y, main="plot")

 # qqnorm
 qqnorm(y,main="qqnorm")
 qqline(y)


 # data for contour, image and persp
 z <- 2 * volcano        # Exaggerate the relief
 x <- 10 * (1:nrow(z))   # 10 meter spacing (S to N)
 y <- 10 * (1:ncol(z))   # 10 meter spacing (E to W)

 # contour
 contour(x,y,z,main="contour")

 # image
 image(x,y,z,main="image")

 # persp
 persp(x, y, z, theta = 135, phi = 30, col = "green3", scale = FALSE,
           ltheta = -120, shade = 0.75, border = NA, box = FALSE)
 title(main="persp")
}

#########################
# Low Level Graphics

make.low <- function(){
 # Low level graphics functions

 old.par <- par(mfrow=c(3,3))
 on.exit(par(old.par))

 # set up data
 set.seed(12)
 x <- runif(20)
 y <- 4*x+rnorm(20)

 # points
 plot(x,y,type="n",main="points")
 points(x,y,pch=9)

 # symbols
 symbols(x,y,circles=abs(rnorm(20))/10,inches=F,main="symbols")

 # abline
 plot(x,y,main="abline")
 abline(lsfit(x,y))

 # arrows
 plot(1:11,1:11,type="n",main="arrows")
 arrows(1:10,rep(1,10),1:10,2:11)

 # box
 plot(x,y,type="n",xlab="",ylab="",axes=F,main="box")
 box()

 # lines
 plot(x,y,main="lines")
 lines(supsmu(x,y))

 # segments
 plot(1:11,1:11,type="n",main="segments")
 segments(1:10,rep(1,10),1:10,2:11)

 # text
 plot(x,y,type="n",main="text")
 text(x,y)

 # polygon
 plot(x,y,type="n",main="polygon")
 polygon(x[c(10,20,13,18,3)],y[c(10,20,13,18,3)],col="red",density=-1)

}

######################
# Distribution of MPG for Cars93 Dataset
attach(Cars93)
par(mfrow=c(2,2))
# Histogram
hist(MPG.highway,xlab="Miles per US Gallon",main="Histogram")
# Boxplot
boxplot(MPG.highway,main="Boxplot")
# Density
plot(density(MPG.highway),type="l",xlab="Miles per US Gallon",main="Density")
# Q-Q Plot
qqnorm(MPG.highway,main="Normal Q-Qplot")
qqline(MPG.highway)

######################
# Histograms
par(mfrow=c(1,2))
hist(MPG.highway,nclass=4,main="Specifying the Number of Classes")
hist(MPG.highway,breaks=seq(from=20,to=60,by=5),
  main="Specifying the Break Points")
par(mfrow=c(1,1))

######################
# Boxplots
boxplot(MPG.highway,ylab="MPG.highway")
summary(MPG.highway)

#####################
# Densities
par(mfrow=c(2,2))
plot(density(MPG.highway),type="l",main="Default Bandwidth")
plot(density(MPG.highway,bw=0.5),type="l",main="Bandwidth=0.5")
plot(density(MPG.highway,bw=1),type="l",main="Bandwidth=1")
plot(density(MPG.highway,bw=5),type="l",main="Bandwidth=5")
par(mfrow=c(1,1))
	
#####################
# Q-Q Plot
# Based on Normal distribution
qqnorm(MPG.highway,ylab="MPG.highway")
qqline(MPG.highway)

# Comparison agains other distributions
x <- rpois(1000,lambda=5)
par(mfrow=c(1,2),pty="s")
qqnorm(x,ylab="x")
qqline(x)
qqplot(qpois(seq(0,1,length=50),lambda=mean(x)),x,xlab="Theoretical Quantiles",ylab="x")
title(main="Poisson Q-Q Plot")
par(mfrow=c(1,1))

############################
# Comparing Groups
par(mfcol=c(2,2))
hist(MPG.highway,xlim=range(MPG.highway,MPG.city))
hist(MPG.city,xlim=range(MPG.highway,MPG.city))
boxplot(list(MPG.highway,MPG.city),names=c("Highway","City"),main="Miles per Gallon")
qqplot(MPG.highway,MPG.city, main="Q-Q Plot")
par(mfrow=c(1,1))
detach(Cars93)

#############################
# Time Series Objects
is.ts(ldeaths)
ldeaths
plot.ts(ldeaths,ylab="Monthly Deaths from Lung Disease (Males and Females)")
# plotting multiple time series
ts.plot(ldeaths,mdeaths,fdeaths,gpars=list(xlab="year", ylab="deaths",
  lty=c(1:3)))
legend(locator(1),c("Overall Deaths","Male Deaths","Female Deaths"),
  lty=1:3,bty="n")
# ACF functions
par(mfrow=c(3,1))
acf(ldeaths,type="covariance",main="Autocovariance Function")
acf(ldeaths,type="correlation",main="Autocorrelation Function")
acf(ldeaths,type="partial",main="Partial Autocorrelation Function")
par(mfrow=c(1,1))

#############################
# Displaying Bivariate Data
par(mfrow=c(4,2))
plot(ldeaths,type="p",main='pty="p"')
plot(ldeaths,type="l",main='pty="l"')
plot(ldeaths,type="b",main='pty="b"')
plot(ldeaths,type="o",main='pty="o"')
plot(ldeaths,type="s",main='pty="s"')
plot(ldeaths,type="h",main='pty="h"')
plot(ldeaths,type="n",main='pty="n"')
par(mfrow=c(1,1))
 
# Adding Points
attach(Cars93)
plot(MPG.highway,Price,type="n",xlim=range(MPG.highway,MPG.city),
		xlab="miles per gallon")
points(MPG.highway,Price,col="red",pch=16)
points(MPG.city,Price,col="blue",pch=16)
legend(locator(1),c("Highway","City"),col=c("red","blue"),pch=16,bty="n")
text(locator(),Manufacturer)

# Adding Text
plot(MPG.highway[1:10],Price[1:10],type="n", ylab="Price",
   	  xlim=range(MPG.highway[1:10],MPG.city[1:10]),xlab="miles per gallon")
points(MPG.highway[1:10],Price[1:10],	col="red",pch=16)
points(MPG.city[1:10],Price[1:10],col="blue",pch=16)
legend(locator(1),c("Highway","City"),col=c("red","blue"),pch=16,bty="n")
text(MPG.highway[1:10],Price[1:10],Manufacturer[1:10],cex=0.7,pos=2)

# Using the "identify" function
plot(MPG.highway[1:10],Price[1:10],type="n",ylab="Price",
   	  xlim=range(MPG.highway[1:10],MPG.city[1:10]),xlab="miles per gallon")
points(MPG.highway[1:10],Price[1:10],	col="red",pch=16)
points(MPG.city[1:10],Price[1:10],col="blue",pch=16)
legend(locator(1),c("Highway","City"),col=c("red","blue"),pch=16)
identify(c(MPG.city[1:10],MPG.highway[1:10]),rep(Price[1:10],2),
  rep(Manufacturer[1:10],2),pos=2)

# Adding Symbols
symbols(MPG.highway,Price, circles=EngineSize,xlab="miles per gallon",
  ylab="Price",inches=0.25,main="Area of Circle Proportional to Engine Size")
detach(Cars93)

# Adding Lines
with(Cars93, {
     plot(Weight,100/MPG.city,pch=16)
     lines(lowess(Weight,100/MPG.city),col="red")
     lines(lsfit(Weight,100/MPG.city),col="blue")
     abline(coef(lsfit(Weight,100/MPG.city)),col="blue")
     xy <- par("usr")[c(1,4)]
     legend(xy[1], xy[2],c("Lowess Smoother","Least Squares"),
	   col=c("red","blue"),lty=1,bty="n")
})


# Use of the "mtext" function
# Calculating a correlation matrix
cor(iris[,-5])
image(1:4,1:4,cor(iris[,-5]),col=gray(seq(from=100,to=0,length=100)/100),
  axes=F,xlab="",ylab="")
mtext(side=1,text=names(iris[,-5]),at=seq(from=1,to=4,length=4),line=1,cex=0.8)
mtext(side=2,text=names(iris[,-5]),at=seq(from=1,to=4,length=4),line=1,cex=0.8)
title(main="Image Plot of Correlations (Iris Data)")

##############################
# High-Level Plotting Routines

# pairs function
pairs(state.x77[,1:5],main = "Information from 50 States of America",pch = 16)
pairs(state.x77[,1:5],main = "Information from 50 States of America",pch = 16,
                           panel=panel.smooth)
                           
# Star Plot
stars(mtcars[, 1:7], key.loc = c(14, 1.8),main = "Motor Vehicle Performance",
               flip.labels=FALSE)

# Segments Plot
palette(rainbow(12, s = 0.6, v = 0.75))
stars(mtcars[, 1:7], key.loc = c(14, 1.8),main = "Motor Vehicle Performance",
               flip.labels=FALSE,draw.segments=TRUE)
               
#################################
# Overalying Figures
# add=T option
z <- 2 * volcano        # Exaggerate the relief
x <- 10 * (1:nrow(z))   # 10 meter spacing (S to N)
y <- 10 * (1:ncol(z))   # 10 meter spacing (E to W)
# image
 image(x,y,z,main="Mt Eden")
# contour
 contour(x,y,z,add=T)
 
# new=T option
x <- rnorm(1000,1,1)
y <- -1 + 4*x + rnorm(1000)
frame()
par(fig=c(0,1,0.4,1),mar=c(1,4,4,4))
plot(x,y,xlab="",ylab="y",pch=16,col="gray",axes=F)
abline(coef(lm(y~x)),col="red")
box()
axis(side=2)
par(new=T,fig=c(0,1,0,0.4),mar=c(5,4,0.5,4))
hist(x,xlab="x",main="",ylab="Frequency")










 

 




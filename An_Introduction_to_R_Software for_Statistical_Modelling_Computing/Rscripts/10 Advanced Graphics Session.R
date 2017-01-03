######################## © CSIRO Australia 2005 ###############################
# Session 10:  Advanced Graphics                                              #
# Authors:     Petra Kuhnert & Bill Venables                                  #
#              CSIRO Mathematical and Information Sciences                    #
# Date:        28 November 2005                                               #
###############################################################################


########################
# Whiteside data

# Preliminary Settings
graphics.off()
require(lattice)
require(MASS)
trellis.device()
trellis.par.set(theme=col.whitebg())

# Demonstrating Lattice Graphics
xyplot(Gas ~ Temp, whiteside, groups = Insul,panel = panel.superpose)
xyplot(Gas ~ Temp | Insul, whiteside,
	xlab = "External temperature",
	ylab = "Gas consumption",
	main = "Whiteside heating data", aspect = 0.6)


# Adding least square lines
xyplot(Gas ~ Temp | Insul, whiteside,
	xlab = "External temperature",
	ylab = "Gas consumption",
	main = "Whiteside heating data", aspect = 0.6,
  panel = function(x, y, ...) {
		panel.xyplot(x, y, ...)
		panel.lmline(x, y, ...,col="red")
	},
  ylim = c(0, 7))

# Delinking the scales
xyplot(Gas ~ Temp | Insul, whiteside,
	xlab = "External temperature",
	ylab = "Gas consumption",
	main = "Whiteside heating data", aspect = 0.6,
	panel = function(x, y, ...) {
		panel.xyplot(x, y, ...)
		panel.lmline(x, y, ...,col="red")
	},
  prepanel = function(x, y, ...) {
		list(xlim = range(x), ylim = range(0, y),
		dx = NULL, dy = NULL)
	},
  scales = list(relation = "free"))

###############################
# Changing trellis parameters

sps <- trellis.par.get("superpose.symbol")
sps
spl <- trellis.par.get("superpose.line")
spl
show.settings()
sps$pch <- c(16,18,4,3,1,2,7)
sps$col <- c("purple", "pink", "blue", "orange", "red","brown","black")
trellis.par.set("superpose.symbol", sps)
Rows(sps,1:3)
show.settings()

# Redo previous plot to show trellis changes
xyplot(Gas ~ Temp , whiteside,  groups=Insul,
	xlab = "External temperature",
	ylab = "Gas consumption",
	panel = panel.superpose,
	main = "Whiteside heating data", aspect = 0.6,
  ylim = c(0, 7))

############################
# Stormer Viscometer Data

# Plotting the data
require(MASS)
xyplot(Time ~ Viscosity, stormer, groups = Wt,
	panel = panel.superpose, type = "b",
	main = "Stormer viscometer calibration data")
	
# Adding a Key/Legend
xyplot(Time ~ Viscosity, stormer, groups = Wt,
	panel = panel.superpose, type = "b",
	main = "Stormer viscometer calibration data",
	key = list(columns = 3,
		text = list(paste(c("Weight:   ", "", ""),
    sort(unique(stormer$Wt)), "gms")),
		points = Rows(sps, 1:3),
		lines = Rows(spl, 1:3),
    lty=1:3)
	)
	
# Adding Fitted Values: Subscripts
dat <- data.frame(x = rep(1:20, 6),
			f = factor(rep(1:6, each = 20)))

dat <- transform(dat,y = 1 + (dat$x-10.5)^2 + rnorm(120, sd = 10))
fm <- lm(y ~ f + poly(x, 2), dat)
dat$fit <- fitted(fm)

xyplot(y ~ x | f, dat, subscripts = TRUE,
	as.table = TRUE,
	panel = function(x, y, subscripts, ...) {
		panel.xyplot(x, y, ...)
		llines(spline(x, dat$fit[subscripts]),
			col="orchid")
})

################################
# Output over several pages
par(ask = TRUE)
with(dat, for(i in unique(f)){
       plot(x[f==i],y[f==i],xlab="x",ylab="y")
       lines(spline(x[f==i],fit[f==i]),col="red")
       title(main=paste("Data group",i))
       bringToTop()
       })
par(ask = FALSE)

# Outputting over several pages (file)
trellis.device(win.metafile, width = 7, filename = "myPlots%02d.wmf")
xyplot(y ~ x | f, dat, subscripts = TRUE,
		as.table = TRUE,
		panel = function(x, y, subscripts, ...) {
			panel.xyplot(x, y, ...)
			llines(spline(x, dat$fit[subscripts]),
				col="orchid")
		},layout = c(2,1), aspect = "xy")
dev.off()

#######################
# Volcanos in New Zealand

# Using image()
data(volcano)
x <- 10*(1:nrow(volcano))
y <- 10*(1:ncol(volcano))
image(x, y, volcano,col = terrain.colors(100),axes = FALSE,xlab="",ylab="")
contour(x, y, volcano,levels = seq(90, 200, by=5),add = TRUE,col = "peru")
axis(1, at = seq(100, 800, by = 100))
axis(2, at = seq(100, 600, by = 100))
box()
title(main = "Maunga Whau Volcano",font.main = 4)

# Using lattice()
length(x)
length(y)
dim(volcano)
vdat <- expand.grid(x = x, y = y)
vdat$v <- as.vector(volcano)
levelplot(v ~ x*y, vdat, contour=T,main="levelplot()")
wireframe(v ~ x*y, vdat, drape=TRUE,main="wireframe():default")
wireframe(v ~ x*y, vdat, drape=TRUE,col.regions = rainbow(100),
  main="wireframe():rainbow")
wireframe(v ~ x*y,vdat,drape=TRUE,
          col.regions=topo.colors(100),main="wireframe():topo.colors")
          
##########################
# Color Palettes

# Demonstration of colour palettes (from R Help System)
 demo.pal <-
       function(n, border = if (n<32) "light gray" else NA,
            main = paste("color palettes;  n=",n),
            ch.col = c("rainbow(n, start=.7, end=.1)", "heat.colors(n)",
                       "terrain.colors(n)", "topo.colors(n)", "cm.colors(n)"))
     {
         nt <- length(ch.col)
         i <- 1:n; j <- n / nt; d <- j/6; dy <- 2*d
         plot(i,i+d, type="n", yaxt="n", ylab="", main=main)
         for (k in 1:nt) {
             rect(i-.5, (k-1)*j+ dy, i+.4, k*j,
                  col = eval(parse(text=ch.col[k])), border = border)
             text(2*j,  k * j +dy/4, ch.col[k])
         }
     }
     n <- if(.Device == "postscript") 64 else 16
          # Since for screen, larger n may give color allocation problem

demo.pal(n)

# Demonstration of grey scale colours
barplot(rep(1,20),col=gray((0:20)/20),main="Grey Scale Palette")

# User-Defined Colour Palette
# Producing an Image using the gplots package
# Alternative expression
# cols <- gplots::colorpanel(25,"green","yellow","red")
require(gplots)
cols1 <- colorpanel(25,"green","yellow","red")
fr <- cut(volcano,breaks=quantile(volcano,0:25/25),include=T)
# plot function
plot(row(volcano),col(volcano),pch=15,col=cols1[fr],cex=1.5,xlab="",ylab="")
title(main="plot() with user defined colours")
# image function
detach("package:gplots")
cols2 <- gplots::colorpanel(25,"brown","yellow","white")
image(volcano,col=cols2,main="image() with user defined colours")

##############################
# Mathematical Expressions

?plotmath
demo(plotmath)

x <- rnorm(5000)
b <- 1
y <- 1+b*x+rnorm(5000)
plot(x,y)
text(locator(1),expression(paste(y,"=",alpha^2+beta[2]^2*x)))
title(main=expression(paste("Coefficients: ",alpha,"=",1,",",beta[2],"=",1)))

plot(x,y)
title(main=substitute(paste("Coefficients: ",alpha,"=",1,",",
     beta[2],"=",b),list(b=b)))
     


########################
# Plotting Coastlines and Co-ordinate Systems
#
# Plotting South-East Coastline and Islands in Moreton Bay
# Reference:  Coastline Extractor:  http://rimmer.ngdc.noaa.gov/mgg/coast/getcoast.html

# Read in co-ordinates
MB.cl <- read.table("MB_coastline.txt")
names(MB.cl) <- c("Longitude","Latitude")
# Using plot()
plot(MB.cl$Long,MB.cl$Lat,type="l",xlab="Longitude",ylab="Latitude")
title(main="Moreton Bay (plot() function)")
# Using "eqscplot()"
eqscplot(MB.cl$Long,MB.cl$Lat,type="l",xlab="Longitude",ylab="Latitude")
title(main="Moreton Bay (eqscplot() function)")










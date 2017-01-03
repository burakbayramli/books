# Condition Factors for Rainbow Trout

# A sample of fish was captured by MOE using two different nets of standard 
# mesh sizes. We wish to compute several condition factors and see if males/
# females or maturity classes differ in their condition factor.

# 2015-07-22 CJS ggplot; ##---; split; lsmeans; etc

#C an W fishury be predicted from food W diets?

# Food W diets (e.g. number of fish, species of fish, etc) were 
# recorded for a sample of people. Based on estimates of fishury in the 
# food, the fishury in the diet was estimated.  
# A W sample was also taken from these people 
# and the W fishury level was also 
# measured.

# Lines starting in ##--part001b; and ##***part001e; are used
# to bracket portions of the R code for inclusion into my
# course notes and are not normally coded.

options(useFancyQuotes=FALSE) # renders summary output corrects

library(car)
library(ggplot2)
library(lsmeans)
source('http://www.stat.sfu.ca/~cschwarz/Stat-650/Notes/MyPrograms/schwarz.functions.r')

# Read in the data
sink('rainbow-condition-R-data.txt', split=TRUE)
##***partdatab;
fish <- read.csv("rainbow-condition.csv", header=TRUE, as.is=TRUE, strip.white=TRUE)
fish$K <- fish$Weight..g./(fish$Len..mm./100)**3
fish$SpeciesF  <- factor(fish$Species)
fish$MaturityF <- factor(fish$Maturity)
fish$SexF      <- factor(fish$Sex)
fish[1:5,]
##***partdatae;
sink()

######################################### Analysis of combined data ##########################

# histogram of the raw data
# Plot the raw data
##***parthistb;
histplot <- ggplot(data=fish, aes(x=K))+
  ggtitle("Histogram of condition factor K")+
  geom_histogram()
histplot
##***parthiste;
ggsave(plot=histplot, file='rainbow-condition-R-hist.png', h=4, w=6, units="in", dpi=300)



# Plot the derived data for fultons K
##***partprelimb;
fish$W <- fish$Weight..g.
fish$L <- (fish$Len..mm./100)**3
wplot <- ggplot(data=fish, aes(x=L, y=W))+
  ggtitle("W vs (L/100)**3")+xlab("(L/100)**3")+
  xlab("(L/100)**3")+
  geom_point()
wplot
##***partprelime;
ggsave(plot=wplot, file='rainbow-condition-R-prelim.png', h=4, w=6, units="in", dpi=300)



# Fit the no-intercept regression line and get the results
sink('rainbow-condition-R-lmfit.txt', split=TRUE)
##***partlmfitb;
k.fit <- lm(W ~ -1 +L, data=fish)
summary(k.fit)
##***partlmfite;
sink()
wplot2 <- wplot + 
  geom_abline(intercept=0, slope=coef(k.fit)[1])
wplot2
ggsave(plot=wplot2, file='rainbow-condition-R-lmfit.png', h=4, w=6, units="in", dpi=300)


# Extract the coefficients and find a confidence interval for them
sink('rainbow-condition-R-lmfitestimates.txt', split=TRUE)
##***partlmfitestimatesb;
summary(k.fit)$coefficients
confint(k.fit)
##***partlmfitestimatese;
sink()

# Make predictions at new values of X for the mean and individual
sink('rainbow-condition-R-005.txt', split=TRUE)
##***part005b;
my.data <- data.frame(L=c(20,30,40))

cat('Confidence intervals for the MEAN response\n')
my.pred.mean <- predict(k.fit, newdata=my.data,
    se.fit=TRUE, interval="confidence")
cbind(L=my.data$L,
    my.pred.mean$fit, 
    se=my.pred.mean$se.fit)
    
cat('Confidence intervals for the INDIVIDUAL response\n',
    'Note that se for predictions are NOT computed\n')
my.pred.indiv <- predict(k.fit, newdata=my.data,
    interval="prediction")
cbind(L=my.data$L,
    my.pred.indiv)

##***part005e;
sink()


# draw a plot with the fitted lines and the various confidence intervals
png('rainbow-condition-R-006.png')
##***part006b;
my.pred.mean  <- predict(k.fit, newdata=fish, interval='confidence')
colnames(my.pred.mean) [c(2,3)]<- c("lcl.mean",'ucl.mean')
my.pred.indiv <- predict(k.fit, newdata=fish, interval='prediction')
colnames(my.pred.indiv)[c(2,3)]<- c("lcl.indiv",'ucl.indiv')

fish <- cbind(fish, my.pred.mean, my.pred.indiv[,2:3] )
wplot3 <- ggplot(data=fish, aes(x=L, y=W))+
  ggtitle("W vs (L/100)**3")+xlab("(L/100)**3")+
  geom_point()+
  geom_abline(intercept=0, slope=coef(k.fit)[1])+
  geom_ribbon(aes(ymin=lcl.mean, ymax=ucl.mean),   alpha=.8)+
  geom_ribbon(aes(ymin=lcl.indiv, ymax=ucl.indiv), alpha=0.2)
wplot3
##***part006e;
ggsave(plot=wplot3, file='rainbow-condition-R-006.png', h=4, w=6, units="in", dpi=300)


# create a 2x2 plot of residual and other plots
png('rainbow-condition-R-diagplot.png')
##***partdiagplotb;
diagplot <- sf.autoplot.lm(k.fit)
grid.newpage()
grid.draw(diagplot)
##***partdiagplote;
dev.off()

grid.newpage()
grid.draw(diagplot)




######################################### Comparing condition factors among maturity classes ##########################

xtabs(~Maturity, data=fish)


# Fit the no-intercept regression line for each maturity class
# Fit the interaction term last.    
sink('rainbow-condition-R-matfit.txt', split=TRUE)
##***partmatfitb;
mat.fit <- lm(W ~ -1 + L + L:MaturityF, data=fish)
summary(mat.fit)
##***partmatfite;
sink()



sink('rainbow-condition-R-matfitanova.txt', split=TRUE)
##***partmatfitanovab;
anova(mat.fit)
##***partmatfitanovae;
sink()

sink('rainbow-condition-R-matfitestimates.txt', split=TRUE)
##***partmatfitestb;
# Estimate the individual slopes
mat.fit.lsmo <- lsmeans::lstrends(mat.fit, ~MaturityF, var="L")
summary(mat.fit.lsmo, infer=TRUE)
summary(pairs(mat.fit.lsmo), infer=TRUE)
##***partmatfiteste;
sink()

##***partmatfitplotb;
# Create a plot of the separate slopes
wplotmat <- ggplot(data=fish, aes(x=L, y=W, group=MaturityF, color=MaturityF, shape=MaturityF))+
  ggtitle("W vs (L/100)**3 by maturity class")+xlab("(L/100)**3")+
  geom_point()+
  geom_smooth(method="lm", formula=y~ 0 + x)
wplotmat
##***partmatfitplote;
ggsave(plot=wplotmat, file='rainbow-condition-R-matfitplot.png', h=4, w=6, units="in", dpi=300)


# create a 2x2 plot of residual and other plots
png('rainbow-condition-R-matdiagplot.png')
##***partdiagplotb;
matdiagplot <- sf.autoplot.lm(mat.fit)
grid.newpage()
grid.draw(matdiagplot)
##***partdiagplote;
dev.off()

grid.newpage()
grid.draw(matdiagplot)





######################################### Comparing condition factors among sex classes ##########################

xtabs(~Sex, data=fish)


# Fit the no-intercept regression line for each sexurity class
# Fit the interaction term last.    
sink('rainbow-condition-R-sexfit.txt', split=TRUE)
##***partsexfitb;
sex.fit <- lm(W ~ -1 + L + L:SexF, data=fish)
summary(sex.fit)
anova(sex.fit)
##***partsexfite;
sink()



sink('rainbow-condition-R-sexfitanova.txt', split=TRUE)
##***partsexfitanovab;
anova(sex.fit)
##***partsexfitanovae;
sink()

sink('rainbow-condition-R-sexfitestimates.txt', split=TRUE)
##***partsexfitestb;
# Estisexe the individual slopes
sex.fit.lsmo <- lsmeans::lstrends(sex.fit, ~SexF, var="L")
summary(sex.fit.lsmo, infer=TRUE)
cld(sex.fit.lsmo)
summary(pairs(sex.fit.lsmo), infer=TRUE)
##***partsexfiteste;
sink()

##***partsexfitplotb;
# Create a plot of the separate slopes
wplotsex <- ggplot(data=fish, aes(x=L, y=W, group=SexF, color=SexF, shape=SexF))+
  ggtitle("W vs (L/100)**3 by Sex class")+xlab("(L/100)**3")+
  geom_point()+
  geom_smooth(method="lm", formula=y~ 0 + x)
wplotsex
##***partsexfitplote;
ggsave(plot=wplotsex, file='rainbow-condition-R-sexfitplot.png', h=4, w=6, units="in", dpi=300)


# create a 2x2 plot of residual and other plots
png('rainbow-condition-R-sexdiagplot.png')
##***partdiagplotb;
sexdiagplot <- sf.autoplot.lm(sex.fit)
grid.newpage()
grid.draw(sexdiagplot)
##***partdiagplote;
dev.off()

grid.newpage()
grid.draw(sexdiagplot)



######################################### Lack of fit test using pure error ##########################
# Because of replicate L values, you can do a formal test for lack of fit

sink('rainbow-condition-R-sexlackfit.txt', split=TRUE)
##***partsexlackfitb;
pure.error.fit <- lm( W ~ interaction(fish$L, fish$Sex), data=fish)
anova(sex.fit, pure.error.fit)
##***partsexlackfite;
sink()










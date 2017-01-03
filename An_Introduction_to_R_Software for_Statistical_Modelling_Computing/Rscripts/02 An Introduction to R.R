######################## © CSIRO Australia 2005 ###############################
# Session 02:  An Introduction to R and the Tinn-R Editor                     #
# Authors:     Petra Kuhnert & Bill Venables                                  #
#              CSIRO Mathematical and Information Sciences                    #
# Date:        28 November 2005                                               #
###############################################################################
 
#######################
# Customisation
# .First function
 .First <- function() {
		require(MASS)
		require(lattice)
		options(length=99999)
		loadhistory()
  	}
  	
# .Last function
.Last <- function() savehistory("My.Rhistory")

#   Rprofile.site
q <- function(save="yes",status = 0, runLast = TRUE)
.Internal(quit(save, status, runLast))

# Things you might want to change
# options(width=80,papersize="a4",editor="notepad")
# options(pager="internal")

# to prefer Compiled HTML help
# options(chmhelp=TRUE)

# to prefer HTML help
# options(html=TRUE)

# to prefer Windows help
# options(winhelp=TRUE)

options(show.signif.stars=FALSE,length=999999)
options("CRAN=http://cran.au.r-project.org/")
options("repos = c(CRAN = getOption("CRAN"),
                   CRANextra = "http://www.stats.ox.ac.uk/pub/RWin"))
                   
# Put working directory in the top border of the R console window
utils:::setWindowTitle(paste("=",getwd()))

####################
# Examples of the use of Distributions
# Generating data from a Normal distribution
norm.vals1 <- rnorm(n=10)
norm.vals2 <- rnorm(n=100)
norm.vals3 <- rnorm(n=1000)
norm.vals4 <- rnorm(n=10000)
# set up plotting region
par(mfrow=c(2,2))
hist(norm.vals1,main="10 RVs")
hist(norm.vals2,main="100 RVs")
hist(norm.vals3,main="1000 RVs")
hist(norm.vals4,main="10000 RVs")

### generating a mixture:
compMix <- ifelse(runif(5000) < 0.25, rnorm(5000, 3, 0.5), rnorm(5000))
hist(compMix,freq=F)
lines(density(compMix,bw=0.4),col="red")


# Use of distributions
# 2-tailed p-value for Normal distribution
1-pnorm(1.96)
qnorm(0.975)
# 2-tailed p-value for t distribution
2*pt(-2.43,df=13)
qt(0.025,df=13)
# p-value from chi-squared distribution with 1 degree of freedom
1-pchisq(5.1,1)
qchisq(0.975,1)






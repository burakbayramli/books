#  The importance of checking residuals.
#  2014-05-24 CJS First Edition

#   This dataset is extracted from:
#
#     Leonard A. Stefanski, (2007)
#     Residual Sur(Realism)'
#     American Statistician,  61, 163-177.

#   Also check out the website at:
#     http://www4.stat.ncsu.edu/~stefanski/NSF_Supported/Hidden_Images/stat_res_plots.html 

options(useFancyQuotes=FALSE) # renders summary output corrects

library(GGally)
library(ggplot2)

satan <- read.csv("residualcheck.csv",header=TRUE, as.is=TRUE, strip.white=TRUE)

head(satan)

# Casement plot (all possible pairs)
ggpairs(satan)

# Same in Base R
pairs(satan)


# DO the multiple regression
satan.fit <- lm(Y ~ x1 + x2 + x3 + x4 + x5 + x6, data=satan)
summary(satan.fit) # loot at the individual estimates

# Look at the residual plots
satan.fit.f <- fortify(satan.fit)
ggplot(data=satan.fit.f, aes(x=.fitted, y=.resid))+
  ggtitle("The importance of checking residuals")+
  geom_point()


# same plot in base R graphics
plot(fitted(satan.fit), resid(satan.fit),
     main='The importance of checking residuals')

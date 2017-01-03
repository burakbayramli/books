# Intuitive explanation of ANOVA
# 2015-04-17 CSJ remove doBy; misc schanges
# 2014-04-19 CJS ggplot, lsmeans, etc

# This data are used to show the intuitive idea behind ANOVA in
# testing for the equality of means.

#
# Lines starting with ##***part001b; or ##***part001e; bracket the source 
# line for inclusion by LaTex and usually are not coded.
#

options(useFancyQuotes=FALSE) # renders summary output corrects


library(ggplot2)
library(gridExtra)
library(lsmeans)
library(plyr)

#source('../../schwarz.functions.r') # in case no internet is avaialble
source('http://www.stat.sfu.ca/~cschwarz/Stat-650/Notes/MyPrograms/schwarz.functions.r')

options(width=200)


# Read in the data
sink('anova-example-R-000.txt', split=TRUE)
##***part001b;
ex.data <- read.csv('anova-example.csv', header=TRUE)
ex.data[1:12,]
##***part001e;
sink()


##***part002b;
# Side-by-side dot and boxplots 
plotprelim <- ggplot(data=ex.data, aes(x=method, y=y))+
  ggtitle("Responds of two experiments")+
  xlab("method")+ylab("Response")+
  geom_jitter(size=4, position=position_jitter(width=0.2, height=0.2))
plotprelim <- plotprelim + facet_grid(. ~ Experiment)
plotprelim
##***part002e;

ggsave(plot=plotprelim, file='example-R-prelim.png', h=6, w=6, units="in", dpi=300)



##***part003b;
# Compute some summary statistics for each method
sink('anova-example-R-002.txt', split=TRUE)
report <- ddply(ex.data, c("Experiment","method"), sf.simple.summary, variable="y", crd=TRUE)
report
##***part003e;
sink()



# Do the analysis
sink('anova-example-R-003.txt', split=TRUE)
result <- dlply(ex.data, "Experiment", function(x){
  res <- lm(y ~method, data=x)
  res
})
l_ply(names(result), function(x,result){
  cat("\n\n***** Results for ",x," *****\n\n")
  print(anova(result[[x]]))
}, result=result)
sink()
# Cuckoo eggs in various host species.
# 2015-04-18 CJS misc changes; remove doby; 
# 2014-04-19 CJS ggplot, lsmeans, ddply etc

# Cuckoo birds lay their eggs in nests of other species. 
# Do they "specialize"?
#
# Cuckoo eggs from various species nests were measured 
# (length in mm). Does the mean egg size differ among the 
# different host species? 


#
# Lines starting with ##***part001b; or ##***part001e; bracket the source 
# line for inclusion by LaTex and usually are not coded.
#

options(useFancyQuotes=FALSE) # renders summary output corrects


library(ggplot2)
library(gridExtra)
library(lsmeans)
library(plyr)

#source('../../schwarz.functions.r') # in case no internet
source('http://www.stat.sfu.ca/~cschwarz/Stat-650/Notes/MyPrograms/schwarz.functions.r')

options(width=200)

# Read in the data. The columns need to be stacked 
# and the missing value removed.
##***part001b;
temp <- read.csv('cuckoo.csv', header=TRUE, 
                 as.is=TRUE, strip.white=TRUE )
eggs <- stack(temp)
colnames(eggs)<- c("eggsize","HostSpecies")
eggs <- eggs[ !is.na(eggs$eggsize),]
eggs$HostSpecies <- factor(eggs$HostSpecies)
##***part001e;

# check the structure of the eggs dataframe
str(eggs)

sink('cuckoo-R-000.txt', split=TRUE)
eggs[1:12,]
sink()

# Get side-by-side dot plots
##***partprelimplotb;
# Side-by-side dot and boxplots 
plotprelim <- ggplot(eggs, aes(x=HostSpecies, y=eggsize))+
     geom_boxplot(notch=TRUE, alpha=0.2, outlier.shape=NA)+
     geom_jitter(size=3,position=position_jitter(width=0.2, height=0.1))+
     xlab("Host Species\n Point jittered to prevent overplotting")+
     ylab("Eggsize (mm)")+
     ggtitle("Eggsize data with overlaid boxplots")
plotprelim
##***partprelimplote;

ggsave(plotprelim, file="cuckoo-R-prelim.png", h=4,w=6, units="in", dpi=300)


# Compute some summary statistics for each group
sink('cuckoo-R-003.txt', split=TRUE)
##***part003b;
report <- ddply(eggs, "HostSpecies", summarize,
                n.eggs     = length(eggsize),
                mean.eggsize= mean(eggsize),
                sd.eggsize  = sd(eggsize))
report

report <- ddply(eggs, "HostSpecies", sf.simple.summary, variable="eggsize", crd=TRUE)
report
##***part003e;
sink()



# fit the linear model and get the ANOVA table and test for effects
sink('cuckoo-R-005.txt', split=TRUE)
##***part005b;
result <- lm(eggsize ~ HostSpecies, data=eggs)
anova(result)
##***part005e;
sink()

##***partdiagb;
# Check the assumptions of the ANOVA model using autoplot and fortity
# as defined in my schwarz.functions
plotdiag <-sf.autoplot.lm(result, which=c(1:3,5), mfrow=c(2,2))
plotdiag
##***partdiage;

ggsave(plot=plotdiag, file='cuckoo-R-diag.png', h=4, w=6, units="in", dpi=300)







sink('cuckoo-R-lsmeansreport.txt', split=TRUE)
##***partlsmeansobjb;
# Create the lsmeans object that is used in subsequent computations and
# obtain basic estimates of the marginal means (not adjusted for simultaneous
# coverage)
result.lsmo <- lsmeans::lsmeans(result, ~HostSpecies, adjust='tukey')
cat("Marginal means (not adjusted for simultaneous coverage)\n\n")
summary(result.lsmo, infer=TRUE)
##***partlsmeansobje;
sink()

cat("Marginal means (adjusted for simultaneous coverage)\n\n")
summary(result.lsmo, infer=TRUE, adjust="tukey")


sink('cuckoo-R-cldreport.txt', split=TRUE)
##***partcldreportb;
# Get the compact letter display and a plot
result.cld <- cld(result.lsmo)
result.cld
##***partcldreporte;
sink()

##***partcldplotsb;
# Make a bar plot of the cld display
plotcld <- sf.cld.plot.bar(result.cld, variable="HostSpecies")
plotcld <- plotcld + 
        xlab("HostSpecies")+
        ylab("Mean eggsize (with 95% ci")+
        ggtitle("Comparison of mean eggsize with cld")
plotcld

# Make a line graph of the cld display
plotcldb <- sf.cld.plot.line(result.cld, variable="HostSpecies")#, ciwidth=0.1)
plotcldb <- plotcldb + 
        xlab("HostSpecies")+
        ylab("Mean eggsize (with 95% ci")+
        ggtitle("Comparison of mean eggsize with cld")
plotcldb
##***partcldplotse;

ggsave(plot=plotcld, file='cuckoo-R-cldbar.png',  h=4, w=6, units="in", dpi=300)
ggsave(plot=plotcldb, file='cuckoo-R-cldline.png',h=4, w=6, units="in", dpi=300)


# Find all the pairwise differences WITHOUT adjusting for multiple comparisons
result.pairs <- pairs(result.lsmo, adjust='none')
summary(result.pairs, infer=TRUE)

sink('cuckoo-R-pairsreport.txt', split=TRUE)
##***partpairsb;
# Find all the pairwise differences adjusting for multipicity
result.pairs <- pairs(result.lsmo, adjust='tukey')
summary(result.pairs, infer=TRUE)
##***partpairse;
sink()

##***partpairsplotb;
# Make a plot of the differences
result.pairs.ci <- confint(result.pairs) # extract the ci values
result.pairs.ci
plotdiff <- ggplot(result.pairs.ci, aes(contrast, estimate, ymin = lower.CL, ymax = upper.CL)) +
    geom_point(size=4)+
    geom_linerange(size=1.5)+
    geom_abline(interecept=0, slope=0, linetype=2)+
    ylab("Estimated diff and 95% ci")+
    xlab("Contrast")+
    ggtitle("Estimated pairwise differences and ci")
plotdiff
##***partpairsplote;

ggsave(plot=plotdiff, file='cuckoo-R-pairdiff.png',h=4, w=6, units="in", dpi=300)




# Now to estimate a specific contast. This can be done using the contrast()
# in the lsmeans package
sink('cuckoo-R-010.txt',split=TRUE)
##***part010b;
cat('Ordering of levels in Host Species\n')
levels(eggs$HostSpecies)
cat('Set up my contrast\n')
my.contrast <- list(c(0,.5,-1,.5,0,0))
my.contrast
contrast.res <- contrast(result.lsmo, my.contrast)
summary(contrast.res)
confint(contrast.res)
##***part010e;
sink()


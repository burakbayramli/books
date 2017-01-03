# Fisher's Exact Test  - Northern Australlian Quoll

# In 1935, the highly toxic cane toad was introduced to Australia to aid with 
# pest control of scarab beetles. The beetles were wreaking havoc on 
# sugarcane crops. Unfortunately, this decision led to an unforeseen 
# and devastating effect on Australia's wildlife due to animal's consuming 
# the toxic toad. Damage has included the possible extinction of the 
# Northern Australian quoll. Although initiatives such as relocating the 
# quoll on the nearby islands have been taken in order to save the species, 
# there is no guarantee the new habitats will remain toadless. 
# Scientists have developed a new plan of attack using conditioned taste 
# aversion ({\bf CTA}) in order to save the quoll population.

# A sample of $62$ quolls was taken ($32$ males and $30$ females). 
# The quolls were then split up into two treatment groups;  
# toad smart (quolls that were given the CTA treatment, $15$ males and $16$ 
# females) and toad naive (Control group, $17$ males and $14$ females). 
# A samples of $34$ quolls ($21$ males, $13$ females) were subjected 
# to a prescreening trial (prior to release into the wild). 

# The trial proceeded as follows: Both toad naive and toad smart quolls 
# were subjected to a live cane toad in a controlled environment.  
# The quolls were monitored using hidden cameras and their response 
# was recorded. The response variable had three levels; 
#    attack (attacked the toad), 
#    reject(sniffed but did not pursue) and 
#    ignore. 

# 2014-12-20 CJS ggplot; as.is=TRUE; split=TRUE

# load required libraries
library(ggplot2)
library(plyr)

options(useFancyQuotes=FALSE) # renders summary output corrects

sink("quolls-R-001.txt", split=TRUE)
##***part001b;
quolls <-read.csv('quolls.csv', header=TRUE, as.is=TRUE, strip.white=TRUE)
quolls$Sex     <- factor(quolls$Sex)
quolls$Trt     <- factor(quolls$Trt)
quolls$Response<- factor(quolls$Response)
# remove all the 0 counts because the simple chi-square test gets annoyed
quolls <- quolls[quolls$Count>0,]
cat("Northern Quolls\n")
print(quolls)
cat("\n\n")
##***part001e;
sink()


# Show the contingency table
sink('quolls-R-005.txt', split=TRUE)
##***part002b;
xtabs(Count ~ Trt + Response + Sex, data=quolls)
##***part002e;
sink()


# Large sample test for each sex separately
sink("quolls-R-010.txt", split=TRUE)
##***part010b;
my.chisq.test <- dlply(quolls, "Sex", function(x){
	  temp <- xtabs(Count ~ Trt+Response, data=x, drop.unused.levels=TRUE)
	  chisq.test(temp, correct=FALSE)
    })
	  
cat("Results from traditional chi-square test\n")
print(my.chisq.test)
cat("Expected Count\n\n")
#l_ply(my.chisq.test, function(x){print(x$expected)})
##***part010e;
sink()


# Fisher's Exact test for each sex
sink("quolls-R-020.txt", split=TRUE)
##***part020b;
my.fisher.test <- dlply(quolls, "Sex", function(x){
	  temp <- xtabs(Count ~ Trt+Response, data=x, drop.unused.levels=TRUE)
	  fisher.test(temp)
    })
	  
cat("Results from Fisher's Exact test\n")
print(my.fisher.test)
##***part020e;
sink()



#----------------------------------------------------------

# Derive the exact test results.
# We will do the male and females separate as the tables are different sizes

# Start with the F data as it is 2 x 2
# First find all the relevant tables. This is tricky because the row and column sums are fixed.
# Traditionally, look at the n11 cell fr 2 x 2 tables
# Look at Agresti (2002), p. 91

Fquolls <- xtabs(Count ~ Trt + Response, data=subset(quolls, quolls$Sex=="F"), drop.unused.levels=TRUE)
Fquolls

n11 <- max(0, sum(Fquolls[1,])+sum(Fquolls[,1])-sum(Fquolls)):min(sum(Fquolls[1,]),sum(Fquolls[,1]))

# Now generate all the possible tables
n12 <- sum(Fquolls[1,])-n11
n21 <- sum(Fquolls[,1])-n11
n22 <- sum(Fquolls[,2])-n12

all.tables <- cbind(n11,n12,n21,n22)

# find the probabilities of each table. 

p.all.tables <- dhyper( n11, n11+n12, n21+n22, n11+n21)

cat("\n\n Listing of all possible tables and their probability \n\n")
print(cbind(all.tables,p.all.tables))


# find the probability of my table

p.obs.table <- dhyper(Fquolls[1,1], sum(Fquolls[1,]), sum(Fquolls[2,]), sum(Fquolls[,1]))
              
# find the various p-values

cat("Probability of the observed table: ", p.obs.table, "\n\n")

cat("One sided p-value for less than alteratives: ", sum(p.all.tables[n11<=Fquolls[1,1]]),"\n\n")

cat("One sided p-value for greater than alteratives: ", sum(p.all.tables[n11>=Fquolls[1,1]]),"\n\n")

# there are several ways to compute two-sided p-values. See Agresti (2002), p. 93
# One popular way is to sum all tables which are more unusual

cat("Two sided p-value for not equal alteratives: ", sum(p.all.tables[p.obs.table >= p.all.tables]),"\n\n")


#----------------------------------------------------------
# Now for the M data which is 2 x 3
Mquolls <- xtabs(Count ~ Trt + Response, data=subset(quolls, quolls$Sex=="M"), drop.unused.levels=TRUE)
Mquolls


# There are efficient algorithms for enumerating all the possible tables with the marginal totals fixed
# but we will proceed by brute force.
# Notice that the Avoid columns only has 2 possibilities. So we can list the tables
# for Attack/Reject with Avoid/Naive=1
n11 <- max(0, (sum(Mquolls[1,]-1)+(sum(Mquolls[,1])-(sum(Mquolls)-1)))):min(sum(Mquolls[1,])-1,sum(Mquolls[,1]))

# Now generate all the possible tables
n12 <- 1
n13 <- sum(Mquolls[1,])-n11-1
n21 <- sum(Mquolls[,1])-n11
n22 <- 0
n23 <- sum(Mquolls[,3])-n13

all.tables.1 <- cbind(n11,n12,n13,n21,n22,n23)
all.tables.1

# Now for the tables with Avpid/Naive =0
all.tables.2 <- all.tables.1
all.tables.2[,2] <- 0
all.tables.2[,5] <- 1
all.tables.2[,6] <- all.tables.2[,6] -1
all.tables.2[,3] <- all.tables.2[,3] +1

all.tables <- rbind(all.tables.1, all.tables.2)
all.tables
# find the probabilities of each table. This is brute force and subject
# to huge rounding error (!) and is not the way it usually is done in practise

p.all.tables <- prod(factorial(apply(Mquolls,1,sum)))*prod(factorial(apply(Mquolls,2,sum)))/
                factorial(sum(Mquolls))/apply(factorial(all.tables),1,prod)
sum(p.all.tables)

cat("\n\n Listing of all possible tables and their probability \n\n")
print(cbind(all.tables,p.all.tables))


# find the probability of my table

p.obs.table <- prod(factorial(apply(Mquolls,1,sum)))*prod(factorial(apply(Mquolls,2,sum)))/
                factorial(sum(Mquolls))/prod(factorial(Mquolls))
              
# find the various p-values

cat("Probability of the observed table: ", p.obs.table, "\n\n")

# One or two sided alternatives don't really make much sense in the case of 2 x 3 table.

# there are several ways to compute two-sided p-values. See Agresti (2002), p. 93
# One popular way is to sum all tables which are more unusual

cat("Two sided p-value for not equal alteratives: ", sum(p.all.tables[p.obs.table >= p.all.tables]),"\n\n")




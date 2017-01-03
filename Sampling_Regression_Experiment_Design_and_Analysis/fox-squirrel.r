# Demonstration of Fisher's Exact Test for the fox-squirrel example.

# 2014-12-20 CJS split=TRUE; 
# The comments starting ##***part0001b and ##***part0001e; etc are used to put the code into latex files;

options(useFancyQuotes=FALSE) # renders summary output corrects


sink("fox-squirrel-R-001.txt", split=TRUE)
##***part001b;
my.Squirrel <-matrix(c(3, 12, 0, 15),
       nr = 2,
       dimnames = list( Outcome = c("Predated", "Not-predated"),
                        Sex = c("F", "M")))

cat("Fox Squirrel data\n")
print(my.Squirrel)
##***part001e;
sink()

#  Do the large sample chi-square test
sink("fox-squirrel-R-002.txt", split=TRUE)
##***part002b;
my.chisq.test <- chisq.test(my.Squirrel, correct=FALSE)
cat("Results from traditional chi-square test\n")
print(my.chisq.test)
my.chisq.test$observed
my.chisq.test$expected
##***part002e;
sink()


# Now for the small sample test

sink("fox-squirrel-R-010.txt", split=TRUE)
##***part010b;
my.fisher.test <- fisher.test(my.Squirrel, alternative = "greater")
cat("Results from Fisher's Exact Test - greater alternative\n")
print(my.fisher.test)
##***part010e;
sink()

sink("fox-squirrel-R-011.txt", split=TRUE)
##***part011b;
my.fisher.test <- fisher.test(my.Squirrel, alternative = "two.sided")
cat("Results from Fisher's Exact Test - greater alternative\n")
print(my.fisher.test)
##***part011e;
sink()



# Derive the exact test results
# First find all the relevant tables. This is tricky because the row and column sums are fixed.
# Traditionally, look at the n11 cell
# Look at Agresti (2002), p. 91

n11 <- max(0, sum(my.Squirrel[1,])+sum(my.Squirrel[,1])-sum(my.Squirrel)):min(sum(my.Squirrel[1,]),sum(my.Squirrel[,1]))

# Now generate all the possible tables
n12 <- sum(my.Squirrel[1,])-n11
n21 <- sum(my.Squirrel[,1])-n11
n22 <- sum(my.Squirrel[,2])-n12

all.tables <- cbind(n11,n12,n21,n22)

# find the probabilities of each table. 

p.all.tables <- dhyper( n11, n11+n12, n21+n22, n11+n21)

cat("\n\n Listing of all possible tables and their probability \n\n")
print(cbind(n11,p.all.tables))


# find the probability of my table

p.obs.table <- dhyper(my.Squirrel[1,1], sum(my.Squirrel[1,]), sum(my.Squirrel[2,]), sum(my.Squirrel[,1]))
              
# find the various p-values

cat("Probability of the observed table: ", p.obs.table, "\n\n")

cat("One sided p-value for less than alteratives: ", sum(p.all.tables[n11<=my.Squirrel[1,1]]),"\n\n")

cat("One sided p-value for greater than alteratives: ", sum(p.all.tables[n11>=my.Squirrel[1,1]]),"\n\n")

# there are several ways to compute two-sided p-values. See Agresti (2002), p. 93
# One popular way is to sum all tables which are more unusual

cat("Two sided p-value for not equal alteratives: ", sum(p.all.tables[p.obs.table >= p.all.tables]),"\n\n")


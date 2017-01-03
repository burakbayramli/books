# Fisher's exact test
# Agresti (2002), p. 72, Table 3.1. Relationship between Aspirin Use and MI

# 2014-12-20 CJS split=TRUE

# Lines starting in ##***part001b; and ##***part001e; are used
# to bracket portions of the R code for inclusion into my
# course notes and are not normally coded.

options(useFancyQuotes=FALSE) # renders summary output corrects

sink("aspirin-R-001.txt", split=TRUE)
##***part001b;
my.Aspirin <-matrix(c(18,658, 28, 656),
       nr = 2, byrow=TRUE,
       dimnames = list(Aspirin = c("Yes", "No"),
                       MI      = c("Yes", "No")))
cat("Swedish Aspirin Study\n")
print(my.Aspirin)
cat("\n\n")
##***part001e;
sink()



# Large sample test
sink("aspirin-R-002.txt",  split=TRUE)
##***part002b;
my.chisq.test <- chisq.test(my.Aspirin, correct=FALSE)
cat("Results from traditional chi-square test\n")
print(my.chisq.test)
my.chisq.test$observed
my.chisq.test$expected
##***part002e;
sink()


# Small sample test
sink("aspirin-R-010.txt", split=TRUE)
##***part010b;
my.fisher.test <- fisher.test(my.Aspirin, alternative = "less")

cat("Results from Fisher's Exact Test\n")
print(my.fisher.test)
##***part010e;
sink()


# Derive the exact test results
# First find all the relevant tables. This is tricky because the row and column sums are fixed.
# Traditionally, look at the n11 cell
# Look at Agresti (2002), p. 91

n11 <- max(0, sum(my.Aspirin[1,])+sum(my.Aspirin[,1])-sum(my.Aspirin)):min(sum(my.Aspirin[1,]),sum(my.Aspirin[,1]))

# Now generate all the possible tables
n12 <- sum(my.Aspirin[1,])-n11
n21 <- sum(my.Aspirin[,1])-n11
n22 <- sum(my.Aspirin[,2])-n12

all.tables <- cbind(n11,n12,n21,n22)

# find the probabilities of each table. 

p.all.tables <- dhyper( n11, n11+n12, n21+n22, n11+n21)

cat("\n\n Listing of all possible tables and their probability \n\n")
print(cbind(n11,p.all.tables))


# find the probability of my table

p.obs.table <- dhyper(my.Aspirin[1,1], sum(my.Aspirin[1,]), sum(my.Aspirin[2,]), sum(my.Aspirin[,1]))
              
# find the various p-values

cat("Probability of the observed table: ", p.obs.table, "\n\n")

cat("One sided p-value for less than alteratives: ", sum(p.all.tables[n11<=my.Aspirin[1,1]]),"\n\n")

cat("One sided p-value for greater than alteratives: ", sum(p.all.tables[n11>=my.Aspirin[1,1]]),"\n\n")

# there are several ways to compute two-sided p-values. See Agresti (2002), p. 93
# One popular way is to sum all tables which are more unusual

cat("Two sided p-value for not equal alteratives: ", sum(p.all.tables[p.obs.table >= p.all.tables]),"\n\n")




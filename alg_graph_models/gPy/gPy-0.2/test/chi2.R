
run_chi2 <- function(filename) {
    data <- read.table(sprintf('%s', filename),header=TRUE)
    mtx  <- matrix(data$count, nrow=length(levels(factor(data$a))))
    #print(chisq.test(mtx,correct=FALSE,simulate.p.value=TRUE,B=10000))
    print(chisq.test(mtx,correct=FALSE))
}

for (file in c('two_independ','two_depend','three_independ','three_depend')) {
    run_chi2(file)
}


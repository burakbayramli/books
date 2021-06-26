x <- read.table("grades",header=T)
n <- nrow(x)

i326 <- (1:n)[x[,1]==326]
x326 <- x[i326,]
nam1 <- as.vector(x[i326,3])
nam2 <- as.vector(x[i326,2])
h <- x326[,4:15]
h <- apply(h,1,mean)
tests <- x326[,16:17]
tests <- apply(tests,1,mean)
exam <- x326[,18]
final <- (.25*h + .5*tests + .25*exam)
y <- cbind(ceiling(h),ceiling(tests),ceiling(exam),ceiling(final))
y <- cbind(nam1,nam2,y)

i727 <- (1:n)[x[,1]==727]
x727 <- x[i727,]
nam1 <- as.vector(x[i727,3])
nam2 <- as.vector(x[i727,2])
h <- x727[,4:15]
h <- apply(h,1,mean)
tests <- x727[,16:17]
tests <- apply(tests,1,mean)
exam <- x727[,18]
final <- (.25*h + .5*tests + .25*exam)
y <- cbind(ceiling(h),ceiling(tests),ceiling(exam),ceiling(final))
y <- cbind(nam1,nam2,y)


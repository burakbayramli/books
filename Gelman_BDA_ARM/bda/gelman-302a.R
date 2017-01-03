alpha.1 <- rbeta (2000, 295, 308)
alpha.2 <- rbeta (2000, 289, 333)
dif <- alpha.2 - alpha.1
hist (dif, xlab="alpha_2 - alpha_1", yaxt="n", breaks=seq(-.12,.08,.01), cex=2)
print (mean(dif>0))

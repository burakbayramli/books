### Example 15.5
###data from Johnson, Johsnon (1972) see book by Clayton and Hills p169

x  = c(90,84,165,307)
x  = matrix(x,2,2)
n  = sum(x)
mr = apply(x,1,sum)
mc = apply(x,2,sum)

### expected counts
E  = matrix(c(mr[1]*mc[1],mr[2]*mc[1],mr[1]*mc[2],mr[2]*mc[2]),2,2)/n

### chi^2 test
U  = sum((x-E)^2/E)
print(U)
print(1-pchisq(U,1))

### LRT test
lrt = 2*(x[1,1]*log( (x[1,1]*n)/(mr[1]*mc[1]) ) +
       x[1,2]*log( (x[1,2]*n)/(mr[1]*mc[2]) ) +
       x[2,1]*log( (x[2,1]*n)/(mr[2]*mc[1]) ) +
       x[2,2]*log( (x[2,2]*n)/(mr[2]*mc[2]) )  )
print(lrt)
print(1-pchisq(lrt,1))

### odds ratio
psi = (x[1,1]*x[2,2])/(x[1,2]*x[2,1])
print(psi)
gamma = log(psi)
print(gamma)
se = sqrt(sum(1/x))
print(se)
w = gamma/se
print(w)
print(2*pnorm(-w))
ci = c(gamma-2*se,gamma+2*se)
print(ci)

### confidence interval on odds scale
ci = exp(ci)
print(ci)



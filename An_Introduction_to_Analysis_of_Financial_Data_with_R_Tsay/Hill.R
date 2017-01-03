"Hill" <- function(x,q){
# Compute the Hill estimate of the shape parameter.
sx=sort(x)
T=length(x)
ist=T-q
y=log(sx[ist:T])
hill=sum(y[2:length(y)])/q
hill=hill-y[1]
sd=sqrt(hill^2/q)
cat("Hill estimate & std-err:",c(hill,sd),"\n")

Hill <- list(est=hill,std=sd)
}
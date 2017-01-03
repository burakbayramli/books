"evtVaR" <- function(xi,sigma,mu,n=21,prob=0.01){
# Comput VaR using the block maximum.
# sigma: scale parameter (It is the alpha_n in the textbook)
# mu: location parameter (It is the beat_n in the textbook)
#      For long position: mu = -beta_n
#      For short position: mu = beta_n
# xi: shape parameter (It is the -k_n of the textbook)
# n: block size
# p: tail probability
if (abs(xi) < 0.00000001)
VaR = mu + sigma*log((-n)*log(1-prob))
if (abs(xi) >= 0.00000001){
v1=1.0-(-n*log(1.0-prob))^{-xi}
VaR = mu - (sigma/xi)*v1
}
print(VaR)
evtVaR<-list(VaR = VaR)
}
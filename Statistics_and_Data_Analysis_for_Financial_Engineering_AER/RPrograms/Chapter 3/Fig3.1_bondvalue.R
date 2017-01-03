#  Program to create Fig 3.2
bondvalue = function(c,T,r,par) 
{
#
#   Computes bv = bond values (current prices) corresponding
#       to all values of yield to maturity in the
#       input vector r
#
#       INPUT
#        c = coupon payment (semi-annual)
#        T = time to maturity (in years)
#        r = vector of yields to maturity (semi-annual rates)
#        par = par value
#
bv = c/r + (par - c/r) * (1+r)^(-2*T)
bv
}
################################

#   Computes the yield to maturity of a bond paying semi-annual
#   coupon payments
#
#   price, coupon payment, and time to maturity (in years)
#   are set below
#
#   Uses the function "bondvalue"
#
price = 1200    #   current price of the bond
C = 40          #   coupon payment
T= 30           #   time to maturity
par = 1000      #   par value of the bond

r = seq(.02,.05,length=300)
value = bondvalue(C,T,r,par) 
yield2M = spline(value,r,xout=price) 

postscript("yield2m.ps",width=6,height=5)
plot(r,value,xlab='yield to maturity',ylab='price of bond',type="l",
    main="par = 1000, coupon payment = 40, T = 30",lwd=2)
abline(h=1200)
abline(v=yield2M)
graphics.off()

################
#  Finding r from the price using a root finder

uniroot(function(r) bondvalue(C,T,r,par) - price, c(0.001,.1))



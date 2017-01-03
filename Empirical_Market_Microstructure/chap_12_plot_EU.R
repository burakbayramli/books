# 
# Written by:
# -- 
# John L. Weatherwax                2009-04-21
# 
# email: wax@alum.mit.edu
# 
# Please send comments and especially bug reports to the
# above email address.
# 
#-----

limit_prices = seq( from=0, to=2, length.out=100 )

EU_of_n = function(n){ 
   EU = ( 1 - exp( - limit_prices ) ) * ( -exp( -(n+1-limit_prices) + 0.5 * (n+1)^2 ) ) + exp( -limit_prices ) * ( - exp( -n + 0.5 * n^2 ) )
   return(EU)
}

neg_log_neg_EU_n_0 = - log( -EU_of_n(0.) )
neg_log_neg_EU_n_m1 = - log( -EU_of_n(-1) )
neg_log_neg_EU_n_m2 = - log( -EU_of_n(-2) )

rng = range( c(neg_log_neg_EU_n_0, neg_log_neg_EU_n_m1, neg_log_neg_EU_n_m2 ) )

#postscript("../../WriteUp/Graphics/Chapter12/dup_fig_12_1.eps", onefile=FALSE, horizontal=FALSE)

plot( limit_prices, neg_log_neg_EU_n_0, ylim=rng, type="l", col="black" )
lines( limit_prices, neg_log_neg_EU_n_m1, col="red" )
lines( limit_prices, neg_log_neg_EU_n_m2, col="green" )

#dev.off()

limit_spot = which.max( neg_log_neg_EU_n_m1 )
optimal_limit_price = limit_prices[limit_spot]

print( sprintf("optimal price=%10.2f; prob_execution= %10.6f",optimal_limit_price, 1-exp(-optimal_limit_price)) )

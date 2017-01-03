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

lambda_buy = seq( from=0, to=2, length.out=100 )
lambda_sell = lambda_buy

p_lambda_buy = 3 - lambda_buy + 0.2 * lambda_buy^2
p_lambda_sell = 2 + 0.2 * lambda_sell + 0.2 * lambda_sell^2

#postscript("../../WriteUp/Graphics/Chapter11/ex_11_1_p_vs_lambda_plot.eps", onefile=FALSE, horizontal=FALSE)

plot( lambda_buy, p_lambda_buy, col="red", type="l", xlab="lambda", ylab="p(lambda)" )
lines( lambda_sell, p_lambda_sell, col="blue" )

#dev.off()

# To find the optimal bid and ask prices we can maximize the expression lambda * ( p_lambda_buy - p_lambda_sell )
exp_to_max = lambda_buy * ( p_lambda_buy - p_lambda_sell ) 
plot( lambda_buy, exp_to_max )

spot = which.max( exp_to_max )
max_value = max(exp_to_max)
max_argument = lambda_buy[spot]
p_bid = p_lambda_sell[spot] 
p_ask = p_lambda_buy[spot]
print( sprintf("max profit= %6.6f; lambda= %6.6f; p_bid = %6.2f; p_ask= %6.2f", exp_to_max[spot], lambda_buy[spot], p_bid, p_ask ) );




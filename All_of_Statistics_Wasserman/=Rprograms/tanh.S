m_.5
theta_seq(-m,m,length=100)
x_seq(-5,5,length=1000)
risk_rep(0,100)
tanh.fun_function(z){
     return( (exp(z)-exp(-z))/(exp(z)+exp(-z)) )
     }
for(i in 1:100){
     f_dnorm(x,theta[i],1)
     temp_f*(theta[i]-m*tanh.fun(m*x))^2
     risk[i]_sum(temp)
     }

postscript("tanh.ps")
plot(theta,risk,type="l")


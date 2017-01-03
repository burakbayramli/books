function x=mu_inv(y,mu) % inverse of mu-law Eq.(7.1-22)
x=(((1+mu).^abs(y)-1)/mu).*sign(y);

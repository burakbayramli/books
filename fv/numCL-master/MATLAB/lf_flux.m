function fhat = lf_flux(um,up,a)

fm = flux(um); fp = flux(up);
fhat = 0.5*( fm+fp - a*(up-um) );

return
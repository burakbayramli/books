function fhat = lw_flux(um,up,dx,dt)

fm = flux(um); fp = flux(up);

fhat = (0.5*(fm+fp) - 0.5*(dt/dx)*0.5*(um+up).*(fp-fm));

return
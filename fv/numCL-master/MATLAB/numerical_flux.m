function fhat = numerical_flux(um,up,dx,dt,A)

% fhat = roe_flux(um,up);
% fhat = godunov_flux(um,up);
fhat = lf_flux(um,up,A);
% fhat = eo_flux(um,up);
% fhat = lw_flux(um,up,dx,dt);

return
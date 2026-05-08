function [omega, nut, nutilde] = update_nut(nutilde,nu,dt,dh,d,u,v)
% For the Spalart-Allmaras model in lid-driven cavity.
% input variables are 2d matrices.
% d: wall distances.
% nut is the turbulent viscosity that gets added onto the nominal
%   viscosity to produce the total effective viscosity.
% nu: nonminal (scalar) viscosity

% apply bc for lid-driven cavity.
nutilde(1,:) = 0;
nutilde(end,:) = 0;
nutilde(:,1) = 0;
nutilde(:,end) = 0;

% Model constants.
sigma = 2/3;
cb1 = 0.1355;
cb2 = 0.622;
kappa = 0.41;
cw2 = 0.3;
cw3 = 2;
cv1 = 7.1;

% Derived model constants.
cw1 = cb1 / kappa^2 + ( 1 + cb2 ) / sigma;

% Derived input variables.
ux = upwind_x(u,u,dh);
uy = upwind_y(u,v,dh);
vx = upwind_x(v,u,dh);
vy = upwind_y(v,v,dh);
S = 2*ux.^2 + 2*vy.^2 + (uy + vx).^2 - 2/3*(ux + vy).^2;
X = nutilde / nu;
fv1 = X.^3 ./ ( X.^3 + cv1^3 );
Stilde = S.^0.5 .* ( 1 ./ X + fv1 );
r = tanh( nutilde ./ ( kappa^2*Stilde.*d.^2 ) ) / tanh(1.0);
g = r + cw2 * ( r.^6 - 6 );
fw = g.*( ( 1 + cw3^6 ) ./ ( g.^6 + cw3^6 ) ).^(1/6);

ntx = upwind_x(nutilde,u,dh);
nty = upwind_y(nutilde,v,dh);
ntxx = spatial_difference_x(nutilde,dh);
ntyy = spatial_difference_y(nutilde,dh);
dnutilde = -( u.*ntx + v.*nty ) ...
    + (nu + nutilde) / sigma .* ( ntxx + ntyy ) ...
    + (cb2 + 1) / sigma * ( ntx.^2 + nty.^2 ) ...
    + cb1 * Stilde .* max(nutilde,nu/100) ...
    - cw1 * fw .* ( nutilde ./ d ).^2;

nutilde = nutilde + dt*dnutilde;
% apply bc for lid-driven cavity.
nutilde(1,:) = 0;
nutilde(end,:) = 0;
nutilde(:,1) = 0;
nutilde(:,end) = 0;

nut = nutilde.*fv1;
omega = turbulent_relaxation(nu, nut);


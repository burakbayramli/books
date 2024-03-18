function W = U2W(U,a,gamma)
% U set of conserved variables, rho*A, rho*u*A, rho*e*A
% W char vars u-2c/(g-1), p/rho^g, u+2c/(g-1);
rho  = U(:,1)./a;
rhou = U(:,2)./a;
u    = U(:,2)./U(:,1);
rhoe = U(:,3)./a;
p    = (gamma-1)*(rhoe-0.5*(rhou.^2)./rho);
c    = sqrt(gamma*p./rho);
% order: u-c u u+c
W(:,1) = u - 2*c/(gamma-1);
W(:,2) = p./rho.^gamma;
W(:,3) = u + 2*c/(gamma-1);




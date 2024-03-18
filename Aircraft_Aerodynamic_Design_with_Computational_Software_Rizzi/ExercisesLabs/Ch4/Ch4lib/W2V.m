function V = W2V(W,gamma)
% U set of conserved variables, rho*A, rho*u*A, rho*e*A
% V primitive rho u p
% W char vars u-2c/(g-1), p/rho^g, u+2c/(g-1);
%W(:,1) = u - 2*c/(gamma-1);
%W(:,2) = p./rho.^gamma;
%W(:,3) = u + 2*c/(gamma-1);
gm1 = gamma-1;
u      = (W(:,1)+W(:,3))/2;
c2     = ((W(:,3)-W(:,1))*gm1/4).^2;
rho    = (c2./(gamma*W(:,2))).^(1/gm1);
%size(u),size(c2),size(rho)
p      = rho.*c2/gamma;
V(:,1) = rho;
V(:,2) = u;
V(:,3) = p;





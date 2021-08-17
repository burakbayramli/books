function Q = IsentropicVortexIC2D(x, y, time)
 
% function Q = IsentropicVortexIC2D(x, y)
% Purpose: compute flow configuration given by
%     Y.C. Zhou, G.W. Wei / Journal of Computational Physics 189 (2003) 159 

% based flow parameters
xo = 5; yo = 0; beta = 5; gamma = 1.4;
rho = 1; u = 1; v = 0; p = 1;

xmut = x-u*time; ymvt = y-v*time;
r = sqrt((xmut-xo).^2 + (ymvt-yo).^2);

% perturbed density
u   = u - beta*exp(1-r.^2).*(ymvt-yo)/(2*pi);
v   = v + beta*exp(1-r.^2).*(xmut-xo)/(2*pi);
rho1 = (1 - ((gamma-1)*beta^2*exp(2*(1-r.^2))/(16*gamma*pi*pi))).^(1/(gamma-1));
p1   = rho1.^gamma;

Q(:,:,1) = rho1; Q(:,:,2) = rho1.*u; Q(:,:,3) = rho1.*v;
Q(:,:,4) = p1/(gamma-1) + 0.5*rho1.*(u.^2 + v.^2);
return;



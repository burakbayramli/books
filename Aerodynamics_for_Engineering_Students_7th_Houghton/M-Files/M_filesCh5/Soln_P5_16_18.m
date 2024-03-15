% Problem 5.16 and 5.18
clear;clc
% y = 0:.01:1;
% u = (3/2.*y - 1/2.*y.^3);
% plot(u,y)
% syms x y 
% u = 2*y + y/(x^2+y^2)^(1/2); % =  d psi / dy
% v = -2*x -x/(x^2+y^2)^(1/2); % = -d psi / dx
%  psi1 = int(u,'y')
%  psi2 = -int(v,'x')
% % zeta = diff(v,'x') - diff(u,'y');
% % simple(zeta)
% % latex(ans)
% [x,y] = meshgrid(-1:.05:1);
% psi = x.^2 + y.^2 + sqrt(x.^2 + y.^2);
% contour(x,y,psi),axis image
% syms U R r z Q 
%  phi = U*z - Q/(4*pi*sqrt(r^2+z^2));
%  u = diff(phi,'z');
%  v = diff(phi,'r');
% psi = U*r^2/2 - Q/(4*pi)*(z/sqrt(r^2+z^2));
% us = diff(psi,'r')/r;
% us = simple(us);
% vs = -diff(psi,'z')/r;
% vs = simple(vs)
% format compact
% u
% us
% v
% vs
% format
%U = 1; Q = 4*pi; Dividing streamline 
z = -2:.1:2;
for ip=1:length(z)
    r = 0.1;
    for it = 1:2000
        r = sqrt(2*z(ip)/sqrt(z(ip)^2+r^2) + 2);
    end
    rb(ip) = r;
end
plot(z,rb,'r')

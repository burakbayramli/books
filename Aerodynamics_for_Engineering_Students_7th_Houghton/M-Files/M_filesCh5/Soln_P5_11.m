%
% Hints and instructors solution to Problem 11 in Chapter 3
% February 1, 2012
 clear;clc
%
% SYMBOLICS USED TO DETERMINE p-lines, ps-lines and ph-lines,
% i.e., isobars, streamlines and equipotentials, respectively. 
format compact
syms x y a b c H
ps = x^2/2 + b*x*y - y^2/2
u =   diff(ps,'y')
v = - diff(ps,'x')
% diff(v,'x') - diff(u,'y')
% RELATIONSHIP FOR IRROTATIONAL FLOW: 
% % This difference is -a+c ==> a = c for irrotational flows.
% diff(u,'x') + diff(v,'y')
% 
% This sum is zero; hence, flow is incompressible.
p = H - (u^2+v^2)/2
% ph1 = int(u,'x')
% ph2 = int(v,'y')
phi = 1/2*b*x^2 - y*x-1/2*b*y^2
uu = diff(phi,'x')
vv = diff(phi,'y')
format
clear
figure(1)
% Results of symbolics applied computationally to show that 
% isobars, streamlines and equipotentials do not coincide. 
[x,y] = meshgrid(-5:.125:5,-3:.125:3);
xx = -5:.125:5; 
yy = -3:.125:3;
b = 10; H = 1;
for n = 1:length(xx)
    for m = 1:length(yy)
        ps(m,n) = x(m,n)^2/2 + b*x(m,n)*y(m,n) - y(m,n)^2/2;
        u(m,n) = b*x(m,n) - y(m,n);
        v(m,n) = -x(m,n) - b*y(m,n);
        p(m,n) = 1 - (u(m,n)^2+v(m,n)^2)/2;
        phi(m,n) = 1/2*b*x(m,n)^2 - y(m,n)*x(m,n)-1/2*b*y(m,n)^2;
    end
end
contour(x,y,ps,'b'),hold on
contour(x,y,p,'k')
contour(x,y,phi,'r')
% % CONCLUSION: Constant lines of ps, p and phi do not coincide (as 
% % shown graphically by the contour plots). You could also show that 
% % the gradients of p, ps and phi are not coincident vectors. 
clear
figure(2)
%
% Rotational flows
% In rotational flow curl u ~= 0 and, hence, a velocitty potential
% cannot be defined. The streamline, on the other hand, certainly 
% can be defined. 
% format compact
% syms x y a b c po
% ps = a*x^2/2 + b*x*y - c*y^2/2
% u =   diff(ps,'y')
% v = - diff(ps,'x')
%  diff(v,'x') - diff(u,'y')
% % % This difference is -a+c ==> a = c for irrotational flows.
%  diff(u,'x') + diff(v,'y')
% % This sum is zero; hence, flow is incompressible.
% p =  - (u^2+v^2)/2
% format
[x,y] = meshgrid(-5:.125:5,-3:.125:3);
xx = -5:.125:5; 
yy = -3:.125:3;
b = 1; c = 1; a = -1;
for n = 1:length(xx)
    for m = 1:length(yy)
        ps(m,n) = a*x(m,n)^2/2 + b*x(m,n)*y(m,n) - c*y(m,n)^2/2;
        u(m,n) = b*x(m,n) - c*y(m,n);
        v(m,n) = -a*x(m,n) - b*y(m,n);
        p(m,n) =  1 - (u(m,n)^2+v(m,n)^2)/2;
    end
end
contour(x,y,ps,'b'),hold on
contour(x,y,p,'+k')
%
% Yes. The streamlines and isobars can coincide if flow is rotational.
%
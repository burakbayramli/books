function simil(inputfile)
%SIMIL Similarity transformation between two sets of points
%      given by coordinates (x,y) and (xi,eta). The inputfile
%      contains the coordinates arranged in two columns:
%		    x1 y1
%		    x2 y2
%		    ...
%		    xp yp
%		    xi1 eta1
%		    xi2 eta2
%		    ...
%		    xip etap
%      Important: The order of points must be the same in
%      both lists

%      Typical call simil('sim.dat')

%Kai Borre 04-13-96
%Copyright (c) by Kai Borre
%$Revision: 1.0 $  $Date: 1997/09/26  $

eval(['load ' inputfile]);
tok = strtok(inputfile,'.');
S = eval(tok);
[m,n] = size(S);
p = m/2;
A = [S(1:p,1)  S(1:p,2)  ones(p,1) zeros(p,1);
     S(1:p,2) -S(1:p,1) zeros(p,1)	ones(p,1)];
b = [S(p+1:2*p,1); S(p+1:2*p,2)];

format bank
xs = mean(S(1:p,1))
ys = mean(S(1:p,2))
xis = mean(S(p+1:2*p,1))
etas = mean(S(p+1:2*p,2))
S(1:p,1) = S(1:p,1)-xs;
S(1:p,2) = S(1:p,2)-ys;
S(p+1:2*p,1) = S(p+1:2*p,1)-xis;
S(p+1:2*p,2) = S(p+1:2*p,2)-etas
norm(S(1:p,1))^2+norm(S(1:p,2))^2
x_xi = S(1:p,1)'*S(p+1:2*p,1)
y_xi = S(1:p,2)'*S(p+1:2*p,1)
x_eta = S(1:p,1)'*S(p+1:2*p,2)
y_eta = S(1:p,2)'*S(p+1:2*p,2)
x = A\b
k = sqrt(x(1)^2+x(2)^2);
phi = atan2(x(2),x(1))*180/pi;
sigma0 = norm(A*x-b)/sqrt(2*p-4)
fprintf('k is %20.10g \n',k)
fprintf('phi is %18.6g degrees\n',phi)
fprintf('t_x is %18.10g m\n',x(3))
fprintf('t_y is %18.9g m\n',x(4))
fprintf('sigma_0 is %14.1g m\n',sigma0)
format short e
Sigmax = sigma0*inv(A'*A)
res = A*x-b;
out = [res(1:p) res(p+1:2*p)]
%%%%%% end simil.m  %%%%%%%%%%%%%%%%%%%

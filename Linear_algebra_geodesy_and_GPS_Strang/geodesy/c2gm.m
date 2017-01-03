function  c2gm(x,y,z,a,invf)
%C2GM  Convertion of cartesian coordinates (X,Y,Z) in m to geographical
%      coordinates (phi,lambda,h) in degrees, degrees and m on a
%      reference ellipsoid with semi-major axis a and flattening f=1/invf

%The iterative part is a modified implementation of
% N. Bartelme and P. Meissl (1975)
%     Ein einfaches, rasches und numerisch stabiles
%     Verfahren zur Bestimmung des k\"urzesten Abstandes eines
%     Punktes von einem sph\"aroidischen Rotationsellipsoid,
%     Allgemeine Vermessungs-Nachrichten, Seite 436--439

%Kai Borre 10-14-95; revised 12-23-95
%Copyright (c) by Kai Borre
%$Revision: 1.0 $  $Date: 1997/10/15  $

p = sqrt(x^2+y^2);
f = 1/invf;
ex2 = (2-f)*f/((1-f)^2);
b = a/sqrt(1+ex2);
c = a*sqrt(1+ex2);

h = 0.01; oldh = 0; mu = 0;
while abs(h-oldh) > 1.e-4
   oldh = h;
   R = a/sqrt(1+ex2*z^2/(z^2+p^2));
   tal = sqrt((p*(1+mu/R^2)/(a*(1+mu/a^2)))^2 + ...
                       (z*(1+mu/R^2)/(b*(1+mu/b^2)))^2);
   mu = (tal-1)*R^2;
   pe = p/(1+mu/a^2);
   ze = z/(1+mu/b^2);
   h = norm([p-pe, z-ze]);
   phi = atan(z/((p*(1-(2-f)*f*R/(R+h)))));
end

N = c/sqrt(1+ex2*cos(phi)^2);
phi = atan(z/((p*(1-(2-f)*f*N/(N+h)))));
lambda = atan2(y,x);
bo = r2dms(phi);
lo = r2dms(lambda);
fprintf('\n      phi = %3.0f %2.0f %8.5f',bo(1),bo(2),bo(3))
fprintf('\n   lambda = %3.0f %2.0f %8.5f',lo(1),lo(2),lo(3))
fprintf('\n        h = %8.3f\n',h)
%%%%%%%%%%%%%% end c2gm.m  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
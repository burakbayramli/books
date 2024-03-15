% Example: Flow over a cylinder with circulation
clear;clc
V = 1; mu = 1; Gamma = -0; R = sqrt(mu/V);
N = 360; dthe = 2*pi/N;
the = 0:dthe:2*pi;
for n = 1:length(the)-1
    them(n) = the(n) + dthe/2;
    x(n) = R*cos(them(n)); y(n) = R*sin(them(n));
    u(n) = V + V*R^2*(y(n)^2 - x(n)^2)/(x(n)^2 + y(n)^2)^2  ...
         - Gamma/2/pi*y(n)/(x(n)^2+y(n)^2);
    v(n) = -2*V*R^2*y(n)*x(n)/(x(n)^2 + y(n)^2)^2 ...
         + Gamma/2/pi*x(n)/(x(n)^2+y(n)^2);
    ut(n) = -u(n)*sin(them(n)) + v(n)*cos(them(n));
    ur(n) = u(n)*cos(them(n)) + v(n)*sin(them(n));
    Cp(n) = 1 - (u(n)^2+v(n)^2)/V^2;
end
 plot(x,y,'k',[-1 1],[0 0],'k'),axis image, hold on
 plot(x,Cp),axis([-2 2 -5 1]), hold off
 CL = -sum(Cp.*sin(them))*dthe
 CLexact = -2*Gamma/R/V
 for mm=1:length(the)-1
     them(mm) = the(mm) + dthe/2;
     uthe(mm) = -2*sin(them(mm)) + Gamma/2/pi/R/V;
    Cp2(mm) = 1 - uthe(mm)^2;
 end
 CL2 = -sum(Cp2.*sin(them))*dthe


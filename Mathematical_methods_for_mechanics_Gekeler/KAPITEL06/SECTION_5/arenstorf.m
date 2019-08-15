function arenstorf
% Plots several Arenstorf orbits
% Figures 6.15 and 6.16
clf,clc
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
example = 100;
while ~ismember(example,[1,2,3,4,5,6,7,8])
example = input('Which example ? (1,2,3,4,5,6,7,8)') 
end
EE = 1.6;
plot(-EE,-EE,'k.','markersize',6), hold on
plot(EE,EE,'k.','markersize',6), hold on
axis equal , axis manual
Quotient = [1,2;2,3;3,4;3,5;4,5;5,4;7,6;7,8];

switch example
case 1, m = Quotient(1,1); k = Quotient(1,2); rr = 0.04;
case 2, m = Quotient(2,1); k = Quotient(2,2); rr = 0.04;
case 3, m = Quotient(3,1); k = Quotient(3,2); rr = 0.04;
case 4, m = Quotient(4,1); k = Quotient(4,2); rr = 0.04;
case 5, m = Quotient(5,1); k = Quotient(5,2); rr = 0.04;
case 6, m = Quotient(6,1); k = Quotient(6,2); rr = 0.04;
case 7, m = Quotient(7,1); k = Quotient(7,2); rr = 0.04;
case 8, m = Quotient(8,1); k = Quotient(8,2); rr = 0.04;
end
%circle(1.5,1.5,0.03,'k')
%circle(-1.5,-1.5,0.03,'k')

a = (m/k)^(2/3); e = 0.5;
PSI = linspace(0,2*pi*k,300);
X = a*(e + cos(PSI)); Y = a*sqrt(1 - e*e)*sin(PSI);
T = sqrt(a)^3*(PSI + e*sin(PSI));
UX = cos(T).*X + sin(T).*Y; UY = cos(T).*Y - sin(T).*X;
plot(UX,UY,'k','linewidth',2),hold on
circle(0,0,rr,'k'), circle(1,0,rr,'k')
grid on
axis off


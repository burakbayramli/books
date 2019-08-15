% BILD020406, Programm zum Plotten des Stabilit"atsbereiches
% von Einschrittverfahren
% Rosenbrock-Verfahren
clf, clc, clear
d = 1/(2 + sqrt(2));
m = 400; c1= 0.6; d1 = 0.3;
%
p = [d*d-2*d+0.5, 1-2*d, 1];
q = [d*d, -2*d, 1];

n = length(p);
y = p - q;
r = roots(y);
Re = real(r);
Im = imag(r);
v1 = [Re(1) Im(1)];
v2 = [];
for k = 1:m
   l = 2*pi*k/m;
   u = exp(l*i);
   y = p - u*q;
   r = roots(y);
   Re = real(r);
   Im = imag(r);
   K = find(Im >= 0);
   Re1 = Re(K);
   Im1 = Im(K);
   w1 = [Re1, Im1];
   v1 = [v1; w1];

   K = find(Im < 0);
   Re2 = Re(K);
   Im2 = Im(K);
   w2 = [Re2, Im2];
   v2 = [v2; w2];
v1
end
plot(v1(:,1),v1(:,2),'k','LineWidth',2), hold on
plot(v2(:,1),v2(:,2),'k','LineWidth',2), hold on
fill(v1(:,1),v1(:,2),'y')
fill(v2(:,1),v2(:,2),'y')

c = 0.25;
% -- X-Achse ------------
X = [-3,15]; Y = [0,0];
arrow(X,Y,c1,d1,'k',2), hold on
% -- Y- Achse ---------
X = [0,0]; Y = [-9,9];
arrow(X,Y,c1,d1,'k',2), hold on
circle(6,0,c,'w'), hold on
text(6,-1,'6','fontsize',22)
text(13.5,-0.8,'\xi','fontsize',22)
text(0.8,8.5,'\eta','fontsize',22)
text(1.8,3,'Komplement','FontSize',22)
text(4,-3,'von S','FontSize',22)

axis equal tight
grid off

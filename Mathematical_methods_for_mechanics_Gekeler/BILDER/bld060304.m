function bld020607
% Ellipse, p = 1, e = 1/2
% V(r) = -1/r + 1/(2r^2)
clf
c = 0.18;
d = 0.07;
X = [-2.2,1];
Y = [0, 0];
arrow(X,Y,c,d,'k',2),hold on
X = [0, 0];
Y = [-1.5,1.5];
arrow(X,Y,c,d,'k',2),hold on
% -- Ellipse -----------------
PHI = linspace(0,2*pi,80);
R = 1./(1 + 0.5*cos(PHI));
[X,Y] = pol2cart(PHI,R);
plot(X,Y,'k','linewidth',2),hold on
circle(0,0,0.07,'w')
% -- Parabel ------------------------
RR = 0.7;
PHI = linspace(-RR*pi, RR*pi);
R = 1./(1 + cos(PHI));
[X,Y] = pol2cart(PHI,R);
plot(X,Y,'k','linewidth',2),hold on
% -- Hyperbel ------------------
RR = 0.62;
PHI = linspace(-RR*pi, RR*pi);
R = 1./(1 + 1.5*cos(PHI));
[X,Y] = pol2cart(PHI,R);
plot(X,Y,'k','linewidth',2),hold on
%%%%%%%%%%%%%%%%%%%%%%%%%
text(-1.7,0.5,'\epsilon = 0.5','Fontsize',22)
text(-1.7,1.5,'\epsilon = 1','Fontsize',22)
text(-0.5,1.9,'\epsilon = 1.5','Fontsize',22)
% -- Rahmen -------------
XR = [-2.2,1,1,-2.2,-2.2];
YR = [-2.2,-2.2,2.2,2.2,-2.2];
plot(XR,YR,'k','linewidth',2)
axis equal tight
grid on
axis off

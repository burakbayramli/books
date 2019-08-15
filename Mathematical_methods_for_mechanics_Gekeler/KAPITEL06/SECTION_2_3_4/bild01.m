function bild01
% Figure for DEMO1.M
% complete Ellipse in polar coordinates
% and piecewisw solution by a differential system
clf
load daten1 T X V XX Y1 Y2 Parmeter
[Y1X,Y1Y]   = pol2cart(Y1(:,1),Y1(:,2));
[Y2X,Y2Y]   = pol2cart(Y2(:,1),Y2(:,2));
c = 0.1; d = 0.05;  % Parameter fuer Pfeile
G = Parmeter(1); M = Parmeter(2); m = Parmeter(3);
alfa = Parmeter(4);
% -- Daten fuer volle Ellipse --------------
v0   = norm(V); r0 = norm(X);
phi  = acos(X'*V/(r0*v0));    % Winkel zw. X und V
L    = m*v0*r0*sin(phi);      % Drehimpuls (konstant!)
E    = 0.5*m*v0*v0 - G*M*m/r0;
if E >= 0
   disp('keine Ellipse')
   return
end
a = - 0.5*G*M*m/E; p = L*L/(G*m*m*M); e = sqrt(2*p*E/(G*m*M) + 1);
E_e = [E,e]
Poly  = [m/2, -G*M*m*m/L, G*M*m/(2*a)];
ROOTS = roots(Poly);
v1    = ROOTS(1); v2 = ROOTS(2);
r1    = L/(m*v1); r2    = L/(m*v2);
COSPHI0 = 1;
if e > 0
   COSPHI0 = (a*(1 - e*e) - r0)/(r0*e);
end
phi0  = acos(COSPHI0);
phi2  = atan2(X(2),X(1));
P0    = [X(1); X(1)*tan(phi2+phi0)];
if phi0 < phi2
   P0 = [X(1); -X(1)*tan(phi2+phi0)];
end
P0    = P0/norm(P0);
psi   = atan2(P0(2),P0(1));
% Bild der ganzen Ellipse -------------
TT      = linspace(0,2*pi,100);
GAMMA   = - psi - pi;
RR      = p./(1 + e*cos(TT + GAMMA));
[XA,YA] = pol2cart(TT,RR);
plot(XA,YA,'k','linewidth',2), hold on
% ----------------------------------------
% Bild der Anfangs- und Endpunkte
[XA,YA] = pol2cart(XX(1,:),XX(2,:));
X2 = [0;XA(1)];; Y2 = [0;YA(1)];
plot(X2,Y2,'k','linewidth',2), hold on
X3 = [0;XA(2)];; Y3 = [0;YA(2)];
plot(X3,Y3,'k','linewidth',2), hold on
X4 = [0;XA(3)];; Y4 = [0;YA(3)];
plot(X4,Y4,'k','linewidth',2), hold on
X5 = [0;XA(4)];; Y5 = [0;YA(4)];
plot(X5,Y5,'k','linewidth',2), hold on
X6 = [0,XA(1),Y1X',XA(2),0];
Y6 = [0,YA(1),Y1Y',YA(2),0];
fill(X6,Y6,'y'), hold on
X7 = [0,XA(3),Y2X',XA(4),0];
Y7 = [0,YA(3),Y2Y',YA(4),0];
fill(X7,Y7,'y'), hold on

rr = 0.02;
circle(XA(1),YA(1),rr,'w')
circle(XA(2),YA(2),rr,'w')
circle(XA(3),YA(3),rr,'w')
circle(XA(4),YA(4),rr,'w')
circle(0,0,rr,'r')
%text(1.5,0.8,'\Delta\phi','fontsize',20)
% -- Ecken des Bildes ---------
plot(-0.2,-0.4,'.','color','w','markersize',3)
plot(1.1,0.4,'.','color','w','markersize',3)
grid on
axis equal tight
%axis off

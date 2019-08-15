function fig0610_11(E)
% Bild fuer Kepler
load grafikdaten c d rr Parlaenge Hyplaenge
load daten X V2 p e psi Achse F2
%plot(Achse(1,:),Achse(2,:)), hold on
X1  = [0, X(1)];
Y1  = [0, X(2)];
X2  = [X(1),X(1)+V2(1)]; Y2  = [X(2),X(2)+V2(2)];
plot(X1,Y1,'k','linewidth',2), hold on   % Ortsvektor
arrow(X2,Y2,c,d,'k',2);                  % Geschw.-vektor
% -- ELLIPSE ----------------------
if E < 0
   TT      = linspace(0,2*pi,400);
   GAMMA   = - psi - pi;
   RR      = p./(1 + e*cos(TT + GAMMA));
   [XA,YA] = pol2cart(TT,RR);
   plot(XA,YA,'k','linewidth',2), hold on
   plot(Achse(1,:),Achse(2,:),'b'), hold on
   circle(0,0,rr,'b')
   circle(F2(1),F2(2),rr,'r')
   circle(Achse(1,1),Achse(2,1),rr,'w')
   circle(Achse(1,2),Achse(2,2),rr,'w')
   circle(X(1),X(2),rr,'w')
end
% -- PARABEL -------------------------
if E == 0
   TT      = linspace(Parlaenge(1),Parlaenge(2),40);
   RR      = p./(1 + e*cos(TT));
   [XA,YA] = pol2cart(TT,RR);
   XN      = [XA;YA];
   D       = [cos(psi), - sin(psi); sin(psi), cos(psi)];
   XN      = D*XN;
   plot(XN(1,:),XN(2,:),'r','linewidth',2), hold on
   plot(Achse(1,:),Achse(2,:)), hold on
   circle(X(1),X(2),rr,'w')   % Massepunkt
   circle(0,0,rr,'w')         % Ursprung
   circle(Achse(1,1),Achse(2,1),rr,'w')
   circle(Achse(1,2),Achse(2,2),rr,'w')
end
% -- HYPERBEL ------------------------
if E > 0
   TT      = linspace(Hyplaenge(1),Hyplaenge(2),20);
   RR      = p./(1 + e*cos(TT));
   [XA,YA] = pol2cart(TT,RR);
   XN      = [XA;YA];
   D       = [cos(psi), - sin(psi); sin(psi), cos(psi)];
   XN      = D*XN;
   plot(XN(1,:),XN(2,:),'k','linewidth',2), hold on
   circle(X(1),X(2),rr,'w')   % Massepunkt
   circle(0,0,rr,'w')         % Ursprung
   circle(Achse(1,1),Achse(2,1),rr,'w')
   circle(Achse(1,1),Achse(2,1),rr,'w')
   circle(Achse(1,2),Achse(2,2),rr,'w')
end

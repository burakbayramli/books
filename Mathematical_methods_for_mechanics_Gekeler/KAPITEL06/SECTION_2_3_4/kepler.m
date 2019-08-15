function [p,e,psi,Achse,F2] = kepler(X,V,E,Parmeter);
% Auxiliary file for demo3.m
% -- beliebiger Kegelschnitt aus Startwerten
% -- ein Brennpunkt im Ursprung
% INPUT: P Punkt auf Kegelschnitt
%        V Geschwindigkeit von P
%        E Gesamtenergie
% OUTPUT: p Parameter, e numerische Exzentrizitaet
%         psi Winkel der Hauptachse gegen x-Achse
%         F2 zweiter Brennpunkt bei Ellipse
G = Parmeter(1); M = Parmeter(2); m = Parmeter(3);
% -------------------------------------------
p  = NaN; e = NaN; psi = NaN; Achse = zeros(2,2);
v0 = norm(V); r0 = norm(X);
phi = acos(X'*V/(r0*v0));    % Winkel zw. X und V
L   = m*v0*r0*sin(phi);      % Drehimpuls (konstant!)
% -- ELLIPSE ---------------------------
if E < 0
   a   = - 0.5*G*M*m/E;       % grosse Achse
   p   = L*L/(G*m*m*M);
   e   = sqrt(2*p*E/(G*m*M) + 1);
   D   = [cos(phi), -sin(phi); sin(phi), cos(phi)];
   F2  = X + D*V*(2*a-r0)/v0; %  2. Brennpunkt
   psi = atan2(F2(2),F2(1));
   F2N = F2/norm(F2);
   Achse = [(a + e*a)*F2N,-(a-e*a)*F2N];
end
% -- PARABEL ------------------------
if E == 0
   F2 = [];
   e = 1;
   CC  = X'*V/(r0*v0);
   phi = acos(CC);
   if CC < 0, V = - V; end
   L   = m*v0*r0*sin(phi);  % Drehimpuls
   p   = L*L/(G*m*m*M);
   phi = 2*phi + pi;
   D   = [cos(phi), -sin(phi); sin(phi), cos(phi)];
   P0  = D*X;
   P0  = 0.5*p*P0/norm(P0);
   psi = atan2(P0(2),P0(1));
   Achse(:,2) = P0;
end
% -- HYPERBEL ------------------------
if E > 0
   F2    = [];
   a     = 0.5*G*M*m/E;
   p     = L*L/(G*m*m*M);
   e     = sqrt(2*p*E/(G*m*M) + 1);
   %poly  = [m/2, -G*M*m*m/L, -G*M*m/(2*a)];
   %ROOTS = roots(poly);
   %v1    = ROOTS(1); v2 = ROOTS(2);
   %r1    = L/(m*v1); r2    = L/(m*v2);
   %if r1 > r2,  e = (r1 + a)/a;
   %else,  e = (r2 + a)/a;
   %end
   % ergibt gleiches e
   psi   = 2*phi;
   D     = [cos(psi), - sin(psi); sin(psi), cos(psi)];
   Y     = D*X;
   F2    = X - (norm(X) + 2*a)*Y/norm(Y);
   Achse = [[0;0],a*(e-1)*F2/norm(F2)];
   psi   = atan2(F2(2),F2(1));
end

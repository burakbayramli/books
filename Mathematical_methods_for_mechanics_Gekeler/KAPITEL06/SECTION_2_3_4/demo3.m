function demo3
% Abbildung zu Kap. VI,3 (f)
% Beliebiger Kegelschnitt aus Startwerten
% Der Ursprung ist Brennpunkt und muss LINKS
% von der positiv orientierten Kurve liegen
% INPUT G: entspricht Gravitationskonstante
%       M Masse des Zentralkoerpers im Brennpunkt FEST
%       m Masse des Massepunktes P
%       X Ortsvektor von P; V Geschwindigkeitsvektor in P
% OUTPUT: Kegelschnitt in exakter Darstellung
%         mit Symmetrieachsen
clc, clf, clear, format compact
nr = 100;
while ~ismember(nr,[1,2])
   nr   = input(' Beispiel Nr. (1/2) ');
end
switch nr
case 1
   % Verschiedene Laengen von x(0)
   % -- Grafikparameter
   c  = 0.12; d  = 0.07;  % fuer Pfeil
   rr = 0.4;            % fuer Kreise
   Parlaenge = [-2; 2];
   Hyplaenge = [-2.3; 2.3];
   save grafikdaten c d rr Parlaenge Hyplaenge
   % -- Rahmen -------------------
   LU = [-20,-30];  RO = [20,5];
   XR = [LU(1),RO(1),RO(1),LU(1),LU(1)];
   YR = [LU(2),LU(2),RO(2),RO(2),LU(2)];
   plot(XR,YR,'k','linewidth',2), hold on
   axis equal tight, axis manual
   % -- Parameter --------------
   G = 1; M = 1; m = 1; Parmeter = [G,M,m];
   % --Anfangswerte ------------
   X   = [1; 1]; V = [-3;1];
   % Orientierung pruefen ---------
   X1 = [X;0]; V1 = [V;0];
   Z = cross(X1,V1);
   if Z(3) <= 0
      disp('falsche Orientierung!')
      return
   end
   V  = V/norm(V);
   V2  = 4*V/5;  % Ellipse
   v0 = norm(V2);
   % -- Verschiedene Laengen von |x(0)|
   X1 = X;
   X4 = 2*X/norm(X);
   X5 = 2.5*X/norm(X);
   X6 = 2.75*X/norm(X);
   X7 = 2.85*X/norm(X);
   X8 = 3.5*X/norm(X);
   X9 = 4*X/norm(X);
   ORT = [X4,X5,X6,X7,X8,X9];
   for I = 1:size(ORT,2)
      X = ORT(:,I);
      r0 = norm(X);
      E  = 0.5*m*v0*v0 - G*M*m/r0;
      [p,e,psi,Achse,F2] = kepler(X,V2,E,Parmeter);
      save daten X V2 p e psi Achse F2
      fig0610_11(E)
      disp('---------------')
      E_e_F2 = [E,e,F2']
   end
   grid on
   axis off
case 2 % versch. Laengen von v(0)
   % -- Grafikparameter
   c  = 0.15; d  = 0.09;  % fuer Pfeil
   rr = 0.06;            % fuer Kreise
   % -- Rahmen -------------------
   LU = [-2.5;-3.6];  RO = [4;1.6];
   XR = [LU(1),RO(1),RO(1),LU(1),LU(1)];
   YR = [LU(2),LU(2),RO(2),RO(2),LU(2)];
   plot(XR,YR,'k','linewidth',2), hold on
   axis equal tight, axis manual
   Parlaenge = [-2.5; 1.7];
   Hyplaenge = [-1.7; 1.5];
   save grafikdaten c d rr Parlaenge Hyplaenge
   % -- Parameter --------------
   G = 1; M = 1; m = 1; Parmeter = [G,M,m];
   % --Anfangswerte ---------------
   X   = [1; 1]; V = [-3;1];
   % Orientierung pruefen ---------
   X1 = [X;0]; V1 = [V;0];
   Z = cross(X1,V1);
   if Z(3) <= 0
      disp('falsche Orientierung!')
      return
   end
   V1    = V/norm(V);
   V2    = 2*V1/5;  % Ellipse
   V3    = 3*V1/5; % Ellipse
   V4    = 4*V1/5; % Ellipse
   V5    = V1;     % Ellipse
   vv    = sqrt(2*G*M/norm(X))   % fuer Parabel
   V6    = vv*V1/norm(V1);  %Parabel
   V7    = 1.5*V1; % fuer Hyperbel
   SPEED = [V2,V3,V4,V5,V6,V7];
   r0    = norm(X);
   for I = 1:size(SPEED,2)
      V2 = SPEED(:,I); v0 = norm(V2);
      E  = 0.5*m*v0*v0 - G*M*m/r0;
      [p,e,psi,Achse,F2] = kepler(X,V2,E,Parmeter);
      save daten X V2 p e psi Achse F2
      fig0610_11(E)
      disp('---------------')
      E_e_F2 = [E,e,F2']
   end
   axis off
end

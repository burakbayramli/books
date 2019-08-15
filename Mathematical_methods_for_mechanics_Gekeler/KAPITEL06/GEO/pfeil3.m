function pfeil3(X,Y,Z,h,r);
% zeichnet 3D-Pfeil mit Kegel an der Spitze
% X X-Koordinaten des Pfeils
% Y Y-Koordinaten des Pfeils
% Z Z-Koordinaten der Pfeils
% Spitze des Pfeils in (X(2),Y(2),Z(2))
% h Hoehe des Kegels an der Spitze
% r Radius der Basis des Kegels an der Spitze
colormap(gray)
m = 5; n = 20;
% -- Kegel mit z-Achse als Symmetrieachse ----
[XX,YY,Z1,Z2] = kegel(m,n,h,r);
DIFF  = [X(2)-X(1); Y(2)-Y(1); Z(2)-Z(1)];
DIFFL = norm(DIFF);
ALF   = acos(DIFF(3)/DIFFL);
if norm(DIFF(1:2)) ~= 0
   WINKEL = -ALF;
   ACHSE  = [DIFF(2);-DIFF(1);0];
else
   WINKEL = 0;
   ACHSE  = [0;-1;0];
end
[XN,YN,ZN] = drehen(XX,YY,-Z1,ACHSE,WINKEL);
[XM,YM,ZM] = drehen(XX,YY,-Z2,ACHSE,WINKEL);
[M1,N1]    = size(XN);
XN = XN + X(2)*ones(M1,N1);
YN = YN + Y(2)*ones(M1,N1);
ZN = ZN + Z(2)*ones(M1,N1);
XM = XM + X(2)*ones(M1,N1);
YM = YM + Y(2)*ones(M1,N1);
ZM = ZM + Z(2)*ones(M1,N1);
surf(XN,YN,ZN), hold on
surf(XM,YM,ZM), hold on

function [XN,YN,ZN] = drehen(X,Y,Z,A,phi)
% -- Drehmatix --------------------------
a   = A/norm(A);
C   = [0, -a(3), a(2); a(3), 0, -a(1); -a(2), a(1), 0];
DD  = cos(phi)*eye(3) + (1 - cos(phi))*a*a' + sin(phi)*C;
XN  = 0*X; YN = 0*Y; ZN = 0*Z;
for I = 1:size(X,1)
   AA      = DD*[X(I,:);Y(I,:);Z(I,:)];
   XN(I,:) = AA(1,:);
   YN(I,:) = AA(2,:);
   ZN(I,:) = AA(3,:);
end

function [X,Y,Z1,Z2] = kegel(m,n,h,r)
% INPUT: m : Scheibenanzahl, n : Kreisunterteilung
%        h : Hoehe,          r : Kreisradius
%        A : Drehachse bei Drehung, phi : Drehwinkel
Z       = linspace(0,1,m);
R       = r*Z;
[X,Y,Z] = cylinder(R,n);
Z1      = h*Z;
Z2      = 0*Z + h;


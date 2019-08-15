function kegel(Z,r,s,A,PHI,PSI)
% zeichnet Kegel
% A : Spitze des Kegels
% Z : Achse
% s : Laenge der Achse (normiert)
% r : Radius
% PHI : Startwinkel fuer sichtbare Mantelflaeche
% PHI : Startwinkel fuer sichtbare Mantelflaeche
% PSI : Sichtbare Mantelflaeche von PHI bis PHI + PSI
% Center des Grundkreises ist A - s*Z;

N = 50; Z = Z(:); A = A(:);
if norm(Z) == 0, return, end
Z = Z/norm(Z);
%C1= [A, A-s*Z];
if Z(1) == 0
V = -[0;Z(3);-Z(2)]; V = V/norm(V);
else
   V = -[Z(3);0;-Z(1)]; V = V/norm(V);
end
W = cross(Z,V);
% -- Grundkreis fuellen -------------
TT = linspace(0,2*pi,N);
Kreis = (A - s*Z)*ones(1,N) + r*V*cos(TT) + r*W*sin(TT);
fill3(Kreis(1,:),Kreis(2,:),Kreis(3,:),'b','erasemode','none'), hold on
% -- Mantelflaeche fuellen ---------------------
TT1 = linspace(PHI,PHI + PSI,N);
SEGM1 = (A - s*Z)*ones(1,N) + r*V*cos(TT1) + r*W*sin(TT1);
FL1 = [A,SEGM1,A];
fill3(FL1(1,:),FL1(2,:),FL1(3,:),'b','erasemode','none'), hold on

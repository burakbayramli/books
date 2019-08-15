function kegel(Z,r,s)
% zeichnet Kegel mit Spitze im Ursprung
% Zeichnet Kegel
% Z : Achse
% s : Laenge der Achse
% r : Radius
%clf
%figure
set(gcf,'renderer','zbuffer')
%set(gca,'nextplot','add')
hidden on
N = 100;
M = 50;
A = [0;0;0];
if norm(Z) == 0
   return
end
Z = Z/norm(Z);
C1= [A, s*Z];
%plot3(C1(1,:),C1(2,:),C1(3,:),'linewidth',2), hold on
if Z(1) == 0
   V = -[0;Z(3);-Z(2)];
   V = V/norm(V);
else
   V = -[Z(3);0;-Z(1)];
   V = V/norm(V);
end
W = [Z(2)*V(3)-Z(3)*V(2);Z(3)*V(1)-Z(1)*V(3);Z(1)*V(2)-Z(2)*V(1)];
C2 = [s*Z,s*Z+r*V];
%plot3(C2(1,:),C2(2,:),C2(3,:),'linewidth',2,'color','k'), hold on
C3 = [s*Z,s*Z+r*W];
%plot3(C3(1,:),C3(2,:),C3(3,:),'linewidth',2,'color','g'), hold on
plot3(0,0,0,'.','markersize',12), hold on
TT = linspace(0,2*pi,N);
Kreis = s*Z*ones(1,N) + r*V*cos(TT) + r*W*sin(TT);
plot3(Kreis(1,:),Kreis(2,:),Kreis(3,:)), hold on
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
PHI = 1.8;
PSI = 0;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
P1 = s*Z + r*V*cos(PHI) + r*W*sin(PHI);
C4 = [A,P1];
plot3(C4(1,:),C4(2,:),C4(3,:),'linewidth',2,'color','r'), hold on
P2 = s*Z + r*V*cos(PSI) + r*W*sin(PSI);
C5 = [A,P2];
plot3(C5(1,:),C5(2,:),C5(3,:),'linewidth',2,'color','k'), hold on
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
TT1 = linspace(PSI,2*pi+PHI,M);
SEGM1 = s*Z*ones(1,M) + r*V*cos(TT1) + r*W*sin(TT1);
FL1 = [A,SEGM1,A];
%fill3(FL1(1,:),FL1(2,:),FL1(3,:),'g'), hold on
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%555
TT2 = linspace(PHI,PSI,M);
SEGM2 = s*Z*ones(1,M) + r*V*cos(TT2) + r*W*sin(TT2);
FL2 = [A,SEGM2,A];
fill3(FL2(1,:),FL2(2,:),FL2(3,:),'g'), hold on
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Kreis = s*Z*ones(1,N) + r*V*cos(TT) + r*W*sin(TT);
fill3(Kreis(1,:),Kreis(2,:),Kreis(3,:),'y'), hold on
plot3(Kreis(1,:),Kreis(2,:),Kreis(3,:),'linewidth',2,'color','k'), hold on

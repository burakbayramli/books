function fig0704
% Figure 7.4, bending beam with derivatives

disp(' Call first demo1, if not done ') 
load daten4 Z p e
FAC = [5e3,1e5,5e6];   % Vergroesserungsfaktoren
clf, hold on
A = koeffizienten(p,Z);
% -- Verschiebung ---------------------
X1 = [];Y1 = [];
for I = 1:size(A,2);
   TT = linspace(0,A(5,I),20);
   X = TT;
   Y = A(4,I)*X.^3 + A(3,I)*X.^2 + A(2,I).*X + A(1,I);
   Y = FAC(1)*Y;
   X = p(I) + X;
   X1 = [X1,X]; Y1 = [Y1,Y];
end
% -- 1. Ableitung der Verschiebung -------
X2 = [];Y2 = [];
for I = 1:size(A,2);
   TT = linspace(0,A(5,I),20);
   X = TT;
   Y = 3*A(4,I)*X.^2 + 2*A(3,I)*X + A(2,I);
   Y = FAC(2)*Y;
   X = p(I) + X;
   X2 = [X2,X]; Y2 = [Y2,Y];
end
%  -- 2. Ableitung der Verschiebung ------
X3 = [];Y3 = [];
for I = 1:size(A,2);
   TT = linspace(0,A(5,I),20);
   X = TT;
   Y = 6*A(4,I)*X + A(3,I);
   Y = FAC(3)*Y;
   X = p(I) + X;
   X3 = [X3,X]; Y3 = [Y3,Y];
end
plot(X1,Y1,'k','linewidth',2),hold on
plot(X2,Y2,'b--','linewidth',2),hold on
plot(X3,Y3,'r:','linewidth',2),hold on
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
legend('5e3*u','1e5*du/dx','5e6*d^2u/dx^2')
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
X = [p(1,1),p(1,size(p,2))]; Y = [0,0];
plot(X,Y,'k','linewidth',2), hold on
% -- CIRCLES --------------------
RR = 3;
for I = 1:length(p)
   circle(p(I),0,RR,'w')
end
% -- Rahmen -----------------
LU = [-10,-60];
plot(LU(1),LU(2),'w.'), hold on
RO = [310,40];
plot(RO(1),RO(2),'w.'), hold on

axis equal tight, grid off

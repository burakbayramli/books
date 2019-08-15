function bild03b(example)
% Figures for examples Beispiele 3,4,5,6 with zero bc for x = 0, x = 1
% plots Y and the scaled eigensolution obtained  
% from bifurcation equation

switch example
case 3, load daten3bcont Y U0 Parmeter
      NIVEAUS = [-0.06,-0.04,-0.02, 0.02,0.04,0.06];
case 4, load daten4bcont Y U0 Parmeter
      NIVEAUS = [-0.06,-0.04,-0.02, 0.02,0.04,0.06];
case 5, load daten5bcont Y U0 Parmeter
      NIVEAUS = [-0.06,-0.04,-0.02, 0.02,0.04,0.06];
case 6, load daten6bcont Y U0 Parmeter
      NIVEAUS = [-0.06,-0.04,-0.02, 0.02,0.04,0.06];
case 7, load daten7bcont Y U0 Parmeter
      NIVEAUS = [-0.06,-0.04,-0.02, 0.02,0.04,0.06];
case 8, load daten8bcont Y U0 Parmeter
      NIVEAUS = [-0.06,-0.04,-0.02, 0.02,0.04,0.06];
end
n = Parmeter(4); m = Parmeter(9); 
Z = reshape(Y,m,n);
U0 = reshape(U0,m,n);
% -- Augment by zero boundary values:
Z = [zeros(m,1), Z, zeros(m,1)];
U0 = [zeros(m,1), U0, zeros(m,1)];
maxz = max(max(abs(Z))); maxu = max(max(abs(U0)));

DIFF = Z - maxz*U0/maxu;

bilda = 100;
while ~ismember(bilda,[1,2,3,4])
   bilda   = input(' Which Figure? (1/2/3/4)');
end;
%bilda = 1;
clf
X = linspace(0,1,n+2); Y = linspace(0,1,n+2);
[U,V] = meshgrid(X,Y);
W1 = griddata(X,Y,Z,U,V,'cubic');
W2 = griddata(X,Y,U0,U,V,'cubic');
W3 = griddata(X,Y,DIFF,U,V,'cubic');

switch bilda
case 1
   disp(' Mesh for X (red) and U0 (black) ')
   mesh(U,V,W1,'edgecolor','r'), hold on, hidden off
   mesh(U,V,W2,'edgecolor','k')
   axis equal tight
   xlabel('x','fontsize',20)
   ylabel('y','fontsize',20)
   view(3)
case 2
   disp(' Contour for X (red) and U0 (black) ')
   contour(U,V,W1,10,'r'), hold on
   contour(U,V,W2,10,'k'), hold on
   grid off
   axis equal tight
case 3
   disp(' Mesh for X - U0 with equal max. values ')
   mesh(U,V,DIFF,'edgecolor','r'), 
   axis equal tight
   xlabel('x','fontsize',20)
   ylabel('y','fontsize',20)
   view(3)
case 4
   disp(' Contour for X - U0 with equal max. values ')
   [C,h] = contour(U,V,DIFF,10); hold on
%   [C,h] = contour(U,V,DIFF,NIVEAUS); hold on
%   clabel(C,h);
   grid off
   axis equal tight
end

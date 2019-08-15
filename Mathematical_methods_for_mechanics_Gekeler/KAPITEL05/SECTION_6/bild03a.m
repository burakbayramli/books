function bild03a
% Figures for continuation for MU after DEMO3A.M
% Beispiele 3, 4, 5, 6, 7
load datenfa Y MU UU MU0 Parmeter Beispiel
n = Parmeter(4); K = Parmeter(5);
Z = reshape(Y,n,n);
% -- Nullrandbedingungen hinzufuegen
Z = [zeros(n,1), Z, zeros(n,1)];
Z = [zeros(1,n+2);Z;zeros(1,n+2)];
U0 = UU(:,K);
U0 = reshape(U0,n,n);
U0 = [zeros(n,1), U0, zeros(n,1)];
U0 = [zeros(1,n+2);U0;zeros(1,n+2)];

bild = 100; KK = [1,2];
%while ~ismember(bild,KK)
%   bilda   = input(' Which Figure? (1/2)');
%end;
bilda = 2;
X = linspace(0,1,n+2); Y = linspace(0,1,n+2);
[U,V] = meshgrid(X,Y);
W1     = griddata(X,Y,Z,U,V,'cubic');
W2     = griddata(X,Y,U0,U,V,'cubic');
clf
switch bilda
case 1
   clf
   % --  CONTOUR  ----------------------
   hold on  % fuer flaches Bild --------
   contour(U,V,W1,10)
  % clabel(C,h,'manual');
   for I = 1:n+2
      plot(XX,YY(I)*ones(1,n+2),'.','MarkerSize',6);
   end
   grid on, axis equal tight
case 2
   % -- MESH ---------------------------
   mesh(U,V,W1,'edgecolor','r'), hold on, hidden off
   mesh(U,V,W2,'edgecolor','k')
   axis equal tight
   xlabel('x','fontsize',20)
   ylabel('y','fontsize',20)
   view(3)
end

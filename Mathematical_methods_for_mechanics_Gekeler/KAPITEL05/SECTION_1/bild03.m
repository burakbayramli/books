function bild03(Example)
% Figures for examples of Poisson equation
clf
switch Example
   case 3,load daten3 Y Parmeter UU
   case 4,load daten4 Y Parmeter UU
   case 5,load daten5 Y Parmeter UU
   case 6,load daten6 Y Parmeter UU
   case 7,load daten7 Y Parmeter UU
   case 8,load daten8 Y Parmeter UU
end
n = Parmeter(3); TYP = Parmeter(5); K = Parmeter(6);
if TYP == 1
   Z = reshape(Y,n,n);
   % -- Add zero boundary conditions
   Z = [zeros(n,1), Z, zeros(n,1)];
   Z = [zeros(1,n+2);Z;zeros(1,n+2)];
   UK = UU(:,K); UK = reshape(UK,n,n);
   UK = [zeros(n,1), UK, zeros(n,1)];
   UK = [zeros(1,n+2);UK;zeros(1,n+2)];

   X     = linspace(0,1,n+2);
   Y     = linspace(0,1,n+2);
   [U,V] = meshgrid(X,Y);
   W1     = griddata(X,Y,UK,U,V,'cubic');
   W2     = griddata(X,Y,Z,U,V,'cubic');
end
if TYP == 2;
   Z = reshape(Y,n+2,n);
   % -- Add zero boundary conditions
   Z = [zeros(n+2,1),Z,zeros(n+2,1)];
   UK = UU(:,K); UK = reshape(UK,n+2,n);
   UK = [zeros(n+2,1), UK, zeros(n+2,1)];

   X     = linspace(0,1,n+2);
   Y     = linspace(0,1,n+2);
   [U,V] = meshgrid(X,Y);
   W1     = griddata(X,Y,UK,U,V,'cubic');
   W2     = griddata(X,Y,Z,U,V,'cubic');
end
bilda = 100;
%while ~ismember(bilda,[1,2])
%   bilda   = input(' Which figure? (1/2)');
%end;
bilda = 1;
hidden off
switch bilda
case 1, % -- MESH ---------------------------
   mesh(U,V,W1,'edgecolor','y','facecolor','none','linewidth',1); hold on
 %   mesh(U,V,W1,'edgecolor','y','linewidth',1);
    mesh(U,V,W2,'edgecolor','b','erasemode','none')
   hidden off
   view(3)
   xlabel('x','fontsize',22)
   ylabel('y','fontsize',22)
   zlabel('z','fontsize',22)

case 2, % --  CONTOUR  ----------------------
   clf, hold on
   contour(U,V,W,10), hold on
  % [C,h] = contour(U,V,W,10);
  % clabel(C,h,'manual');
   for I = 1:n+2
      plot(X,Y(I)*ones(1,n+2),'.','MarkerSize',6);
   end
   grid on
   axis equal tight
end

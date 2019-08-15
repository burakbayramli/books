function bild03f
% BILD03 for continuation resp. MU
load datenf Y Parmeter Beispiel
bilda = 100;
%while ~ismember(bilda,[1,2])
%   bilda = input(' Which figure? (1/2)');
%end;
bilda = 1;
n = Parmeter(3); TYP = Parmeter(5);
switch TYP
case 1 % Nullrandbedingungen
   Z = reshape(Y,n,n);
   % -- Nullrandbedingungen hinzufuegen
   Z = [zeros(n,1), Z, zeros(n,1)];
   Z = [zeros(1,n+2);Z;zeros(1,n+2)];
   X     = linspace(0,1,n+2);
   Y     = linspace(0,1,n+2);
   [U,V] = meshgrid(X,Y);
   W     = griddata(X,Y,Z,U,V,'cubic');
case 2 % Gemischte Randbedingungen
   Z = reshape(Y,n+2,n);
   % -- Nullrandbedingungen hinzufuegen
   Z = [zeros(n+2,1),Z,zeros(n+2,1)];
   X     = linspace(0,1,n+2);
   Y     = linspace(0,1,n+2);
   [U,V] = meshgrid(X,Y);
   W     = griddata(X,Y,Z,U,V,'cubic');
end
switch bilda
case 1,% -- MESH --------------------
   %clf
   mesh(U,V,W), hold on
   switch Beispiel
     case 3, axis([0 1 0 1 -1 1])
     case 4, axis([0 1 0 1 -3 3])
     case 5, axis([0 1 0 1 -4 4])
     case 6, axis([0 1 0 1 -1 1])
     case 7, axis([0 1 0 1 -1 1])
     case 8, axis([0 1 0 1 -0.4 0.4])
   end
   view(3)
case 2 % -- CONTOUR -----------------
   clf, hold on
   contour(U,V,W,10), hold on
  % [C,h] = contour(U,V,W,10);
  % clabel(C,h,'manual');
   grid on
   axis equal tight
end

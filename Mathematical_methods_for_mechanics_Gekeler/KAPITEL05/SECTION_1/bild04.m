function bild04
% Eigenfunctions 
clf
load daten UU MU0
n = 24;
U1 = UU(:,1);
%U1 = - U1;
Z = reshape(U1,n,n+2);
% -- Nullrandbedingungen hinzufuegen
Z = [zeros(n,1), Z, zeros(n,1)];
Z = [zeros(1,n+2);Z;zeros(1,n+2)];
done      = 0;
KK        = [1,2];
%while ~done
%   bild   = input(' Welches Bild? (1/2)');
%   done = ismember(bild,KK);
%end;
bild = 2;
clf
X     = linspace(0,1,n+2);
Y     = linspace(0,1,n+2);
Z1    = zeros(1,n+2);
[U,V] = meshgrid(X,Y);
W     = griddata(X,Y,Z,U,V,'cubic');
if bild == 1
   % --  CONTOUR  ----------------------
   hold on  % fuer flaches Bild --------
   contour(U,V,W,10), hold on
  % [C,h] = contour(U,V,W,10);
  % clabel(C,h,'manual');
   for I = 1:n+2
      plot(X,Y(I)*ones(1,n+2),'.','MarkerSize',6);
   end
   grid on
   axis equal tight
end
if bild == 2
   % -- MESH ---------------------------
   mesh(U,V,W)
   view(3)
end

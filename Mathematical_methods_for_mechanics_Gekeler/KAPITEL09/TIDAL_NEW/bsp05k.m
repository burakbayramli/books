function H = bsp05k(p);
% sea bed for bsp05.m
% 2/3 < r < 2;
X = p(1,:); Y = p(2,:);
H = zeros(1,length(X));
R = sqrt(X.^2 + Y.^2);

J = find(R > 2);
%H(J) = NaN;
K = find(R < 2/3);
%H(K) = NaN;
K = find(R > 2/3);
J = find(R < 2);
L = intersect(J,K);
Z = (R(L) - 4/3)/(2/3);
H(L) = sqrt(1 - Z.^2);
clf
   xlin  = linspace(min(X),max(X),60);
   ylin  = linspace(min(Y),max(Y),60);
   [U,V] = meshgrid(xlin,ylin);
   W     = griddata(X,Y,H,U,V,'cubic');
   [C,h] = contour(U,V,W,8,'k');
  % mesh(U,V,W,'erasemode','none'), hold on
    
%view(20,10);
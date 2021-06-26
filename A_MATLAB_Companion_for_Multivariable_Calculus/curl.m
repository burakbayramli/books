
%
%             Function mfile     curl.m
%
%   The call is   curl(u,v,corners)   where u = u(x,y) and v = v(x,y)
%   are the vector field F = [u,v].  The mfile calculates the curl
%   with centered differences. The vector field is displayed using 
%   quiver , and a pcolor map is made of the curl, showing by
%   color the values of the curl.





function out = curl(u,v, corners)
a = corners(1); b = corners(2); c = corners(3); d = corners(4);

x = linspace(a,b,51); y = linspace(c,d,51);

[X,Y] = meshgrid(x,y);
h = 10^(-6);

C1 = (feval(v,X+h, Y) - feval(v, X-h,Y))/(2*h);
C2 = (feval(u,X, Y+h) - feval(u,X, Y-h))/(2*h);
C = C1 - C2;
close
pcolor(X,Y,C); map = jet;  
myjet = map(33:64,:);
colormap(myjet);
shading interp 
colorbar
hold on

xx = linspace(a,b, 11); yy = linspace(c,d,11);

[XX,YY] = meshgrid(xx,yy);

U = feval(u,XX,YY); V = feval(v, XX,YY);
quiver(XX,YY,U,V, 'k');
axis image
hold off

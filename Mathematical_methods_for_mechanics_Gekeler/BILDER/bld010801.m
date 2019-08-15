% BILD006
clf
U = linspace(-0.05,0.05,40);
V = linspace(-0.05,0.05,40);
[X,Y] = meshgrid(U,V);
%Z = X.*Y.*Y./(X.*X + Y.*Y.*Y.*Y + 1.0E-4);
%mesh(X,Y,Z)
Z = X.*Y.*Y./(X.*X + Y.*Y.*Y.*Y + eps);

[m,n] = size(Z);
U = 2*ones(m,n);
V = min(Z,U);
W = -2*ones(m,n);
Z = max(V,W);
mesh(X,Y,Z)
%axis off
%title('f(x,y), |f(x,y)| \leq 2','fontsize',22)
xlabel('x','fontsize',18)
ylabel('y','fontsize',18)

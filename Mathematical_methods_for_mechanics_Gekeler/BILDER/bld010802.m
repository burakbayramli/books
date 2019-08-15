% BILD007
clf
U = linspace(-0.05,0.05,40);
V = linspace(-0.05,0.05,40);
[X,Y] = meshgrid(U,V);
%Z = Y.*(X.*X + Y.*Y).^(3/2)./((X.*X - Y.*Y).^2 + Y.*Y + 1.0E-4);
%mesh(X,Y,Z)
Z = Y.*(X.*X + Y.*Y).^(3/2)./((X.*X - Y.*Y).^2 + Y.*Y + eps);
[m,n] = size(Z);
U = ones(m,n);
V = min(Z,2*U);
Z = max(V,-2*U);
mesh(X,Y,Z)

%axis off
%title('h(x,y), |h(x,y)| \leq 2)','fontsize',22)
xlabel('x','fontsize',18)
ylabel('y','fontsize',18)

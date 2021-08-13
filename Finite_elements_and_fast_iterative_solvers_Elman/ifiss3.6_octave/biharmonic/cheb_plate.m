function [xx,yy,uu] = cheb_plate(N)
%CHEB_PLATE   from Trefethen Approximation Theory book p.148
%   [xx,yy,uu] = cheb_plate(N);
%   input
%          N  number of collocation points
%   output
%         xx,yy   chebyshev mesh points
%         uu      solution matrix
% construct spectral approximation to biharmonic operator
[D,x] = cheb(N); D2 = D^2; D2 = D2(2:N,2:N);
S = diag([0; 1./(1-x(2:N).^2); 0]);
D4 = (diag(1 -x.^2)*D^4 - 8*diag(x)*D^3 - 12*D^2)*S;
D4 = D4(2:N,2:N); I = eye(N-1);
L = kron(I,D4) + kron(D4,I) + 2*kron(D2,I)*kron(I,D2);
DXY = kron(D2,I)*kron(I,D2);
%
% solve uniform loaded plate problem
[nn,nm] = size(L);
f=ones(nm,1);
u = L\f;
uu = zeros(N+1,N+1);
uu(2:N,2:N) = reshape(u,N-1,N-1);
% compute the twisting moment
m = DXY*u;
mm = reshape(m,N-1,N-1)
%
% plot the result
[xx,yy] = meshgrid(x,x);
[xm,ym] = meshgrid(x(2:N),x(2:N));
figure(98)
subplot(122)
mesh(xm,ym,mm), axis square
title('height of twisting moment')
subplot(121)
mesh(xx,yy,uu), axis square
title('height of displacement')
%%  solution data
nvtx=(N-1).^2; k1=(nvtx+1)/2;
fprintf('\ncomputed solution values:')
fprintf('\nu = %9.7f, u_xy = %9.6e \n\n',u(k1),m(k1))
figure(99)
contour(xm,ym,mm,30), axis square
title('contours of the twisting moment')
return

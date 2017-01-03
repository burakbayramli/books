function mit18086_poisson
%MIT18086_POISSON
%    Sets up and solves a 1d, 2d and 3d Poisson problem
%    on domain [0 1]^dim, with homogeneous Dirichlet b.c.
%    everywhere and constant right hand side.
%    Uses successive Kronecker products to construct
%    sparse system matrices

% 03/2007 by Benjamin Seibold
%            http://www-math.mit.edu/~seibold/
% Feel free to modify for teaching and learning.

n = 10;
h = 1/(n+1);
x = h:h:1-h;

clf
K1D = spdiags(ones(n,1)*[-1 2 -1],-1:1,n,n);  % 1d Poisson matrix
subplot(2,3,4), spy(K1D)

I1D = speye(size(K1D));                       % 1d identity matrix
K2D = kron(K1D,I1D)+kron(I1D,K1D);            % 2d Poisson matrix
subplot(2,3,5), spy(K2D)

I2D = speye(size(K2D));                       % 2d identity matrix
K3D = kron(K2D,I1D)+kron(I2D,K1D);            % 3d Poisson matrix
subplot(2,3,6), spy(K3D)

f1D = h^2*ones(n,1);                          % 1d right hand side
u1D = K1D\f1D;
subplot(2,3,1), plot(x,u1D,'.-')
title('1d poisson equation')

f2D = h^2*ones(n^2,1);                        % 2d right hand side
u2D = K2D\f2D;
subplot(2,3,2), surf(x,x,reshape(u2D,n,n))
title('2d poisson equation')

f3D = h^2*ones(n^3,1);                        % 3d right hand side
u3D = K3D\f3D;
subplot(2,3,3), slice(x,x,x,reshape(u3D,n,n,n),[1 2]/3,.5,.5)
title('3d poisson equation')

function mit18086_cg
%MIT18086_CG
%    Sets up a 2d Poisson problem and solves the arising
%    linear system by a conjugate gradient method.
%    The error over iteration steps is plotted.

% 04/2007 by Benjamin Seibold
%            http://www-math.mit.edu/~seibold/
% Feel free to modify for teaching and learning.

m = 30;                                    % size of geometry

D = numgrid('L',m);
A = delsq(D);
n = size(A,1);
b = rand(n,1);
x_true = A\b;
e = zeros(n,1);

d = b;                             % initial search direction
r = b;                             % initial residual
x = b*0;                           % initial solution
r2 = r'*r;
for i = 1:n
   Ad = A*d;                       % apply the matrix A
   alpha = r2/(d'*Ad);             % a first scalar product
   x = x+alpha*d;                  % update solution
   r = r-alpha*Ad;                 % update residual
   r2old = r2;
   r2 = r'*r;                      % a second scalar product
   beta = r2/r2old;               
   d = r+beta*d;                   % new search direction
   e(i) = norm(x-x_true);
end
clf
subplot(2,2,1), spy(D), title('domain shape')
subplot(2,2,2), spy(A), title('system matrix')
subplot(2,2,3:4), semilogy(1:n,e,'r.-'), grid on
title('error over cg iteration')

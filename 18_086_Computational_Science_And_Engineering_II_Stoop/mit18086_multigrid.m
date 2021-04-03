function mit18086_multigrid
%MIT18086_MULTIGRID
%    Sets up a 1d Poisson test problem and solves it by multigrid.
%    Method uses twogrid recursively using Gauss-Seidel for smoothing
%    and elimination to solve at coarsest level (n<4).
%    Number of pre- and postsmoothing and coarse grid iteration steps
%    can be prescribed.

% 04/2007 by Benjamin Seibold
%            http://www-math.mit.edu/~seibold/
% Feel free to modify for teaching and learning.
%---------------------------------------------------------------------
levels = 5;                                          % size of problem
nu1 = 2;                           % number of presmoothing iterations
nu2 = 2;                          % number of postsmoothing iterations
gamma = 2;   % number of coarse grid iterations (1=V-cycle, 2=W-cycle)
%---------------------------------------------------------------------
n = 2^(levels+2)-1;                            % number of grid points
h = 1/(n+1);
x = (h:h:(1-h))';
f = pi^2*(sin(pi*x)+4^2*sin(pi*4*x)+9^2*sin(pi*9*x));
A = spdiags(ones(n,1)*[-1 2 -1],-1:1,n,n);
b = f*h^2;
uc = A\b;

global t level, t = 0; level = levels;
clf, subplot(2,2,3:4), hold on
u = twogrid(A,b,nu1,nu2,gamma);
hold off, axis tight
subplot(2,2,1), plot(x,u,'b.-',x,uc,'r.--')
title('correct solution and multigrid approximation')
subplot(2,2,2), plot(x,uc-u,'r.-')
title('error')

%=====================================================================

function x = twogrid(A,b,nu1,nu2,gamma,x0)
%TWOGRID
%    Recursive twogrid cycle for 1d Poisson problem.
%    nu1 = number of presmoothing iterations (Gauss-Seidel)
%    nu2 = number of postsmoothing iterations (Gauss-Seidel)
%    gamma = number of coarse grid iterations (1=V-cycle, 2=W-cycle)
%    x0 = starting vector (0 if not prescribed)
global t level
n = length(b);
if n<4
   x = A\b;                          % solve exactly at coarsest level
else
   G = speye(n)-tril(A)\A; cG = tril(A)\b;       % create Gauss-Seidel
   I = spdiags(ones(n-2,1)*[1 2 1],-2:0,n,n-2); % create interpolation
   I = I(:,1:2:end)/2; R = I'/2;            % and restriction matrices
   if nargin<6, x = b*0; else, x = x0; end           % starting vector
   for i = 1:nu1, x = G*x+cG; end                       % presmoothing
   r = b-A*x;                                       % compute residual
   rh = R*r;                        % restrict residual to coarse grid
   t = t+1; level = level-1; plot([t-1 t],[level+1 level],'bo-')
   eh = rh*0;                                        % starting vector
   for i = 1:gamma
      eh = twogrid(R*A*I,rh,nu1,nu2,gamma,eh); % coarse grid iteration
   end
   e = I*eh;                                       % interpolate error
   t = t+1; level = level+1; plot([t-1 t],[level-1 level],'bo-')
   x = x+e;                                          % update solution
   for i = 1:nu2, x = G*x+cG; end                      % postsmoothing
end

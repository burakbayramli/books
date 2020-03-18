% Updated 03/18/08, then 10/10/08
%Primal-Dual Newton's method
%-------------------------------------------------------------------------
% Input variables
%-------------------------------------------------------------------------
% f:        noisy image
% u:        Initial guess for the desired primal variable u 
% w:        Initial guess for the desired dual variable w
% lbd:      Constant. Objective energy function = TV + (lbd/2)||u-z||^2
% NIT:      Maximum number of iterations
% tau:      Step length (dual)
% theta:    Step length (primal)
%--------------------------------------------------------------------------
%-------------------------------------------------------------------------
% Output variables
%-------------------------------------------------------------------------
% u:            Desired numerical solution - primal variable
% w= (w1,w2):   Dual variable. Numerical Solution, w=gu/|gu|
% Primal:       Primal energy
% Dual:         Dual energy
% DGap:         Duality Gap = Primal energy - Dual energy
% TimeCost:     CPU time cost

function [u, w, Primal, Dual, Dgap, TimeCost, itr, beta] = ...
      CGM2d(u, f, lbd, beta, NIT, GapTol, verbose)

n=length(f);            %Assume a square image n*n;
C=sum(sum(f.^2));
N=n^2;
shuf=reshape([1:N ; N+1:2*N], 2*N,1);  
ix=1:2:2*N;
iy=2:2:2*N;

% A1 is the 1-dim operator of gradient(forward difference with Numan BC at grid n)
% A1'*v=v_x
% A is the same as the matrix A in the CGM paper: A'*u = Gradient of u
% A'*u = grad of u written in a column vector =((u_x,u_y)_{i,j})=[ux11; uy11; ux12; uy12;...; uxnn;uynn]
% A*w = - div w
e1=ones(n, 1);
A1=spdiags([e1 -e1], [-1 0], n, n);
A1(n, n)=0;       
A=[ kron(speye(n), A1) kron(A1, speye(n))];
A=A(:, shuf);    
I2N=speye(2*N);
IN=speye(N);

eta=0.999;
alp = 1/lbd;

%%%%%%%%%%%%%%%%%%%%%%%%%% Initialization %%%%%%%%%%%%%%%%%%%%%%%%%%
f = reshape(f, N, 1 );
u = reshape(u, N, 1 );
w = zeros(2*N,1);
grad_u=A'*u;
gu2= grad_u(ix).^2+grad_u(iy).^2;  
Primal(1)     = sum(sqrt(gu2)) + (lbd/2)*sum((u-f).^2);
Dual(1)       = (lbd/2)*(C - sum((f-alp*A*w).^2));
Dgap(1)       = (Primal(1)-Dual(1)) / (abs(Primal(1))+abs(Dual(1)));
DgapAbs(1)    = (Primal(1)-Dual(1));
TimeCost(1)   = 0;
t0            = cputime;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

for itr=1:NIT  
  
  if verbose
    fprintf(' CGM2d itr %3d: relative Dgap = %7.3e\n', itr, Dgap(itr));
  end
  
  norm_grad_u=kron(sqrt(gu2+beta), [1;1]);
  
  %Note here * and ./ are not associative:  A*(B./C) ~= A*B./C
  rh=alp*A*(grad_u./norm_grad_u)+u-f;     %right-hand side of equation (36) in the paper   
   
  Grad_u=[ spdiags(grad_u(ix), 0, N, N);
           spdiags(grad_u(iy), 0, N, N) ];
  Grad_u=Grad_u(shuf, :);
   
  W=[ spdiags(w(ix), 0, N, N);
      spdiags(w(iy), 0, N, N) ];
  W=W(shuf, :);
  D=spdiags(1./norm_grad_u, 0, 2*N, 2*N);
  M=alp*A*D*(I2N-0.5*D*(Grad_u*W'+W*Grad_u'))*A' + IN;
  
  %use direct solver 
  % PCG has worse convergence results and takes more time
  %du = -pcg(M,rh, 1.e-6, 100);
  du=-M\rh;   
  dw=grad_u./norm_grad_u - w + D*(I2N-0.5*D*(Grad_u*W'+W*Grad_u'))*A'*du;
  
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  % Step Length Rule - Only Necessary for Relatively Small Beta
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  % |w+sdw|^2 = dwdw*s^2+2wdw*s+ww = 1  ==> solve quadratic equation for s
  % then take the minimum over all i,j.
  ww=w(ix).^2+w(iy).^2;
  wdw=w(ix).*dw(ix)+w(iy).*dw(iy);
  dwdw=dw(ix).^2+dw(iy).^2;
  
  %Subscript indices could be logicals. It's used here to exclude the
  %points where |dw|=0; 
  idx_dw= dwdw > 0;
  
  ww=ww(idx_dw);
  wdw=wdw(idx_dw);
  dwdw=dwdw(idx_dw);
  s_w=min((-wdw+sqrt(wdw.^2+dwdw.*(1-ww)))./dwdw);
  s_w=eta*min(s_w, 1);
  
  u= u +du;
  w= w+ s_w*dw;
  
  grad_u=A'*u;
  gu2= grad_u(ix).^2+grad_u(iy).^2;  
  Primal(itr+1)     = sum(sqrt(gu2)) + (lbd/2)*sum((u-f).^2);
  Dual(itr+1)       = (lbd/2)*(C - sum((f-alp*A*w).^2));
  DgapAbs(itr+1)    = (Primal(itr+1)-Dual(itr+1));
  Dgap(itr+1)       = (Primal(itr+1)-Dual(itr+1)) / ...
      (abs(Primal(itr+1)) + abs(Dual(itr+1)));
  TimeCost(itr+1)   = cputime - t0;
  if (Dgap(itr+1) <= GapTol)
     break;
  end
  
  beta = beta*(DgapAbs(itr+1)/DgapAbs(itr))^2;
end
 
u=reshape(u, n, n);
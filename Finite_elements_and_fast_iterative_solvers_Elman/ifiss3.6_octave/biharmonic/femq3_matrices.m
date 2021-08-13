function [a,r,f] = femq3_matrices(xy,ev)
%FEMQ3_MATRICES  fast bicubic coefficient matrix generator
%   [A,Q,f] = femq3_matrices(xy,ev);
%   input
%          xy         nodal coordinate vector  
%          ev         element mapping matrix
%   output
%          A          stiffness matrix
%          Q          mass matrix 
%          f          rhs vector
%
%   Analytic integration formulas to avoid quadrature.
%   Constant load function is assumed
%   Natural boundary conditions apply. Essential conditions
%   must be explicitly enforced by calling function clampedbc.
%   IFISS function: DJS; 14 August 2018.
% Copyright (c) 2018 D.J. Silvester, P. Nadukandi

% check the grid
fprintf('setting up Q3z biharmonic matrices... ')
x=xy(:,1); y=xy(:,2);
lx=max(x)-min(x); ly=max(y)-min(y);
dx=diff(x); dy=diff(y); kx=find(diff(x)>0);  ky=find(diff(y)>0);
hx=max(dx(kx)); hxx=min(dx(kx)); hy=max(dy(ky));  hyy=min(dy(ky));
if (hx~=hy) | (hx~=hxx),
error('Oops.. grid is not uniform.. check data'),
else, h=hx; end
fprintf('h is %9.6f \n',h)
nvtx=length(x); ndof=4*nvtx;
nel=length(ev(:,1));
lx=max(x)-min(x); ly=max(y)-min(y);

% generate element matrices
h = hx;
NN = [156, 54, 22*h, -13*h; 54, 156, 13*h, -22*h; 22*h, 13*h, 4*h*h,-3*h*h;...
      -13*h, -22*h, -3*h*h, 4*h*h].*(h/420);
LL = [6,-6,3*h,3*h;-6,6,-3*h,-3*h;3*h,-3*h,2*h*h,h*h;3*h,-3*h,h*h,2*h*h]*2/(h*h*h);
LM = [-36,36,-3*h,-3*h;36,-36,3*h,3*h;-33*h,3*h,-4*h*h,h*h; ...
      -3*h,33*h,h*h,-4*h*h]/(30*h);
xId = [1,2,2,1,3,4,4,3,1,2,2,1,3,4,4,3];
yId = [1,1,2,2,1,1,2,2,3,3,4,4,3,3,4,4];
KK = LL(xId,xId).*NN(yId,yId) + LM(xId,xId)'.*LM(yId,yId) + ...
     LM(xId,xId).*LM(yId,yId)' + NN(xId,xId).*LL(yId,yId);
MM = NN(xId,xId).*NN(yId,yId);


% generate right-hand side for unit load function
ff = [36;36;36;36;6*h;-6*h;-6*h;6*h;6*h;6*h;-6*h;-6*h;h*h;-h*h;h*h;-h*h]*h*h/144;

% create mapping vector
      mv=[ev,ev+nvtx,ev+2*nvtx,ev+3*nvtx];

% initialise global matrices
a = sparse(ndof,ndof);
r = sparse(ndof,ndof);
f = zeros(ndof,1);
% perform assembly of global matrix  and source vector
      for krow=1:16
      nrow= mv(:,krow);
          for kcol=1:16
		  ncol= mv(:,kcol);
          a = a + sparse(nrow,ncol,KK(krow,kcol),ndof,ndof);
          r = r + sparse(nrow,ncol,MM(krow,kcol),ndof,ndof);
          end
      f(nrow,1) = f(nrow,1) + ff(krow);
      end
return


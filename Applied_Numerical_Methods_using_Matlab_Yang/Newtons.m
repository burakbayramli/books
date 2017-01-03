%newtons.m  to solve a set of nonlinear eqs  f1(x)=0, f2(x)=0,..
function [x,fx,xx]= newtons(f,x0,TolX,MaxIter,varargin)
%input:  f   = a 1st-order vector ftn equivalent to a set of equations
%        x0  = the initial guess of the solution
%        TolX = the upper limit of |x(k)-x(k-1)|
%        MaxIter= the maximum # of iteration
%output: x   = the point which the algorithm has reached
%        fx  = f(x(last))
%        xx  = the history of x
h=1e-5; TolFun=eps; EPS=1e-6;
fx=feval(f,x0,varargin{:}); 
Nf=length(fx); Nx=length(x0);
if Nf~=Nx, error('Incompatible dimensions of f and x0!'); end
if nargin<4, MaxIter=100; end
if nargin<3, TolX=EPS; end
xx(1,:)=x0(:).';
%fx0= norm(fx);
for k=1: MaxIter
  J=jacob(f,xx(k,:),h,varargin{:});
  if rank(J)<Nx
     k=k-1; fprintf('Warning: Jacobian singular! with det(J)=%12.6e\n',det(J)); break; 
  else 
     dx= -J\fx(:); %-[dfdx]^-1*fx;
  end
  %for l=1: 3 %damping to avoid divergence %(2)
  %dx= dx/2;                               %(3)
  xx(k+1,:)= xx(k,:)+dx.';
  fx= feval(f,xx(k+1,:),varargin{:}); fxn=norm(fx);
  % if fxn<fx0,  break;  end
  %end
  if fxn<TolFun|norm(dx)<TolX, break; end
  %fx0= fxn;
end
x= xx(k+1,:);
if k==MaxIter 
  fprintf('Do not rely on this, though the best in %d iterations\n',MaxIter) 
end
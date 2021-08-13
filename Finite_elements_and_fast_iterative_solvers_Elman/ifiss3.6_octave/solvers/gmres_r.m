function [x, flag, total_iters, error] = gmres_r(aparams,mparams, b, params, x0)
%GMRES_R GMRES iteration with right preconditioning 
%   [x, flag, total_iters, error] = gmres_r(aparams,mparams, b, params, x0)
%   input
%          aparams      structure defining coefficient matrix
%          mparams      structure defining preconditioning matrix
%          b            right-hand side vector
%          params       three-dimensional vector to control iteration
%            params(1) = relative residual reduction factor
%            params(2) = max number of iterations
%            params(3) (Optional) = reorthogonalization method
%                   1 -- Brown/Hindmarsh condition (default)
%                   2 -- Never reorthogonalize (not recommended)
%                   3 -- Always reorthogonalize (not cheap!)
%          x0           initial iterate
%   output
%          x            computed (approximate) solution
%          flag         convergence flag
%                   0 for convergence to params(1) within params(2) iterations
%                   1 if iterated params(2) times but did not converge to params(1)
%          total_iters  total iteration count
%          error        vector of residual norms of iterates
%   IFISS function: HCE; 15 March 2005.


%Modification of gmres.m written by Tim Kelley.
%Copyright 1994 C. T. Kelley.  
%Reproduced and distributed with permission of the copyright holder.
%Modifications to handle IFISS data structures identified in line.

%function [x, flag, error, total_iters] = gmres(x0, b, atv, params)
% GMRES linear equation solver
% Implementation following Saad-Schultz
%
% C. T. Kelley, July 10, 1994
%
% This code comes with no guarantee or warranty of any kind.
%
% function [x, flag, error, total_iters] = gmres(x0, b, atv, params)
%
%
% Input: x0 = initial iterate
%        b = right hand side
%        atv, a matrix-vector product routine
%            atv must return Ax when x is input
%            the format for atv is
%            function ax = atv(x)
%            Note that for GMRES we incorporate any 
%            preconditioning into the atv routine.
%        HE modification:  pass matrix vector product via aparams
%                          pass preconditioning operation via mparams
%        params = three dimensional vector to control iteration
%            params(1) = relative residual reduction factor
%            params(2) = max number of iterations
%            params(3) (Optional) = reorthogonalization method
%                   1 -- Brown/Hindmarsh condition (default)
%                   2 -- Never reorthogonalize (not recommended)
%                   3 -- Always reorthogonalize (not cheap!)
%
% Output: x=solution
%         flag = 0 for convergence to params(1) within params(2) iterations
%              = 1 if iterated params(2) times but did not converge to params(1)  
%         error = vector of residual norms for the history of
%            the iteration
%         total_iters = number of iterations
%
% Requires givapp.m

% Names of mvp and preconditioning routines
afun = aparams.Afun;
mfun = mparams.Mfun;

%
% initialization
%
n=length(b);
errtol=params(1);
kmax=params(2);
reorth=1;
if length(params) == 3
    reorth=params(3);
end
x=x0;
flag=0;
%
%
h=zeros(kmax);
v=zeros(n,kmax);
c=zeros(kmax+1,1);
s=zeros(kmax+1,1);
if norm(x) ~=0
%  r = b-feval(atv,x);
   r = b-feval(afun,x,aparams);
else
   r = b;
end
rho=norm(r);
g=rho*eye(kmax+1,1);
errtol=errtol*norm(b);
error=[];
%
% test for termination on entry
%
error=[error,rho];
total_iters=0;       %fprintf('  %5i     %8.4f \n',total_iters,log10(rho));
if(rho < errtol) 
    return
end
%
v(:,1)=r/rho;
beta=rho;
k=0;
%
% GMRES iteration
%
while((rho > errtol) & (k < kmax))
    k=k+1;
%   v(:,k+1)=feval(atv,v(:,k));
%% Modification by HCE, 15 Oct. 2004: adapt for right preconditioning
    v(:,k+1) = feval(afun, feval(mfun,v(:,k),aparams,mparams), aparams);
    normav=norm(v(:,k+1));
%
% Modified Gram-Schmidt
%
    for j=1:k
        h(j,k)=v(:,j)'*v(:,k+1);            % h(j,k)=v(:,k+1)'*v(:,j);
        v(:,k+1)=v(:,k+1)-h(j,k)*v(:,j);
    end
    h(k+1,k)=norm(v(:,k+1));
    normav2=h(k+1,k);
%
% Reorthogonalize?
%
if  (reorth == 1 & normav + .001*normav2 == normav) | reorth ==  3
    for j=1:k
        hr=v(:,j)'*v(:,k+1);                 % hr=v(:,k+1)'*v(:,j);
        h(j,k)=h(j,k)+hr;
        v(:,k+1)=v(:,k+1)-hr*v(:,j);
    end
    h(k+1,k)=norm(v(:,k+1));
end
%
%   watch out for happy breakdown 
%
    if(h(k+1,k) ~= 0)
         v(:,k+1)=v(:,k+1)/h(k+1,k);
    end
%
%   Form and store the information for the new Givens rotation
%
    if k > 1
        h(1:k,k)=givapp(c(1:k-1),s(1:k-1),h(1:k,k),k-1);
    end
    nu=norm(h(k:k+1,k));
    if nu~=0
        c(k)=conj(h(k,k)/nu);             %   c(k)=h(k,k)/nu;  % Change 6/3/97
        s(k)=-h(k+1,k)/nu; 
        h(k,k)=c(k)*h(k,k)-s(k)*h(k+1,k);
        h(k+1,k)=0;
        g(k:k+1)=givapp(c(k),s(k),g(k:k+1),1);
    end
%
% Update the residual norm
%
    rho=abs(g(k+1));         %fprintf('  %5i     %8.4f \n',k,log10(rho));
    error=[error,rho];
end
%
% At this point either k > kmax or rho < errtol.
% It's time to compute x and leave.
%
% set flag first
if rho<errtol, flag=0; else flag=1; end

y=h(1:k,1:k)\g(1:k);
total_iters=k;

%x = x0 + v(1:n,1:k)*y;
%% Modification by HCE, 15 Oct. 2005: adapt for right preconditioning
x = x0 + feval(mfun, v(1:n,1:k)*y, aparams,mparams);

error = error';


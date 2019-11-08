function [x,histout,costdata] = gradproj(x0,f,up,low,tol,maxit)
%
% C. T. Kelley, June 11, 1998
%
% This code comes with no guarantee or warranty of any kind.
%
% function [x,histout,costdata] = gradproj(x0,f,up,low,tol,maxit)
%
% gradient projection with Armijo rule, simple linesearch 
% 
%
% Input: x0 = initial iterate
%        f = objective function,
%            the calling sequence for f should be
%            [fout,gout]=f(x) where fout=f(x) is a scalar
%              and gout = grad f(x) is a COLUMN vector
%        up = vector of upper bounds
%        low = vector of lower bounds
%        tol = termination criterion norm(grad) < tol
%              optional, default = 1.d-6
%        maxit = maximum iterations (optional) default = 1000
%
% Output: x = solution
%         histout = iteration history   
%             Each row of histout is
%   [norm(grad), f, number of step length reductions, iteration count,
%            relative size of active set]
%         costdata = [num f, num grad, num hess] (for steep, num hess=0)
%
% 
if nargin < 4
error(' gradproj requires bounds ');
end
xc=x0; ndim=length(up); kku=zeros(ndim,1); kkl=zeros(ndim,1);
for i=1:ndim
        kku(i)=up(i); kkl(i)=low(i);
	if kkl(i) > kku(i)
        error(' lower bound exceeds upper bound')
        end
end
%
% put initial iterate in feasible set
%
if norm(xc - kk_proj(xc,kku,kkl)) > 0
      disp(' initial iterate not feasibile ');
      xc=kk_proj(xc,kku,kkl);
end
alp=1.d-4;
if nargin < 6
maxit=1000; 
end
if nargin < 5
tol=1.d-6;
end
itc=1; 
[fc,gc]=feval(f,xc);
numf=1; numg=1; numh=0;
ithist=zeros(maxit,5);
xt=kk_proj(xc - gc,kku,kkl);
pgc=xc - kk_proj(xc - gc,kku,kkl);
ia=0; for i=1:ndim; if(xc(i)==kku(i) | xc(i)==kkl(i)) ia=ia+1; end; end;
ithist(1,5)=ia/ndim;
ithist(1,1)=norm(pgc); ithist(1,2) = fc; ithist(1,4)=itc-1; ithist(1,3)=0; 
while(norm(pgc) > tol & itc <= maxit)
        lambda=1;
        xt=kk_proj(xc-lambda*gc,kku,kkl); ft=feval(f,xt);
        numf=numf+1;
	iarm=0; itc=itc+1;
        pl=xc - xt; fgoal=fc-(pl'*pl)*(alp/lambda);
%
%       simple line search
%
        q0=fc; qp0=-gc'*gc; qc=ft;
	while(ft > fgoal)
                lambda=lambda*.1;
		iarm=iarm+1;
		xt=kk_proj(xc-lambda*gc,kku,kkl);
                pl=xc-xt;
		ft=feval(f,xt); numf = numf+1;
		if(iarm > 10) 
		disp(' Armijo error in gradient projection')
                histout=ithist(1:itc,:); costdata=[numf, numg, numh];
		return; end
                fgoal=fc-(pl'*pl)*(alp/lambda);
	end
	xc=xt; [fc,gc]=feval(f,xc); numf=numf+1; numg=numg+1;
        pgc=xc-kk_proj(xc-gc,kku,kkl); 
	ithist(itc,1)=norm(pgc); ithist(itc,2) = fc; 
	ithist(itc,4)=itc-1; ithist(itc,3)=iarm;
ia=0; for i=1:ndim; if(xc(i)==kku(i) | xc(i)==kkl(i)) ia=ia+1; end; end;
ithist(itc,5)=ia/ndim;
end
x=xc; 
histout=ithist(1:itc,:); costdata=[numf, numg, numh];
%
% projection onto active set
%
function px = kk_proj(x,kku,kkl)
ndim=length(x);
px=zeros(ndim,1);
px=min(kku,x); 
px=max(kkl,px);

function [x,histout,costdata] = steep(x0,f,tol,maxit)
%
% C. T. Kelley, Dec 20, 1996
%
% This code comes with no guarantee or warranty of any kind.
%
% function [x,histout,costdata] = steep(x0,f,tol,maxit)
%
% steepest descent with Armijo rule, polynomial linesearch 
% 
%
% Input: x0 = initial iterate
%        f = objective function,
%            the calling sequence for f should be
%            [fout,gout]=f(x) where fout=f(x) is a scalar
%              and gout = grad f(x) is a COLUMN vector
%        tol = termination criterion norm(grad) < tol
%              optional, default = 1.d-6
%        maxit = maximum iterations (optional) default = 1000
%
% Output: x = solution
%         histout = iteration history   
%             Each row of histout is
%            [norm(grad), f, number of step length reductions, iteration count]
%         costdata = [num f, num grad, num hess] (for steep, num hess=0)
%
% Requires: polymod.m
%
% linesearch parms
% 
bhigh=.5; blow=.1;
%
%
alp=1.d-4;
if nargin < 4
maxit=1000; 
end
if nargin < 3
tol=1.d-6;
end
itc=1; xc=x0; 
[fc,gc]=feval(f,xc);
numf=1; numg=1; numh=0;
ithist=zeros(maxit,4);
ithist(1,1)=norm(gc); ithist(1,2) = fc; ithist(1,4)=itc-1; ithist(1,3)=0; 
while(norm(gc) > tol & itc <= maxit)
%
%       fixup for very long steps, see (3.50) in the book
%
	lambda=min(1,100/(1+norm(gc))); xt=xc-lambda*gc; ft=feval(f,xt);
        numf=numf+1;
	iarm=0; itc=itc+1;
        fgoal=fc-alp*lambda*(gc'*gc);
%
%       polynomial line search
%
        q0=fc; qp0=-gc'*gc; lamc=lambda; qc=ft;
	while(ft > fgoal)
		iarm=iarm+1;
                if iarm==1
                   lambda=polymod(q0, qp0, lamc, qc, blow, bhigh);
                else
                   lambda=polymod(q0, qp0, lamc, qc, blow, bhigh, lamm, qm);
                end
                qm=qc; lamm=lamc; lamc=lambda;
		xt=xc-lambda*gc;
		ft=feval(f,xt); numf = numf+1; qc=ft;
		if(iarm > 10) 
		disp(' Armijo error in steepest descent ')
                histout=ithist(1:itc,:); costdata=[numf, numg, numh];
		return; end
                fgoal=fc-alp*lambda*(gc'*gc);
	end
	xc=xt; [fc,gc]=feval(f,xc); numf=numf+1; numg=numg+1;
	ithist(itc,1)=norm(gc); ithist(itc,2) = fc; 
	ithist(itc,4)=itc-1; ithist(itc,3)=iarm;
end
x=xc; 
histout=ithist(1:itc,:); costdata=[numf, numg, numh];

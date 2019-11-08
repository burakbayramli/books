function [x,histout,costdata] = gaussn(x0,f,tol,maxit)
%
% C. T. Kelley, Dec 14, 1997
%
% This code comes with no guarantee or warranty of any kind.
%
% function [x,histout,costdata] = gaussn(x0,f)
%
% Damped Gauss-Newton with Armijo rule
% simple divide by 2 stepsize reduction
%
% Input: x0 = initial iterate
%        f = r^T r/2 = objective function,
%            the calling sequence for f should be
%            [fout,gout,jac]=f(x) where fout=f(x) is a scalar
%              gout = jac^T r = grad f(x) is a COLUMN vector
%              and jac = r' = Jacobian of r is an M x N matrix
%        tol = termination criterion norm(grad) < tol
%        maxit = maximum iterations (optional) default = 100
%
% Output: x = solution
%         histout = iteration history   
%             Each row of histout is      
%       [norm(grad), f, number of step length reductions, iteration count] 
%         costdata = [num f, num grad, num hess] (for gaussn, num hess=0)
%
% At this stage all iteration parameters are hardwired in the code.
%
%
alp=1.d-4;
if nargin < 4
maxit=100; 
end
itc=1; xc=x0; 
[fc,gc,jac]=feval(f,xc); 
numf=1; numg=1; numh=0;
ithist=zeros(1,4);
ithist(1,1)=norm(gc); ithist(1,2) = fc; ithist(1,4)=itc-1; ithist(1,3)=0; 
while(norm(gc) > tol & itc <= maxit)
        dc=(jac'*jac)\gc;
	lambda=1.0; xt=xc-lambda*dc; ft=feval(f,xt); numf=numf+1;
	iarm=0; itc=itc+1;
%
% Goal for sufficient decrease
%
        fgoal= fc - alp*lambda*(gc'*dc);
	while(ft > fgoal)
		iarm=iarm+1;
		lambda=lambda/2;
		xt=xc-lambda*dc;
		ft=feval(f,xt); numf=numf+1;
		if(iarm > 10) 
		disp(' Armijo error in Gauss-Newton')
                x=xc; histout=ithist(1:itc-1,:)
                costdata=[numf, numg, numh];
		return; end
	end
	xc=xt; [fc,gc,jac]=feval(f,xc); numf=numf+1; numg=numg+1;
	ithist(itc,1)=norm(gc); ithist(itc,2) = fc; 
	ithist(itc,4)=itc-1; ithist(itc,3)=iarm;
end
x=xc; histout=ithist(1:itc,:);
costdata=[numf, numg, numh];

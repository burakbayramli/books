function [xp, idid, lambda]=polyline(xc, fc, gc, d, ft, f, maxarm)
%
% C. T. Kelley, Dec 29, 1997
%
% This code comes with no guarantee or warranty of any kind.
%
% function [xp, idid]=polyline(xc, fc, gc, d, ft, fobj, maxarm)
%
% polynomial line search, call after first point is rejected
%
% Input: xc = current point
%        fc = current function value
%        gc = current gradient value
%         d = direction
%        ft = trial function (rejected value)
%         f = objective function
%             the calling sequence for f should be
%             [fout,gout]=f(x) where fout=f(x) is a scalar
%             and gout = grad f(x) is a COLUMN vector
%    maxarm = maximum number of step length reductions   
%
% Output: xp = successful new point (if it exists)
%       idid = number of calls to f (if line search succeeds) or
%              -1 if line search fails.
%
% Requires: polymod.m
%
% line search parameters that everyone uses
%
alp=1.d-4; blow=.1; bhigh=.5;
%
% Set up the search
%
q0=fc; qp0=gc'*d; qc=ft; lamc=1; iarm=0; numf=0;
fgoal=fc+alp*lamc*qp0;
while ft > fgoal
    iarm=iarm+1;
    if iarm==1  % quadratic
       lambda=polymod(q0, qp0, lamc, qc, blow, bhigh);
    else
       lambda=polymod(q0, qp0, lamc, qc, blow, bhigh, lamm, qm);
    end
    qm=qc; lamm=lamc; lamc=lambda;
    xt=xc+lambda*d;
    ft=feval(f,xt); numf = numf+1; qc=ft;
    if(iarm > maxarm)
         disp(' line search failure'); idid=-1; xp=xc;
    return; end
    fgoal=fc+alp*lamc*qp0;
end
xp=xt; idid=numf;

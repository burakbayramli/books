function [x,lhist,histout,simpdata]=nelder(x0,f,tol,maxit,budget)
%
% Nelder-Mead optimizer, No tie-breaking rule other than MATLAB's sort
%
% C. T. Kelley, December 12, 1996
%
%
% This code comes with no guarantee or warranty of any kind.
%
% function [x,lhist,histout,simpdata] = nelder(x0,f,tol,maxit,budget)
%
% inputs:
%	vertices of initial simplex = x0 (n x n+1 matrix)
%          The code will order the vertices for you and no benefit is
%          accrued if you do it yourself.
%
%       objective function = f
%
%       termination tolerance = tol
%       maximum number of iterations = maxit (default = 100)
%           As of today, dist = | best value - worst value | < tol
%           or when maxit iterations have been taken
%       budget = max f evals (default=50*number of variables)
%                 The iteration will terminate after the iteration that
%                 exhausts the budget
%
%
% outputs:
%	final simplex = x (n x n+1) matrix
%
%       number of iterations before termination = itout (optional)
%       iteration histor = histout itout x 5
%         histout = iteration history, updated after each nonlinear iteration
%                 = lhist x 5 array, the rows are
%                   [fcount, fval, norm(grad), dist, diam]
%                   fcount = cumulative function evals
%                   fval = current best function value
%                   norm(grad) = current simplex grad norm
%                   dist = difference between worst and best values
%                   diam = max oriented length
%       simpdata = data for simplex gradient restart 
%              = [norm(grad), cond(v), bar f]
%
% initialize counters
%
lhist=0; fcount=0;
%
% set debug=1 to print out iteration stats
%
debug=0;
%
% Set the N-M parameters
%
rho=1; chi=2; gamma=.5; sigma=.5;
dsize=size(x0); n=dsize(1);
if nargin < 4 maxit=100; end
if nargin < 5 budget=100*n; end
%
% set the paramters for stagnation detection/fixup
% setting oshrink=0 gives vanilla Nelder-Mead
%
oshrink=1; restartmax=3; restarts=0;
%
%
% Order the vertices for the first time
%
x=x0; [n,m]=size(x); histout=zeros(maxit*3,5); simpdata=zeros(maxit,3);
itout=0; orth=0;
xtmp=zeros(n,n+1); z=zeros(n,n); delf=zeros(n,1);
for j=1:n+1; fv(j)=feval(f,x(:,j)); end; fcount=fcount+n+1;
[fs,is]=sort(fv); xtmp=x(:,is); x=xtmp; fv=fs;
itc=0; dist=fv(n+1)-fv(1);
diam=zeros(n,1);
for j=2:n+1
   v(:,j-1)=-x(:,1)+x(:,j);
   delf(j-1)=fv(j)-fv(1);
   diam(j-1)=norm(v(:,j-1));
end
sgrad=v'\delf; alpha=1.d-4*max(diam)/norm(sgrad);
lhist=lhist+1;
histout(lhist,:)=[fcount, fv(1), norm(sgrad,inf), 0, max(diam)];
%
% main N-M loop
%
while(itc < maxit & dist > tol & restarts < restartmax & fcount <= budget)
    fbc=sum(fv)/(n+1);
    xbc=sum(x')'/(n+1);
    sgrad=v'\delf;
    simpdata(itc+1,1)=norm(sgrad);
    simpdata(itc+1,2)=cond(v);
    simpdata(itc+1,3)=fbc;
    if(det(v) == 0)
        disp('simplex collapse')
        break
    end
    happy=0; itc=itc+1; itout=itc;
%
% reflect
%
    y=x(:,1:n);
    xbart = sum(y')/n;  % centriod of better vertices
    xbar=xbart';
    xr=(1 + rho)*xbar - rho*x(:,n+1);
    fr=feval(f,xr); fcount=fcount+1;
    if(fr >= fv(1) & fr < fv(n)) happy = 1; xn=xr; fn=fr; end;
%    if(happy==1) disp(' reflect '); end
%
% expand
%
    if(happy == 0 & fr < fv(1))
        xe = (1 + rho*chi)*xbar - rho*chi*x(:,n+1);
        fe=feval(f,xe); fcount=fcount+1;
        if(fe < fr) xn=xe;  fn=fe; happy=1; end
        if(fe >=fr) xn=xr;  fn=fr; happy=1; end
%        if(happy==1) disp(' expand '); end
    end
%
% contract
%
   if(happy == 0 & fr >= fv(n) & fr < fv(n+1))
%
% outside contraction
%
       xc=(1 + rho*gamma)*xbar - rho*gamma*x(:,n+1);
       fc=feval(f,xc); fcount=fcount+1;
       if(fc <= fr) xn=xc; fn=fc; happy=1; end;
%       if(happy==1) disp(' outside '); end;
   end
%
% inside contraction
%
   if(happy == 0 & fr >= fv(n+1))
       xc=(1 - gamma)*xbar+gamma*x(:,n+1);
       fc=feval(f,xc); fcount=fcount+1;
       if(fc < fv(n+1)) happy=1; xn=xc; fn=fc; end;
%   if(happy==1) disp(' inside '); end;
   end
%
%  test for sufficient decrease, 
%  do an oriented shrink if necessary
%
   if(happy==1 & oshrink==1)
       xt=x; xt(:,n+1)=xn; ft=fv; ft(n+1)=fn;
%       xt=x; xt(:,n+1)=xn; ft=fv; ft(n+1)=feval(f,xn); fcount=fcount+1;
       fbt=sum(ft)/(n+1); delfb=fbt-fbc; armtst=alpha*norm(sgrad)^2;
       if(delfb > -armtst/n) 
           restarts=restarts+1;
           orth=1; diams=min(diam);
           sx=.5+sign(sgrad); sx=sign(sx);
if debug==1
           [itc, delfb, armtst]
end
           happy=0;
           for j=2:n+1; x(:,j)=x(:,1); 
           x(j-1,j)=x(j-1,j)-diams*sx(j-1); end;
       end
   end
%
%  if you have accepted a new point, nuke the old point and
%  resort
%
   if(happy==1)
       x(:,n+1)=xn; fv(n+1)=fn;
%       x(:,n+1)=xn; fv(n+1)=feval(f,xn); fcount=fcount+1;
       [fs,is]=sort(fv); xtmp=x(:,is); x=xtmp; fv=fs;
   end
%
% You're in trouble now! Shrink or restart.
%
   if(restarts >= restartmax) disp(' stagnation in Nelder-Mead'); end;
   if(happy == 0 & restarts < restartmax)
       if(orth ~=1) disp(' shrink '); end;
       if(orth ==1) 
       if debug == 1 disp(' restart '); end
       orth=0; end;
       for j=2:n+1;
           x(:,j)=x(:,1)+sigma*(x(:,j)-x(:,1));
           fv(j)=feval(f,x(:,j));
       end
       fcount=fcount+n;
       [fs,is]=sort(fv); xtmp=x(:,is); x=xtmp; fv=fs;
   end
%
%  compute the diameter of the new simplex and the iteration data
%
   for j=2:n+1
       v(:,j-1)=-x(:,1)+x(:,j);
       delf(j-1)=fv(j)-fv(1);
       diam(j-1)=norm(v(:,j-1));
   end
   dist=fv(n+1)-fv(1);
   lhist=lhist+1;
   sgrad=v'\delf;
   histout(lhist,:)=[fcount, fv(1), norm(sgrad,inf), dist, max(diam)];
end

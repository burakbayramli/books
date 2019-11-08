function [x,lhist,histout] = imfil(x0,f,budget,scales,parms)
%
%
% C. T. Kelley, January 9, 1998
%
% This code comes with no guarantee or warranty of any kind.
%
% function [x,fcount,histout] = imfil(x0,f,budget,scales,parms)
%
% Unconstrained implicit filtering code
% 
% IMPLICIT FILTERING with SR1 and BFGS quasi-Newton methods
%
% Input: x0 = initial iterate
%        f = objective function,
%            the calling sequence for f should be [fout]=f(x)
%        budget = max f evals 
%                 The iteration will terminate after the iteration that
%                 exhausts the budget, default=50*number of variables
%        scales = the decreasing sequence of difference increments 
%                 This is an optional argument and the default is
%                 1, 1/2, ... 1/128
%        parms = optional argument = array of conrol praamters
%
%        parms(1) = 
%             target = value of f at which the iteration should be terminated
%                 This is an optional argument, which you SHOULD set to
%                 something reasonable for your problem. The default is
%                 as close to no limit as we can get, -1.d8
%
%        parms(2) = 0 for centered diffs, 1 for forward diffs
%                   default and recommended value = 0
% 
%        parms(3) = quasi-Newton method selection
%                   0 = none, 1 = bfgs, 2 = SR1
%                   default and recommend value = 1
%
%
% Output: x = estimated minimizer
%         lhist = number of nonzero rows in histout 
%               = number of nonlinear iterations for all the scales        
%         histout = iteration history, updated after each nonlinear iteration 
%                 = lhist x 5 array, the rows are
%                   [fcount, fval, norm(sgrad), norm(step), iarm]
%                   fcount = cumulative function evals
%                   fval = current function value
%                   norm(sgrad) = current simplex grad norm
%                   norm(step) = norm of last step 
%                   iarm=line searches to date
%                        =-1 means first iterate at a new scale 
%
% This code uses centered difference approximations to the gradient. Setting
% fdiff = 1 will change this to forward differences. We do not recommend 
% that.
%
% set debug = 1 to print iteration stats
%
debug=0;
%
fcount=0; 
%
% And now for the knobs. Implicit filtering has too many of these and they
% can make a difference. The ones we use are:
%
% min_gscal (default = .01)
%   if norm(difference_grad) < min_gscal*h  we terminate at the scale
%   with success
%
% maxit and maxitarm (defaults 2 and 5)
%    At most maxit*n iterations are taken for each scale and at most
%    maxitarm step length reductions are allowed
%
% nterm controls termination on stencil failure for centered diffs (defalt = 0)
%       = 0 to terminate on stencil failure before starting the line search
%       = 1 to ignore stencil failure
%
% iquit (default = 3)
%    After iquit consecutive line search failures, we terminate the iteration
%
% beta (default = .5) 
%    step size reduction factor in the line search
%
% set the knobs
%
% min_gscal=.5; 
min_gscal=.01; 
% maxit=10; maxitarm=10; iquit=3; beta=.1;
maxit=200; maxitarm=10; iquit=3; beta=.5; nterm=0;
%
% set up the difference scales
%
quasi=1; fdiff=0; ftol=-1.d8;
if nargin == 5
    np=length(parms); ftol=parms(1);
    if np>1 & parms(2) == 1 fdiff=1; end
    if np==3 quasi=parms(3); end
end
nterm=fdiff+nterm;
if nargin>3
    dscal=scales;
else
    dscal=-(0:8)'; dscal=2.^dscal;
end
nscal=length(dscal);
n=length(x0);
if nargin> 2; flim=budget; 
else flim = 50*n; end
%
% initialize histout
%
lhist=0; 
histout=zeros(nscal*maxit*n*maxitarm,5);
%
% sweep through the scales
%
x=x0; xold=x0; n=length(x0); v=eye(n); xc=x0; hess=eye(n); ns=0; iquitc=0;
% for ns=1:nscal
while (ns < nscal & fcount <= flim & iquitc < iquit)
    ns=ns+1;
    itc=0; h=dscal(ns); z0=x; fval=feval(f,x); fcount=fcount+1;
    stol=min_gscal*h; iarm=0; lok=1;
    [sgrad,fb,xb,sflag] =simpgrad(x,f,h*v,fval,fdiff);
    fcount=fcount+(2-fdiff)*n; lhist=lhist+1;
    histout(lhist,:)=[fcount, fval, norm(sgrad), 0, -1];
    if norm(sgrad,inf) < stol | (sflag+nterm)==0
%
%   Convergence at this scale on stencil failure or tolerance match
%
       gc=sgrad; lhist=lhist+1;
       if (sflag+nterm) ~= 0 
           iquitc=iquitc+1; 
       else
           iquitc=0;
       end
       histout(lhist,:)=[fcount, fval, norm(sgrad), 0, -1];
   else
%
%   Take a few quasi-Newton iterates
%
       iquitc=0;
       while itc < maxit*n & fval > ftol & norm(sgrad,inf) >= stol...
            &lok==1 & fcount < flim & sflag+nterm > 0
           itc=itc+1;
%
%     compute the difference gradient, scale it
%
            gc=sgrad; 
            if(itc > 1)
              [sgrad,fb,xb,sflag]=simpgrad(x,f,h*v,fval,fdiff); 
              fcount=fcount+(2-fdiff)*n; 
            end
            dgrad=sgrad;
%
%     watch out for stencil failure!
%
          if sflag+nterm > 0
%
%     update iterate and Hessian 
%
            if itc > 1 & quasi > 0
                  if quasi==1
                     hess = bfupdate(x, xc, sgrad, gc, hess); 
                  else
	             hess = sr1up(x, xc, sgrad, gc, hess); 
	          end
	    end; 
            xc=x;
%
%     new direction and line search
%
            if quasi > 0
                 sdir=hess\dgrad;
            else
                 sdir=dgrad;
            end
            [fct, x, fval, hess, iarm]=...
                  linearm(f, sdir, fval, x, hess, maxitarm,beta,h,quasi,fdiff);
            fcount=fcount+fct;
%
%     reduce scale upon failure of line search
%
            if iarm >= maxitarm
               lok=0;
               x=xb; fval=fb;
            end
          end
%
%      keep the records
%
            stepn=norm(xold-x,inf); xold=x; lhist=lhist+1;
            histout(lhist,:)=[fcount, fval, norm(sgrad), stepn, iarm];
%
      end % end of nonlinear step
%
   end % end of sweep through the scale
if debug ==1 
    [ns,fval,h,norm(sgrad),itc]
end
end % end of loop over the scales
%
%   BFGS update of Hessian; nothing fancy
%
function hess = bfupdate(x, xc, sgrad, gc, hess)
y=sgrad-gc; s=x-xc; z=hess*s;
if y'*s > 0
   hess = hess + (y*y'/(y'*s)) - (z*z'/(s'*z));
end
%
% SR1 update
%
function hess = sr1up(x, xc, sgrad, gc, hess)
y=sgrad-gc; s=x-xc; z=y - hess*s;
if z'*s ~=0
	ptst=z'*(hess*z)+(z'*z)*(z'*z)/(z'*s); 
	if ptst > 0 hess = hess + (z*z')/(z'*s); end
end
%
%    Line search for implicit filtering
%
function [fct, x, fval, hessp, iarm]=...
                linearm(f, sdir, fold, xc, hess, maxitarm,beta,h,quasi,fdiff)
lambda=1;
n=length(xc);
hessp=hess;
iarm=-1;
fct=0;
aflag=1;
dd=sdir;
smax=10*min(h,1); if norm(dd) > smax dd=smax*dd/norm(dd); end
x=xc;
fval=fold;
while iarm < maxitarm & aflag==1
    d=-lambda*dd; 
    iarm=iarm+1;
    xt=x+d; ft=feval(f,xt); fct=fct+1;
    if ft < fval & aflag==1; aflag=0; fval=ft; x=xt; end
    if aflag==1; lambda=beta*lambda; end
end
if iarm == maxitarm & aflag == 1
       disp(' line search failure'); [iarm, h, quasi, fdiff]
%      hessp=eye(n);
end

function results = mlogit(y,x,beta,theta)
% PURPOSE: multinomial logistic regression 
% logit(p_ij) = theta(j) + x_i'beta , i = 1,..,nobs, j = 1,..,k-1,
%---------------------------------------------------
% USAGE: results = mlogit(y,x,beta,theta)
% where: y = dependent variable vector (nobs x 1)
%        x = independent variables matrix (nobs x nvar)
%     beta = (optional) initial values for beta 
%    theta = (optional) initial values for theta  
% NOTE:  k = # of distinct ordinal values in round(y)
%---------------------------------------------------
% RETURNS: a structure
%        results.meth  = 'mlogit'
%        results.beta  = theta and beta estimates
%        results.tstat = t-stats for theta and beta
%        results.yhat  = yhat (fitted probs, p_ij)
%        results.lik   = log-likelihood function value
%        results.grad  = derivative of likelihood wrt beta,theta
%        results.nobs  = nobs
%        results.nvar  = nvars (nvar + k)
%        results.k     = # of categories
%        results.iter  = # of iterations
%        results.z     = y data vector in category form
%        results.y     = y data vector from input
%---------------------------------------------------
% NOTES: uses functions mlogit_lik, mderivs, dmult        
%---------------------------------------------------
% SEE ALSO: prt(results), plt(results)
%---------------------------------------------------

% written by:
%  Gordon K Smyth, U of Queensland, Australia, gks@maths.uq.oz.au
% Nov 19, 1990.  Last revision Aug 29, 1995.

% documentation and results structure modifications made by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu


% check input
if nargin<2, error('mlogit: wrong # of input arguments'); end;
y=round(y(:)); [my ny]=size(y); [mx nx]=size(x);
if (mx~=my), error('mlogit: row dimensions of x and y disagree'); end;

% initial calculations
xstd=std(x); x=-x./(ones(mx,1)*xstd);
tol=1e-6; incr=10; decr=2;
ymin=min(y); ymax=max(y); yrange=ymax-ymin;
z =( y*ones(1,yrange) )==( ones(my,ny)*( ymin   :(ymax-1)) );
z1=( y*ones(1,yrange) )==( ones(my,ny)*((ymin+1): ymax   ) );
z=z(:,any(z)); z1=z1(:,any(z1)); [mz nz]=size(z);

% starting values
if nargin<3, g=cumsum(sum(z))'./my; theta=log(g./(1-g)); end;
if nargin<4, beta=zeros(nx,1); else beta=beta.*(xstd'); end;
tb=[theta; beta];

if nargin > 4, error('mlogit: wrong # of arguments'); end;

% likelihood and derivatives at starting values
[g,g1,p,dev]=mlogit_lik(y,x,tb,z,z1);
[dl,d2l]=mderivs(x,z,z1,g,g1,p);
epsilon=std(d2l(:))/1000;

% maximize likelihood using Levenberg modified Newton's method
iter=0;
while abs(dl'*(d2l\dl)/length(dl)) > tol,
   iter=iter+1;
   tbold=tb;
   devold=dev;
   tb=tbold-d2l\dl;
   [g,g1,p,dev]=mlogit_lik(y,x,tb,z,z1);
   if (dev-devold)/(dl'*(tb-tbold)) < 0,
      epsilon=epsilon/decr;
   else;
      while (dev-devold)/(dl'*(tb-tbold)) > 0,
         epsilon=epsilon*incr;
         if epsilon>1e+15,
            error('mlogit: epsilon too large');
         end;
         tb=tbold-(d2l-epsilon*eye(d2l))\dl;
         [g,g1,p,dev]=mlogit_lik(y,x,tb,z,z1);
      end;
   end;
   [dl,d2l]=mderivs(x,z,z1,g,g1,p);
end;
% put together results structure
results.meth = 'mlogit';
results.y = y;
results.z = z;
results.k = nz;
results.lik = -dev/2;
results.grad = dl;
results.nvar = nx+nz;
results.nobs = my;
theta = tb(1:nz,1);
beta = tb(nz+1:nz+nx,1)./(xstd');;
results.beta = [theta
                beta ];
results.iter = iter;
se=sqrt(diag(inv(-d2l)));
results.tstat(1:nz,1) = tb(1:nz,1)./se(1:nz,1);
results.tstat(nz+1:nz+nx,1) = beta./(se((nz+1):(nz+nx),1)./(xstd'));
e=( (x*tb((nz+1):(nz+nx),1))*ones(1,nz) )+( ones(my,ny)*theta' );
results.prob=diff([zeros(my,ny) exp(e)./(1+exp(e)) ones(my,ny)]')';


function [dl,d2l]=mderivs(x,z,z1,g,g1,p)
% PURPOSE: function called by mlogit to calculate derivatives of ln(L)
%---------------------------------------------------
% NOTE: Calls dmult function

% written by:
%  Gordon K Smyth, U of Queensland, Australia, gks@maths.uq.oz.au
% Nov 19, 1990.  Last revision Aug 29, 1995.

% documentation and results structure modifications made by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu


% first derivative
v=g.*(1-g)./p; v1=g1.*(1-g1)./p;
dlogp=[dmult(v,z)-dmult(v1,z1) dmult(v-v1,x)];
dl=sum(dlogp)';

% second derivative
w=v.*(1-2*g); w1=v1.*(1-2*g1);
d2l=[z x]'*dmult(w,[z x])-[z1 x]'*dmult(w1,[z1 x])-dlogp'*dlogp;

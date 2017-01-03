function alpha=step(b,infoz,stat,varargin)
% PURPOSE: Determine step size in NUMZ package
%-----------------------------------------------------------------
% USAGE: alpha=step(b,infoz,stat,varargin)
%  Where
%  b         vector of model parameters
%  infoz     structure variable with settings for MINZ0
%  stat      structure variable with minimization status
%  varargin  Variable list of arguments passed to func
%
% RETURNS:   alpha     scalar step size
%-----------------------------------------------------------------
% REFERENCES:  Numerical Recipes in FORTRAN  (LNSRCH, p. 378)
%-----------------------------------------------------------------

% written by:
% Mike Cliff,  UNC Finance  mcliff@unc.edu
% CREATED:  1/24/99


%=================================================================
%  INITIALIZATIONS
%=================================================================

direc = stat.direc;
alf=infoz.ftol;
fold = stat.f;
maxalpha = 10;
alpha=1;
tmpalpha=1;
go=1;
lvar = length(varargin);
func = fcnchk(infoz.func,lvar);

% I don't use this step from  Num. Recipes; it causes trouble
%sumx = sqrt(direc'*direc)
%if sumx > maxalpha
%  direc = direc*maxalpha/sumx
%end

slope = stat.G'*direc;
temp=abs(direc)./max(abs(b),1);
temp=max(temp);
test=max(temp,eps);         % Added to avoid /0 error
minalpha = infoz.btol/test;
b0 = b;

%=================================================================
%  FIND MINIMIZING STEP SIZE
%=================================================================

while go == 1
  b = b0 + alpha*direc;
  if strcmp(infoz.call,'other'),
  f = feval(func,b,varargin{:});
  else,
  f = feval(func,b,infoz,stat,varargin{:});
  end;
  if alpha < minalpha
    b = b0;
    go = 0;
    alpha = 0;
  elseif f < fold + alpha*slope*alf
    go = 0;
  else
    if alpha == 1
      tmpalpha = -slope/(2*(f-fold-slope));
    else
      rhs1 = f - fold - alpha*slope;
      rhs2 = f2 - fold2 - alpha2*slope;
      a = (rhs1/alpha^2 - rhs2/alpha2^2)/(alpha-alpha2);
      b = (-alpha2*rhs1/alpha^2 + alpha*rhs2/alpha2^2)/(alpha-alpha2);
      if a == 0
        tmpalpha = -slope/(2*b);
      else 
        disc = b^2 - 3*a*slope;
        if disc < 0
          disp('Round off problem in STEP'); 
          disc = 0;
        end
        tmpalpha = (-b+sqrt(disc))/(3*a);
      end
      if tmpalpha > .5*alpha, tmpalpha = .5*alpha; end
    end
  end
  alpha2 = alpha;
  f2 = f;
  fold2 = fold;
  if go ~= 0, alpha = max(tmpalpha,.1*alpha); end
end

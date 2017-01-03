function [x,fval,exitflag,output]=fminsearchbnd(fun,x0,LB,UB,options,varargin)
% fminsearchbnd: fminsearch, but with bound constraints by transformation
% usage: fminsearchbnd(fun,x0,LB,UB,options,p1,p2,...)
% 
% arguments:
%  LB - lower bound vector or array, must be the same size as x0
%
%       If no lower bounds exist for one of the variables, then
%       supply -inf for that variable.
%
%       If no lower bounds at all, then LB may be left empty.
%
%  UB - upper bound vector or array, must be the same size as x0
%
%       If no upper bounds exist for one of the variables, then
%       supply +inf for that variable.
%
%       If no upper bounds at all, then UB may be left empty.
%
%  See fminsearch for all other arguments and options.
%  Note that TolX will apply to the transformed variables. All other
%  fminsearch parameters are unaffected.
%
% Notes:
%
%  Variables which are constrained by both a lower and an upper
%  bound will use a sin transformation. Those constrained by
%  only a lower or an upper bound will use a quadratic
%  transformation, and unconstrained variables will be left alone.
%
%  At least one variable must be bounded, on at least one end.
%
%  Variables may not be fixed by setting their bounds equal.
%
%  The bounds are inclusive inequalities, which admit the
%  boundary values themselves, but will not permit ANY function
%  evaluations outside the bounds.
%
%
% Example usage:
% rosen = @(x) (1-x(1)).^2 + 105*(x(2)-x(1).^2).^2;
%
% fminsearch(rosen,[3 3])     % unconstrained
% ans =
%    1.0000    1.0000
%
% fminsearchbnd(rosen,[3 3],[2 2],[])     % constrained
% ans =
%    2.0000    4.0000

% size checks
xsize = size(x0);
x0 = x0(:);
n=length(x0);

if isempty(LB)
  LB = repmat(-inf,n,1);
else
  LB = LB(:);
end
if isempty(UB)
  UB = repmat(inf,n,1);
else
  UB = UB(:);
end

if (all(isinf(LB)) & all(isinf(UB)))
  error 'Just use fminsearch on unbounded problems.'
end

if (n~=length(LB)) | (n~=length(UB))
  error 'x0 is incompatible in size with either LB or UB.'
end

% set default options if necessary
if (nargin<5)|isempty(options)
  options = optimset('fminsearch');
end

% stuff into a struct to pass around
params.args = varargin;
params.LB = LB;
params.UB = UB;
params.fun = fun;

% 0 --> unconstrained variable
% 1 --> lower bound only
% 2 --> upper bound only
% 3 --> dual finite bounds
params.BoundClass = zeros(n,1);
for i=1:n
  k = isfinite(LB(i)) + 2*isfinite(UB(i));
  params.BoundClass(i) = k;
  if (k==3) & (LB(i)==UB(i))
    error 'Bounds must be distinct if both are supplied.'
  end
end

% transform starting values into their unconstrained
% surrogates. Check for infeasible starting guesses.
x0u = x0;
for i = 1:n
  switch params.BoundClass(i)
    case 1
      % lower bound only
      if x0(i)<=LB(i)
        % infeasible starting value. Use bound.
        x0u(i) = 0;
      else
        x0u(i) = sqrt(x0(i) - LB(i));
      end
    case 2
      % upper bound only
      if x0(i)>=UB(i)
        % infeasible starting value. use bound.
        x0u(i) = 0;
      else
        x0u(i) = sqrt(UB(i) - x0(i));
      end
    case 3
      % lower and upper bounds
      if x0(i)<=LB(i)
        % infeasible starting value
        x0u(i) = -pi/2;
      elseif x0(i)>=UB(i)
        % infeasible starting value
        x0u(i) = pi/2;
      else
        x0u(i) = 2*(x0(i) - LB(i))/(UB(i)-LB(i)) - 1;
        x0u(i) = asin(max(-1,min(1,x0u(i))));
      end
    case 0
      % unconstrained variable. x0u(i) is set.
  end
end

% now we can call fminsearch, but with our own
% intra-objective function.
[xu,fval,exitflag,output] = fminsearch(@intrafun,x0u,options,params);

% undo the variable transformations into the original space
x = xtransform(xu,params);

% final reshape
x = reshape(x,xsize);

% ======================================
% ========= begin subfunctions =========
% ======================================
function fval = intrafun(x,params);
% transform variables, then call original function

% transform
xtrans = xtransform(x,params);

% and call fun
fval = feval(params.fun,xtrans,params.args{:});


% ======================================
function xtrans = xtransform(x,params);
% converts unconstrained variables into their original domains

xtrans = x;
for i = 1:length(x)
  switch params.BoundClass(i)
    case 1
      % lower bound only
      xtrans(i) = params.LB(i) + x(i).^2;
    case 2
      % upper bound only
      xtrans(i) = params.UB(i) - x(i).^2;
    case 3
      % lower and upper bounds
      xtrans(i) = (sin(x(i))+1)/2;
      xtrans(i) = xtrans(i)*(params.UB(i) - params.LB(i)) + params.LB(i);
    case 0
      % unconstrained variable. xtrans(i) is set.
  end
end








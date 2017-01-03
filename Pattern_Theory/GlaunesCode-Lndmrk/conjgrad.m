function [x, flog, pointlog] = conjgrad(x, f, gradf, options, varargin)

%CONJGRAD Conjugate gradients optimization.
%
%	Description
%	[X, FLOG, POINTLOG] = CONJGRAD(F, X, OPTIONS, GRADF) uses a
%	conjugate gradients algorithm to find the minimum of the function
%	F(X) whose gradient is given by GRADF(X).  Here X is a row vector and
%	F returns a scalar value.  The point at which F has a local minimum
%	is returned as X. A log of the function values after each cycle is
%	(optionally) returned in FLOG, and a log of the points visited is
%	(optionally) returned in POINTLOG.
%
%	CONJGRAD(F, X, OPTIONS, GRADF, P1, P2, ...) allows  additional
%	arguments to be passed to F() and GRADF().
%
%	The optional parameters have the following interpretations.
%
%	verbosemode is set to 1 to verbosemode error values; also logs error
%	values in the return argument ERRLOG, and the points visited in the
%	return argument POINTSLOG.  If verbosemode is set to 0, then only
%	warning messages are verbosemodeed.  If verbosemode is -1, then nothing is
%	verbosemodeed.
%
%	breakratio is a measure of the precision required of the objective
%	function at the solution.  If the absolute difference between the
%	objective function values between two successive steps is less than
%	breakratio times its initial value, then this condition is satisfied.
%   default 1e-6
%
%	maxiter is the maximum number of iterations; default 500.
%
%	lineprec is the precision in parameter space of the line search;
%	default 1E-4.

%  Set up the options.
maxiter = options.maxiter;
breakratio = options.breakratio;
lineprec = 1e-6;
verbosemode = options.verbosemode;

% Set up options for line search
line_options = foptions;
% Need a precise line search for success
if lineprec > 0
  line_options(2) = lineprec;
else
  line_options(2) = 1e-4;
end

% Next two lines allow conjgrad to work with expression strings
f = fcnchk(f, length(varargin));
gradf = fcnchk(gradf, length(varargin));

nparams = length(x(:));
fnew = feval(f, x, varargin{:});
flog = fnew;
finit = fnew;
xmaxinit = max(abs(x(:)));
gradnew = feval(gradf, x, varargin{:});
d = -gradnew;		% Initial search direction
br_min = 0;
br_max = 1.0;	% Initial value for maximum distance to search along
tol = sqrt(eps);

j = 1;
if nargout >= 3
  flog(j, :) = fnew;
  if nargout == 4
    pointlog(j, :) = x(:)';
  end
end

while (j <= maxiter)

  xold = x;
  fold = fnew;
  gradold = gradnew;

  gg = gradold(:)'*gradold(:);
  if (gg == 0.0)
    % If the gradient is zero then we are done.
    return;
  end

  % This shouldn't occur, but rest of code depends on d being downhill
  if (gradnew(:)'*d(:) > 0)
    d = -d;
    if verbosemode
      warning('search direction uphill in conjgrad');
    end
  end

  line_sd = d./norm(d(:));
  [lmin, line_options] = feval(@linemin, f, xold, line_sd, fold, ...
    line_options, varargin{:});
  % Set x and fnew to be the actual search point we have found
  x = xold + lmin * line_sd;
  fnew = line_options(8);

  % Check for termination
  if fold - fnew < breakratio*finit
    return;
  end

  gradnew = feval(gradf, x, varargin{:});

  % Use Polak-Ribiere formula to update search direction
  gamma = ((gradnew(:) - gradold(:))'*(gradnew(:)))/gg;
  d = (d .* gamma) - gradnew;

  if (verbosemode > 0)
    fprintf(1, 'Cycle %4d  Function %11.6f\n', j, line_options(8));
  end

  j = j + 1;
  if nargout >= 2
    flog(j, :) = fnew;
    if nargout == 3
      pointlog(j, :) = x(:)';
    end
  end
  
  
end

% If we get here, then we haven't terminated in the given number of 
% iterations.

if (verbosemode >= 0)
  disp(maxitmess);
end


%%%%%%%%%%%%%%%%%%% subfunctions

function v = dval(u, x, y, h)

[DW,N] = size(u);
h2 = h^2;
[D, M] = size(y);
v = zeros(DW, M);
for i = 1:M,
    for j = 1:N,
      dxy = 0;
      for d = 1:D
        dxy = dxy + (x(d,j) - y(d,i))^2/h2;
      end
      for k = 1:DW
	v(k,i) = v(k,i) + u(k,j)*exp(-dxy);
      end
    end
end

return;


function y = linef(lambda, fn, x, d, varargin)
%LINEF	Calculate function value along a line.
%
%	Description
%	LINEF(LAMBDA, FN, X, D) calculates the value of the function FN at
%	the point X+LAMBDA*D.  Here X is a row vector and LAMBDA is a scalar.
%
%	LINEF(LAMBDA, FN, X, D, P1, P2, ...) allows additional arguments to
%	be passed to FN().   This function is used for convenience in some of
%	the optimisation routines.
%
%	See also
%	GRADCHEK, LINEMIN
%

%	Copyright (c) Ian T Nabney (1996-2001)

% Check function string
fn = fcnchk(fn, length(varargin));

y = feval(fn, x+lambda.*d, varargin{:});



function [x, options] = linemin(f, pt, dir, fpt, options, varargin)

%LINEMIN One dimensional minimization.
%
%	Description
%	[X, OPTIONS] = LINEMIN(F, PT, DIR, FPT, OPTIONS) uses Brent's
%	algorithm to find the minimum of the function F(X) along the line DIR
%	through the point PT.  The function value at the starting point is
%	FPT.  The point at which F has a local minimum is returned as X.  The
%	function value at that point is returned in OPTIONS(8).
%
%	LINEMIN(F, PT, DIR, FPT, OPTIONS, P1, P2, ...) allows  additional
%	arguments to be passed to F().
%
%	The optional parameters have the following interpretations.
%
%	OPTIONS(1) is set to 1 to display error values.
%
%	OPTIONS(2) is a measure of the absolute precision required for the
%	value of X at the solution.
%
%	OPTIONS(3) is a measure of the precision required of the objective
%	function at the solution.  Both this and the previous condition must
%	be satisfied for termination.
%
%	OPTIONS(14) is the maximum number of iterations; default 100.

% Set up the options.
if(options(14))
  niters = options(14);
else
  niters = 100;
end
options(10) = 0; % Initialise count of function evaluations

display = options(1);

% Check function string
f = fcnchk(f, length(varargin));

% Value of golden section (1 + sqrt(5))/2.0
phi = 1.6180339887499;
cphi = 1 - 1/phi;
TOL = sqrt(eps);	% Maximal fractional precision
TINY = 1.0e-10;         % Can't use fractional precision when minimum is at 0

% Bracket the minimum
[br_min, br_mid, br_max, num_evals] = feval(@minbrack, @linef, ...
  0.0, 1.0, fpt, f, pt, dir, varargin{:});
options(10) = options(10) + num_evals;  % Increment number of fn. evals
					% No gradient evals in minbrack

% Use Brent's algorithm to find minimum
% Initialise the points and function values
w = br_mid;   	% Where second from minimum is
v = br_mid;   	% Previous value of w
x = v;   	% Where current minimum is
e = 0.0; 	% Distance moved on step before last
fx = feval(@linef, x, f, pt, dir, varargin{:});
options(10) = options(10) + 1;
fv = fx; fw = fx;

for n = 1:niters
  xm = 0.5.*(br_min+br_max);  % Middle of bracket
  % Make sure that tolerance is big enough
  tol1 = TOL * (max(abs(x))) + TINY;
  % Decide termination on absolute precision required by options(2)
  if (max(abs(x - xm)) <= options(2) & br_max-br_min < 4*options(2))
    options(8) = fx;
    return;
  end
  % Check if step before last was big enough to try a parabolic step.
  % Note that this will fail on first iteration, which must be a golden
  % section step.
  if (max(abs(e)) > tol1)
    % Construct a trial parabolic fit through x, v and w
    r = (fx - fv) .* (x - w);
    q = (fx - fw) .* (x - v);
    p = (x - v).*q - (x - w).*r;
    q = 2.0 .* (q - r);
    if (q > 0.0) p = -p; end
    q = abs(q);
    % Test if the parabolic fit is OK
    if (abs(p) >= abs(0.5*q*e) | p <= q*(br_min-x) | p >= q*(br_max-x))
      % No it isn't, so take a golden section step
      if (x >= xm)
        e = br_min-x;
      else
        e = br_max-x;
      end
      d = cphi*e;
    else
      % Yes it is, so take the parabolic step
      e = d;
      d = p/q;
      u = x+d;
      if (u-br_min < 2*tol1 | br_max-u < 2*tol1)
        d = sign(xm-x)*tol1;
      end
    end
  else
    % Step before last not big enough, so take a golden section step
    if (x >= xm)
      e = br_min - x;
    else
      e = br_max - x;
    end
    d = cphi*e;
  end
  % Make sure that step is big enough
  if (abs(d) >= tol1)
    u = x+d;
  else
    u = x + sign(d)*tol1;
  end
  % Evaluate function at u
  fu = feval(@linef, u, f, pt, dir, varargin{:});
  options(10) = options(10) + 1;
  % Reorganise bracket
  if (fu <= fx)
    if (u >= x)
      br_min = x;
    else
      br_max = x;
    end
    v = w; w = x; x = u;
    fv = fw; fw = fx; fx = fu;
  else
    if (u < x)
      br_min = u;   
    else
      br_max = u;
    end
    if (fu <= fw | w == x)
      v = w; w = u;
      fv = fw; fw = fu;
    elseif (fu <= fv | v == x | v == w)
      v = u;
      fv = fu;
    end
  end
  if (display == 1)
    fprintf(1, 'Cycle %4d  Error %11.6f\n', n, fx);
  end
end
options(8) = fx;



function  [br_min, br_mid, br_max, num_evals] = minbrack(f, a, b, fa,  ...
			 varargin)
%MINBRACK Bracket a minimum of a function of one variable.
%
%	Description
%	BRMIN, BRMID, BRMAX, NUMEVALS] = MINBRACK(F, A, B, FA) finds a
%	bracket of three points around a local minimum of F.  The function F
%	must have a one dimensional domain. A < B is an initial guess at the
%	minimum and maximum points of a bracket, but MINBRACK will search
%	outside this interval if necessary. The bracket consists of three
%	points (in increasing order) such that F(BRMID) < F(BRMIN) and
%	F(BRMID) < F(BRMAX). FA is the value of the function at A: it is
%	included to avoid unnecessary function evaluations in the
%	optimization routines. The return value NUMEVALS is the number of
%	function evaluations in MINBRACK.
%
%	MINBRACK(F, A, B, FA, P1, P2, ...) allows additional arguments to be
%	passed to F
%
%	See also
%	LINEMIN, LINEF
%

%	Copyright (c) Ian T Nabney (1996-2001)

% Check function string
f = fcnchk(f, length(varargin));

% Value of golden section (1 + sqrt(5))/2.0
phi = 1.6180339887499;

% Initialise count of number of function evaluations
num_evals = 0;

% A small non-zero number to avoid dividing by zero in quadratic interpolation
TINY = 1.e-10;

% Maximal proportional step to take: don't want to make this too big
% as then spend a lot of time finding the minimum inside the bracket
max_step = 10.0;

fb = feval(f, b, varargin{:});
num_evals = num_evals + 1;

% Assume that we know going from a to b is downhill initially 
% (usually because gradf(a) < 0).
if (fb > fa)
  % Minimum must lie between a and b: do golden section until we find point
  % low enough to be middle of bracket
  c = b;
  b = a + (c-a)/phi;
  fb = feval(f, b, varargin{:});
  num_evals = num_evals + 1;
  cnt = 1;
  while (fb > fa) & (cnt<5)
      cnt = cnt + 1;
    c = b;
    b = a + (c-a)/phi;
    fb = feval(f, b, varargin{:});
    num_evals = num_evals + 1;
  end
else  
  % There is a valid bracket upper bound greater than b
  c = b + phi*(b-a);
  fc = feval(f, c, varargin{:});
  num_evals = num_evals + 1;
  bracket_found = 0;
  
  while (fb > fc)
    % Do a quadratic interpolation (i.e. to minimum of quadratic)
    r = (b-a).*(fb-fc);
    q = (b-c).*(fb-fa);
    u = b - ((b-c)*q - (b-a)*r)/(2.0*(sign(q-r)*max([abs(q-r), TINY])));
    ulimit = b + max_step*(c-b);
    
    if ((b-u)'*(u-c) > 0.0)
      % Interpolant lies between b and c
      fu = feval(f, u, varargin{:});
      num_evals = num_evals + 1;
      if (fu < fc)
	% Have a minimum between b and c
	br_min = b;
	br_mid = u;
	br_max = c;
	return;
      elseif (fu > fb)
	% Have a minimum between a and u
	br_min = a;
	br_mid = c;
	br_max = u;
	return;
      end
      % Quadratic interpolation didn't give a bracket, so take a golden step
      u = c + phi*(c-b);
    elseif ((c-u)'*(u-ulimit) > 0.0)
      % Interpolant lies between c and limit
      fu = feval(f, u, varargin{:});
      num_evals = num_evals + 1;
      if (fu < fc)
	% Move bracket along, and then take a golden section step
	b = c;
	c = u;
	u = c + phi*(c-b);
      else
	bracket_found = 1;
      end
    elseif ((u-ulimit)'*(ulimit-c) >= 0.0)
      % Limit parabolic u to maximum value
      u = ulimit;
    else
      % Reject parabolic u and use golden section step
      u = c + phi*(c-b);
    end
    if ~bracket_found
      fu = feval(f, u, varargin{:});
      num_evals = num_evals + 1;
    end
    a = b; b = c; c = u;
    fa = fb; fb = fc; fc = fu;
  end % while loop
end   % bracket found
br_mid = b;
if (a < c)
  br_min = a;
  br_max = c;
else
  br_min = c; 
  br_max = a;
end



function s = maxitmess()
%MAXITMESS Create a standard error message when training reaches max. iterations.
%
%	Description
%	S = MAXITMESS returns a standard string that it used by training
%	algorithms when the maximum number of iterations (as specified in
%	OPTIONS(14) is reached.
%
%	See also
%	CONJGRAD, GLMTRAIN, GMMEM, GRADDESC, GTMEM, KMEANS, OLGD, QUASINEW, SCG
%

%	Copyright (c) Ian T Nabney (1996-2001)

s = 'Maximum number of iterations has been exceeded';


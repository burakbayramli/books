function I = adaptGK(fun,a,b,tol,maxLevel,varargin)
% adaptGK  Adaptive numerical integration using Gauss-Kronrod 7-15 rule
%          and automatic interval subdivision.  Interval over which the
%          G-K rule is applied is bisected until the larger of an absolute
%          or relative tolerance is met, or until recursion limit is reached 
%
% Synopsis:  I = adaptGK(fun,a,b)
%            I = adaptGK(fun,a,b,tol)
%            I = adaptGK(fun,a,b,tol,maxLevel)
%            I = adaptGK(fun,a,b,tol,maxLevel,arg1,arg2)
%
% Input:  fun = (string) name of m-file that evaluates f(x)
%         a,b = lower and upper limits of the integral
%         tol = tolerance on changes in numerical value of integral.
%               Refinement is stopped when abs(I1-I2) < max(dr*I1,da)
%               where I1 and I2 are values of integral on level 1 (coarse)
%               and levels 2 (fine) level;  dr = relative tolerance, and
%               da = absolute tolerance.  If tol is a scalar then dr = tol
%               and da = 0.  If tol is a vector of length 2, then dr = tol(1),
%               and da = tol(2);  Default:  tol = [5e-3  5e-6]
%               If input tol = [], default values are used
%         maxLevel = (optional) maximum number of refinements.  Corresponds
%                    to 2^(maxLevel-1) panels or h = (b-a)/( 2^(maxLevel-1) )
%                    Default: maxLevel = 16 implies h/(b-a) = 3e-5
%                    If input maxLevel = [], default value are used
%         arg1,arg2,... = (optional) additional parameters passed through to fun
%
% Output:    I = approximate value of the integral of f(x)*dx from a to b

if nargin<4,  tol = [5e-3  5e-6];      end  %  Default tol and maxLevel
if nargin<5,  maxLevel=16;             end
if isempty(maxLevel),  maxLevel = 16;  end
if isempty(tol)           %  case of tol = [] on input
  tol = [5e-3  5e-6];
else
  if length(tol)==1       %  True if only relative tolerance is supplied
    tol = [tol 0];
  end
end

% --- Call worker function with "level" and "I1" arguments that
%     are invisible to user of adaptGK
I = doAdaptGK(fun,a,b,tol,maxLevel,0,0,varargin{:});

% =====================
function I = doAdaptGK(fun,a,b,tol,maxLevel,level,I1,varargin)
% doAdaptGK  Routine that does work of adaptive Gauss-Kronrod quadrature
%            adaptGK is a simple interface to doAdaptGK that hides
%            the level and I1 parameters from the user 
%
% Synopsis:  I = doAdaptGK(fun,a,b,tol,maxLevel,level,I1,vararing)
%
% Input:  Same parameters as adaptGK except
%         level = local refinement level;  level = 0 on startup
%         I1    = current value of integral on this level;  I1 = 0 on startup

level = level + 1;         %  Current refinement level
if level == 1              %  on first call, I1 has not been evaluated
   [I1,ae1] = gaussKronrod15(fun,a,b,varargin{:});
end
c = a + 0.5*(b-a);
[I2left,ae2left]   = gaussKronrod15(fun,a,c,varargin{:});
[I2right,ae2right] = gaussKronrod15(fun,c,b,varargin{:});

I2 = I2left + I2right;  ae2 = ae2left + ae2right;

% --- Prevent infinite loop if recursion limit is exceeded by more than 2
if level>maxLevel
  warning(sprintf('In adaptGK: level = %d exceeds limit\n',level));
  if level>maxLevel+2
    fprintf('Refinement halted, recursion limit exceded by factor of 2\n');
    I = I2;  return;   %  This "return" will stop the recursion 
  end
end

% --- Refine more, or accept?
if abs(I2-I1) < max(tol(1)*abs(I1),tol(2))   %  Is difference between levels significant?
  I = I2;  return;          %  Yes: accept fine level result, stop recursion
else                        %  No:  subdivide again.
  I =   doAdaptGK(fun,a,c,[tol(1) tol(2)/2],maxLevel,level,I2left,varargin{:}) ...      %  I_left
      + doAdaptGK(fun,c,b,[tol(1) tol(2)/2],maxLevel,level,I2right,varargin{:});        %  I_right
end

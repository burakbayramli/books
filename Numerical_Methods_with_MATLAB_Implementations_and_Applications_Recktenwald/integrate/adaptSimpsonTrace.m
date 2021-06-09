function [I,x] = adaptSimpsonTrace(fun,a,b,tol,maxLevel,level)
% adaptSimpsonTrace  Adaptive numerical integration based on Simpson's rule
%
% Synopsis:  [I,x] = adaptSimpson(fun,a,b)
%            [I,x] = adaptSimpson(fun,a,b,tol)
%            [I,x] = adaptSimpson(fun,a,b,tol,maxLevel)
%
% Input:  fun  = (string) name of m-file that evaluates f(x)
%         a,b  = lower and upper limits of the integral
%         tol  = (optional) absolute tolerance on error. Default:  tol = 5e-6
%         maxLevel = (optional) limit on number of refinements.  
%                    Warnings are issued when maxLevel is reached.
%                    Refinement is halted when maxLevel+2 consecutive
%                    refinements are attempted. Default:  maxLevel = 10
%
% Output:    I = approximate value of the integral of f(x)*dx from a to b
%            x = vector of x values used to evaluate the integral

if nargin<4,  tol = 5e-6;     end
if nargin<5,  maxLevel = 10;  end
if nargin<6,  level = 0;      end      %  level~=0 only on recursive call

level = level + 1;                     %  current refinement level
h2 = (b-a)/4;                          %  Node spacing on (h/2) level
x = a:h2:b;                            %  Save x for trace output
f = feval(fun,x);                      %  Evaluate all f(x) for this level
s1 = (f(1) + 4*f(3) + f(5))*2*h2/3;                  %  h   level
s2 = (f(1) + 4*f(2) + 2*f(3) + 4*f(4) + f(5))*h2/3;  %  h/2 level

% --- Prevent infinite loop if recursion limit is exceeded by more than 2
if level>maxLevel
  warning(sprintf('In adaptSimpson: level = %d exceeds limit\n',level));
  if level>maxLevel+2
    fprintf('Refinement halted, recursion limit exceded by factor of 2\n');
    I = s2;   return;   %  This "return" will stop the recursion 
  end
end

% --- Refine more, or accept?
if abs(s2-s1) < 15*tol        %  Is difference between levels significant?
  I = s2;  return;            %  Yes: accept fine level result, stop recursion
else                          %  No:  subdivide again.
  c = a + (b-a)/2;
  [Il,xl] = adaptSimpsonTrace(fun,a,c,tol/2,maxLevel,level);   %  I_left
  [Ir,xr] = adaptSimpsonTrace(fun,c,b,tol/2,maxLevel,level);   %  I_right
  I = Il + Ir;
  x = [xl(1:end) xr(2:end)];
end

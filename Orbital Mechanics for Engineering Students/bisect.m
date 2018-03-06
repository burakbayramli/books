% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
function root = bisect(fun, xl, xu)
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%{
  This function evaluates a root of a function using
  the bisection method

  tol  - error to within which the root is computed
  n    - number of iterations
  xl   - low end of the interval containing the root
  xu   - upper end of the interval containing the root
  i    - loop index 
  xm   - mid-point of the interval from xl to xu
  fun  - name of the function whose root is being found
  fxl  - value of fun at xl
  fxm  - value of fun at xm
  root - the computed root

  User M-functions required: none
%}
% ----------------------------------------------

tol = 1.e-8;
n   = ceil(log(abs(xu - xl)/tol)/log(2));

for i = 1:n
    xm  = (xl + xu)/2;
    fxl = feval(fun, xl);
    fxm = feval(fun, xm);
    if fxl*fxm > 0
        xl = xm;
    else
        xu = xm;
    end
end

root = xm;

end
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
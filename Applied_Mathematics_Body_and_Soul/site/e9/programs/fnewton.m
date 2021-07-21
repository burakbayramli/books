function x = fnewton(f, x0, tol)
% Newton's Method solution of f(x) = 0
% f is the function f(x) = 0, x0 is initial guess and tol is tolerance
% in the residual f(x)
  
  x = x0;
  residual = feval(f, x);

  % We measure the residual to check convergence. Alternatively we can
  % measure the difference between the new and old x.

  while(abs(residual) > tol)
    a = 1 / derivative(f, x);
    x = x - a * feval(f, x);
    residual = feval(f, x);
  end

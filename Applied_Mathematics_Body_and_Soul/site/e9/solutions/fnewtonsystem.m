function x = fnewtonsystem(f, x0, tol)
% Newton's Method solution of the system f(x) = 0
% f is the function f(x) = 0, x0 is initial guess and tol is tolerance
% in the residual f(x)
  
  x = x0;
  residual = feval(f, x);

  % We measure the residual to check convergence. Alternatively we can
  % measure the difference between the new and old x.

  while(norm(residual) > tol)
    J = jacobi(f, x);
    r = -feval(f, x);
    h = J \ r;
    x = h + x;
    residual = feval(f, x);
  end

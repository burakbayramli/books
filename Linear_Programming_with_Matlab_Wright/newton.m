function [z] = newton(z0, F, J, eps, itmax)
%
% syntax: z = newton(z0, @myF, @myJ, eps, itmax)
% 
% performs Newton's method from starting point z0, terminating when
% 2-norm of step is shorter than eps or when at itmax steps have been
% taken, whichever comes first. Call as follows:
%
% where z0 is the starting point, myF and myJ are the actual names 
% of the function and Jacobian evaluation routines; method terminates 
% when length of Newton step drops below eps or after at most itmax 
% iterations (whichever comes first).

z=z0; iter = 0;
while iter<itmax
  Fval = feval(F,z); Jval = feval(J,z);
  fprintf(' iteration %3d, Fnorm=%9.4e\n', iter, norm(Fval));
  zstep = -Jval\Fval;
  z = z + zstep; iter = iter+1;
  if norm(zstep) < eps	% stop if the step is short
    break;
  end
end
return;

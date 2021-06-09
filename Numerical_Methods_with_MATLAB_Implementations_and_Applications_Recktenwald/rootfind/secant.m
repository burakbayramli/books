function x = secant(fun,a,b,xtol,ftol,verbose)
% secant  Secant method for finding roots of scalar f(x) = 0
%
% Synopsis:  x = secant(fun,a,b)
%            x = secant(fun,a,b,xtol)
%            x = secant(fun,a,b,xtol,ftol)
%            x = secant(fun,a,b,xtol,ftol,verbose)
%
%  Input:  fun  = (string) name of function for which roots are sought
%          a,b  = endpoints of initial bracket interval
%          xtol = (optional) relative tolerance on x.     Default:  xtol=5*eps
%          ftol = (optional) relative tolerance on f(x).  Default:  ftol=5*eps
%          verbose = (optional) printing switch.  Default: verbose=0, no printing.
%
%  Output:  x = the root of the function

error('Completion of code in secant function is an exercise for the reader');

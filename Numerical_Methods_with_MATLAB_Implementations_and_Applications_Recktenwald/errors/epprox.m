function epprox
% epprox  Demonstrate catastrophic cancellation in evaluation of
%         e = exp(1) = lim_{n->infinity} (1 + 1/n)^n
%
% Synopsis:  y = epprox
%
% Input:     none
%
% Output:    Table comparing exp(1) and f(n) = (1 + 1/n)^n as n -> infinity

%  Ref:  N.J. Higham, Accuracy and Stability of Numerical Algorithms,
%        1996, SIAM, section 1.11

e = exp(1);
fprintf('\n    n          f(n)              error\n');
for n=logspace(0,16,9)
   f = (1+1/n)^n;
   fprintf('%9.1e  %14.10f  %14.10f\n',n,f,abs(f-e));
end

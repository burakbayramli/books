function ssum = sinser(x,tol,n)
% sinser  Evaluate the series representation of the sine function
%
% Synopsis: ssum = sinser(x)
%           ssum = sinser(x,tol)
%           ssum = sinser(x,tol,n)
%
% Input:    x   = argument of the sine function, i.e., compute sin(x)
%           tol = (optional) tolerance on accumulated sum. Default:  tol = 5e-9
%                 Series is terminated when abs(T_k/S_k) < delta.   T_k is the
%                 kth term and S_k is the sum after the kth term is added.                
%           n   = (optional) maximum number of terms. Default: n = 15
%
% Output:   ssum = value of series sum after nterms or tolerance is met

if nargin < 2,  tol = 5e-9;  end
if nargin < 3,  n = 15;      end

term = x;  ssum = term;         %  Initialize series
fprintf('Series approximation to sin(%f)\n\n  k      term         ssum\n',x);
fprintf('%3d  %11.3e  %12.8f\n',1,term,ssum); 

for k=3:2:(2*n-1)
  term = -term * x*x/(k*(k-1));                  %  Next term in the series
  ssum = ssum + term;
  fprintf('%3d  %11.3e  %12.8f\n',k,term,ssum); 
  if abs(term/ssum)<tol, break;  end             %  True at convergence
end
fprintf('\nTruncation error after %d terms is %g\n\n',(k+1)/2,abs(ssum-sin(x)));

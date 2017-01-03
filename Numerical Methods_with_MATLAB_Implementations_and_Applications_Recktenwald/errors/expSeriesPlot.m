function ssum = expSeriesPlot(x,tol,n)
% expSeriesPlot  Evaluate and plot series representation of exp(x)
%
% Synopsis:  ssum = expSeriesPlot(x)
%            ssum = expSeriesPlot(x,tol)
%            ssum = expSeriesPlot(x,tol,n)
%
% Input:  x   = argument of the exp function, i.e., compute exp(x)
%         tol = (optional) tolerance on accumulated sum. Default: tol = 5e-9
%               Series is terminated when T_k/S_k < delta, where T_k is the
%               kth term and S_k is the sum after the kth term is added.
%         n   = (optional) maximum number of terms. Default: n = 15
%
% Output:   ssum = value of series sum after n or tolerance is met

if nargin < 2,  tol = 5e-9;  end
if nargin < 3,  n = 15;      end

term = 1;  ssum = term;  Eabs(1) = abs(ssum-exp(x));   % Initialize
fprintf('Series approximation to exp(%f)\n\n',x);
fprintf('  k      term         ssum         Eabs\n'); 
fprintf('%3d  %11.3e  %11.3e  %11.3e\n',1,x,ssum,Eabs(1)); 

for k=2:n
  term = term*x/(k-1);                  % Next term in the series
  ssum = ssum + term;
  Eabs(k) = abs(ssum-exp(x));
  fprintf('%3d  %11.3e  %11.3e  %11.3e\n',k,term,ssum,Eabs(k));
  if abs(term/ssum)<tol, break;  end    %  True at convergence
end

semilogy(1:k,Eabs,'-');
xlabel('Number of terms');   ylabel('Absolute Error');
fprintf('\nTruncation error after %d terms is %11.3e\n\n',k,Eabs(k));

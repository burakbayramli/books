function p = legndr(n,x)
% Legendre polynomials function
% Inputs  
%    n = Highest order polynomial returned
%    x = Value at which polynomial is evaluated
% Output
%    p = Vector containing P(x) for order 0,1,...,n

%* Perform upward recursion
p(1)=1;      % P(x) for n=0
if(n == 0) return; end
p(2)=x;      % P(x) for n=1
for i=3:n+1  % Use upward recursion to obtain other n's
  p(i) = ((2*i-3)*x*p(i-1) - (i-2)*p(i-2))/(i-1);
end
return;

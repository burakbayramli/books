function vectorSequence(x,n)
% vectorSequence  Behavior of a vector sequence x.^k in different p-norms
%
% Synopsis:  vectorSequence
%            vectorSequence(x)
%            vectorSequence(x,n)
%
% Input:     x = (optional) vector used in sequence x.^k
%                Default:  x = [1/2  1/4  1/8  1/16];
%            n = (optional) maximum number of iterations; Default: n = 10
%
% Output:    Plot of norm(x.^k,p) for k = 1,2,...,n and p = 1, 2, Inf

if nargin<1,  x = [1/2  1/4  1/8  1/16];  end
if nargin<2,  n = 10;                     end

fprintf('   k    norm(x^k,1)    norm(x^k,2)    norm(x^k,Inf)\n');
for k=0:n
  y = x.^k;
  norm1 = norm(y,1);  norm2 = norm(y);  normi = norm(y,inf);
  fprintf('%4d  %12.3e   %12.3e   %12.3e\n',k,norm1,norm2,normi);
  semilogy(k,norm1,'d',k,norm2,'o',k,normi,'+');
  hold on        
end
hold off
xlabel('iteration, k');  ylabel('norms of x^k');
legend('1 norm','2 norm','\infty norm');

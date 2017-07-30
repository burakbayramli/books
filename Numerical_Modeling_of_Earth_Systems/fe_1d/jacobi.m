function [ x counts residue ] = jacobi(a,b,x,tol,max_i)
%
%  jacobi iterative solver for A x = b 
%
%  x should be initialized with some starting vector
%

m=length(b);
x1 = zeros(m,1);			% 


counts=0;
jrange=1:m;

normb = norm(b);

residue = 10;
while ((counts<max_i) && (residue > tol))
  for i=1:m
    if(a(i,i) == 0)
        error('matrix singular')
    end
    ind = find(jrange ~= i);
    sigma = sum(a(i,ind) * x(ind));% if A is declared sparse, matlab will take care of not using the zero entries
    x1(i) = (b(i)-sigma)/a(i,i);
  end
  
  residue = max(abs(x-x1))/normb;
  x = x1;
  normb = norm(x);
  counts = counts + 1;
end


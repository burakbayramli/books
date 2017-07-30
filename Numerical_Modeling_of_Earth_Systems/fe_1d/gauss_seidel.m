function [ x counts residue ] = gauss_seidel(a,b,x,tol,max_i)
%
% gauss_seidel, x should be filled with the initial guess
% 

m = length(b);

bnorm = norm(b);

accuracy = tol*bnorm;
counts=0;
residue = accuracy * 10;

jrange=1:m;
onorm = norm(x);
while((counts<max_i) && (residue>accuracy))
  for i=1:m
    if(a(i,i) == 0)
      error('matrix singular')
    end
    ind = find(jrange ~= i);
    var = sum(a(i,ind)*x(ind));% if A is declared sparse, matlab will take care of not using the zero entries
    x(i)=(b(i) - var)/a(i,i);
  end
  nnorm = norm(x);
  residue = abs(nnorm - onorm)/bnorm;

  onorm = nnorm;
  counts=counts+1;
end


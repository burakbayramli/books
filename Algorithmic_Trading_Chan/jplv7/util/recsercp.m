function result = recsercp(x,z)
% PURPOSE: computes a recursive series involving products
%--------------------------------------------------------
% USAGE: result = recsercp(x,z)
% where: x = a matrix or vector of dimensions (n,k)
%        z = a matrix or vector of same dimensions
%--------------------------------------------------------
% RETURNS:
%        results(1:n,1:k) = contains columns computed
%        recursively with result(1,:)=x(1,:) + z(1,:) and
%        result(j,:)=result(j-1,:).*x(j,:) + z(j,:)
%--------------------------------------------------------

% written by:
% Kit Baum
% Dept of Economics
% Boston College
% Chestnut Hill MA 02467 USA
% baum@bc.edu
% 9525

if (nargin ~= 2)
   error('Wrong number of arguments to recsercp');
end;
[n1 k1] = size(x);
[n2 k2] = size(z);
if (n1 ~= n2)
   error('recsercp arguments must have same number of rows');
end;
if (k1 ~= k2)
   error('recsercp arguments must have same number of columns');
end;
result=zeros(n1,k1);
result(1,:) = x(1,:) + z(1,:);
if (n1==1)
   return;
end;
for j=2:n1;
   result(j,:)=result(j-1,:).*x(j,:) + z(j,:);
end;


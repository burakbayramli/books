function result = recserar(x,y0,a)
% PURPOSE: computes a vector of autoregressive recursive series
%--------------------------------------------------------------
% USAGE: result = recserar(x,y0,a)
% where: x  = a matrix of dimensions (n,k)
%        y0 = a matrix of dimensions (p,k)
%        a  = a matrix of dimensions (p,k)
%--------------------------------------------------------
% RETURNS:
%        results(1:n,1:k) = contains columns computed
%        recursively with result=y0 for rows 1:p, and
%        result(j,:)=result(j-1,:).*a(1:p,:) + x(j,:)
%        for rows p+1:n.
%--------------------------------------------------------

% written by:
% Kit Baum
% Dept of Economics
% Boston College
% Chestnut Hill MA 02467 USA
% baum@bc.edu
% 9525

if (nargin ~= 3)
   error('Wrong number of arguments to recserar');
end;
[n1 k1] = size(x);
[p1 k2] = size(y0);
[p2 k3] = size(a);
if (k1 ~= k2)
   error('recserar x, y0 must have same number of columns');
end;
if (k1 ~= k3)
   error('recserar x, a must have same number of columns');
end;
if (p1 ~= p2)
   error('recserar y0, a must have same number of rows');
end;
result=zeros(n1,k1);
for j=1:p1;
result(j,:) = y0(j,:);
end;
for j=(p1+1):n1;
   result(j,:)=x(j,:);
   for k=1:p1;
      result(j,:)=result(j,:)+a(k,:).*result(j-k,:);
   end;
end;


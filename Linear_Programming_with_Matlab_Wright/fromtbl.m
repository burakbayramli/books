function [A,b,p,alpha,B] = fromtbl(H,xlab,objlab,rhslab)
% syntax: [A,b,p,alpha,B] = fromtbl(H,xlab,objlab,rhslab)
% extract the matrices A,b,p,alpha,B,N from tableau H
% xlab is the character that starts the variable names
% objlab is the label on the objective row
% rhslab is the label on the rhs

if nargin < 4
  rhslab = '1';
  if nargin < 3
    objlab = 'z';
    if nargin < 2
      xlab = 'x';
    end
  end
end

[m,n] = size(H.val);
y = strmatch(xlab,H.bas);
x = strmatch(xlab,H.nonbas);

A = H.val(y,x);

rhs = strmatch(rhslab,H.nonbas,'exact');
if ~isempty(rhs)
  b = -H.val(y,rhs);
end;

obj = strmatch(objlab,H.bas,'exact');
if ~isempty(obj)
  p = H.val(obj,x)';
  if ~isempty(rhs)
    alpha = H.val(obj,rhs);
  end;
end;

m = length(y); B = zeros(m,1);
for i=1:m
  H.bas{y(i)}(1) = [];
  B(i) = str2num(H.bas{y(i)});
end

return;

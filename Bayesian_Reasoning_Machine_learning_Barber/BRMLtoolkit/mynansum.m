function y = mynansum(x,d)
%MYNANSUM sum of values that are not nan
x(isnan(x))=0;
if nargin==1; y = sum(x);
else y = sum(x,d);
end
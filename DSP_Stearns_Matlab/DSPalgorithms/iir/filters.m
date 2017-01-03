function y=filters(b,a,x)
% y=filters(b,a,x)
% 
% Same as Matlab "filter" function except b and a both have
% N rows, representing N filter sections in cascade.
%
% If N=1, the filter has only one section.
% If a=1, the filter is an FIR filter with no feedback weights.
%
% x is assumed to be a vector. If so, y is the filtered version of x.
% If not, see the description of the Matlab "filter" function.
%
% See also filter, filters2

[N,n]=size(b);
[M,m]=size(a);
if(N~=M)
   error('The b and a arrays must have the same # rows.');
end
input=x;
for i=1:N,
   y=filter(b(i,:),a(i,:),input);
   input=y;
end
function y=filters2(b,a,x)
% y=filters2(b,a,x)
% 
% Same as Matlab "filter" function except b and a both have
% N rows, representing N filter sections in cascade.
%
% If N=1, the filter has only one section.
% If a=1, the filter is an FIR filter with no feedback weights.
%
% If x is a vector, y is the filtered version of x.
% If x is an array, the columns of x are filtered.
% 
% See also filter, filters

%if b is a vector and a is a scalar, make b a row vector
if length(b)==numel(b) & numel(a)==1,
    b=b(:)';
end
%check for errors
[N,n]=size(b);
[M,m]=size(a);
if N~=M,
   error('The b and a arrays must have the same # rows.');
end
[K,cols]=size(x);
y=zeros(K,cols);
for c=1:cols,
    input=x(:,c);
    for i=1:N,
        yc=filter(b(i,:),a(i,:),input);
        input=yc;
    end
    y(:,c)=yc;
end
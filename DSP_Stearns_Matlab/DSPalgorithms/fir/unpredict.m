function x=unpredict(b,e)
% x=unpredict(b,e)
%
% Inverse of function predict. Recovers x from b and e.
% Recovery of integer vector x is exact.
%
% Inputs:
%    b  = weight vector output by function predict.
%    e  = integer prediction error vector from predict.
% Outputs:  
%    x  = integer version of input vector used by predict.
%
% See also: lms_predict, lms_unpredict, lms_filter
b=col_vec(b);
N=length(b);
e=col_vec(e);
K=length(e);
x=zeros(K,1);
f=zeros(N,1);
for k=1:K,
    x(k)=e(k)+round(b'*f);
    f=[x(k);f(1:N-1)];
end
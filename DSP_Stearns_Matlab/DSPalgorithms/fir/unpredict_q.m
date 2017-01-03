function x=unpredict_q(b,e,q)
% x=unpredict_q(b,e,q)
%
% Inverse of function predict_q. Recovers x from b and e.
% Inputs:
%    b  = weight vector output by function precict.
%    e  = integer prediction error vector from predict.
%    q  = quantizing factor used in predict_q.
% Outputs:  
%    x  = integer estimate of input vector used by predict.
%         (May be inaccurate if q>1.
%
% See also: predict_q, predict, unpredict, lms_predict, lms_unpredict

% Check for errors.
if nargin~=3,
    error 'unpredict_q requires 3 arguments.'
elseif q<1,
    error 'Third argument (q) must be at least 1.'
end

% Initialize.
b=col_vec(b);
N=length(b);
e=col_vec(e);
K=length(e);
x=zeros(K,1);
f=zeros(N,1);
for k=1:K,
    x(k)=q*e(k)+round(b'*f);
    f=[x(k);f(1:N-1)];
end
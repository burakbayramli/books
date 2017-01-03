function [e,b]=predict_q(x,N,q)
% [e,b]=predict_q(x,N,q)
%
% Least-squares one-step prediction with quantizing.
% Lossy prediction in the time domain with recovery possible.
%
% Inputs:
%    N  = filter size.
%    x  = input vector to FIR predictor. 
%         NOTE: x is rounded to integers.
%    q  = quantizing factor: error (e) is divided by q and rounded.
%
% Outputs:  
%    e  = quantized prediction error, e(1:length(x)) (integers).
%    b  = optimal weight vector with N elements.
%
% See also: unpredict_q, predict, predict_nr, lms_predict, lms_filter

% Round x to integers.
x=round(col_vec(x));                    %x is rounded to integers

% Check for errors.
if max(abs(x))==0
    error 'No prediction possible because x=0 after rounding.'
end
if nargin~=3,
    error 'predict_q requires 3 arguments.'
elseif q<1,
    error 'Third argument (q) must be at least 1.'
end

% Compute the optimal weight vector, b.
K=length(x);
Rff=autocovar_mat(x,N);           %covariance if c=0
rfd=crosscovar([0; x(1:K-1)],x,N);
b=Rff\rfd;                              %optimal weights

% Initialize e=K zeros and f=N zeros.
e=zeros(K,1);
f=zeros(N,1);

% Filter; update g, e and f at each step.
for k=1:K,
	g=round(f'*b);                      % kth kth filter output, rounded
    e(k)=round((x(k)-g)/q);             % output quantized error
    xhat=q*e(k)+g;                      % x(k) reproducible in unpredict_q
    f=[xhat; f(1:N-1)];                 % f reproducible in unpredict_q
end
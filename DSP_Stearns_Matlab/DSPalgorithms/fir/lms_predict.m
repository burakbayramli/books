function [e,P,b]=lms_predict(b,u,x,Q)
% [e,P,b]=lms_predict(b,u,x,Q)
%
% Adaptive LPC with rounding and quantization.
% Inputs:
%    b  = initial weight vector with length >=1 and <=1000.
%    u  = adaptive gain constant; 0 <= u << 1.
%    x  = input vector of INTEGER samples.
%    Q  = quantization integer used to reduce entropy of e.
%         (See below. Q=1 if omitted.)
% Outputs:  
%    e  = prediction error, e(1:length(x)).
%    P  = power used to compute mu=2*u/(N*P).
%    b  = final weight vector.
%
% Weights are computed similar to (9.51) in text. However,
% the filter output is quantized to produce e as integers.
% Furthermore, a 4th argument, Q (integer>1), will change e
% to e=round(e/Q), thus producing a lower-entropy error, 
% which is then multiplied by Q and used to adjust b.
% When Q=1, exact recovery is possible.
% P=avg. signal power computed internally; P=mean(x.^2).
% See also: lms_unpredict, lms_filter

% Initialize.
x=round(col_vec(x));
K=length(x);
b=col_vec(b);
N=length(b);

% Check for errors and set Q.
if length(u)>1,
    error('Adaptive gain (u) must be a scalar.');
elseif u<0 | u>=1,
   error('Adaptive gain u is out of range.');
elseif length(b)<1 | length(b)>1e3
    error('Length of weight vector b must be from 1 to 1000.')
elseif K<N,
	error('Length of signal vector should be > length of b.');
end
if nargin==4,
    if Q<1 | Q-round(Q)~=0,
        error('Quantizing factor Q must be an integer and at least 1.');
    end
    Q=round(Q);
else
    Q=1;
end
% Compute mu=2*u/(N*avg. power).
P=max(1.e-6, x'*x/length(x));           % P must be >0
mu=2*u/(N*P);
% Initialize e=K zeros and f=N zeros.
e=zeros(K,1);
f=zeros(N,1);
xhat=0;
% Filter; update e and b at each step.
for k=1:K,
	g=round(f'*b);                      % kth filter output, rounded
    e(k)=round((x(k)-g)/Q);             % output quantized error
    err=Q*e(k);                         % reproducible error used to adapt
	b=b+mu*err*f;                       % updated weight vector
    xhat=err+g;                         % reproducible x(k)
    f=[xhat; f(1:N-1)];                 % reproducible f vector
end
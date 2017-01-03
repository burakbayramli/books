function [e,P,b]=lms_predict_nr(b,u,x)
% [e,P,b]=lms_predict_nr(b,u,x)
%
% Adaptive LPC without rounding and quantization.
% Inputs:
%    b  = initial weight vector.
%    u  = adaptive gain constant; 0 <= u << 1.
%    x  = input vector of INTEGER samples.
% Outputs:  
%    e  = prediction error, e(1:length(x)).
%    P  = power used to compute mu=2*u/(N*P).
%    b  = final weight vector.
%
% Weights are computed similar to (9.51) in the text.
% P=avg. signal power computed internally as P=mean(x.^2).
% See also: lms_predict, lms_filter

% Initialize.
x=x(:);                                 %x is a column vector
K=length(x);
b=b(:);                                 %so is b
N=length(b);

% Check for errors and set Q.
if length(u)>1,
    error('Adaptive gain (u) must be a scalar.');
elseif u<0 | u>=1,
   error('Adaptive gain u is out of range.');
elseif K<N,
	error('Length of signal vector should be > length of b.');
end

% Compute mu=2*u/(N*avg. power).
P=max(1.e-6, x'*x/length(x));           % P must be >0
mu=2*u/(N*P);
% Initialize e=K zeros and f=N zeros.
e=zeros(K,1);
f=zeros(N,1);
% Filter; update e and b at each step with no rounding or quantizing.
for k=1:K,
	g=f'*b;                             % kth filter output
    e(k)=x(k)-g;                        % output error
	b=b+mu*e(k)*f;                      % updated weight vector
    f=[x(k); f(1:N-1)];                 % updated f vector
end
function x=lms_unpredict(b,u,P,e,Q)
% x=lms_unpredict(b,u,P,e,Q)
%
% Recovery of signal from output of lms_predict.
% Inputs:
%    b  = initial weight vector used in lms_predict.
%    u  = adaptive gain used by lms_predict.
%    P  = power term computed by lms_predict.
%    e  = prediction error output from lms_predict.
%    Q  = quantizing integer used in lms_predict.
%         Q is set to 1 if missing.
% Output:  
%    x  = original signal processed by lms_predict.
%
% See also: lms_predict, lms_filter

% Initialize.
e=round(col_vec(e));
K=length(e);
b=col_vec(b);
N=length(b);
x=zeros(K,1);                           % initialize the output 
mu=2*u/(N*P);                           % adaptive gain factor

% Check for errors and set Q.
if u<0 | u>=1,
   error('Adaptive gain u is out of range.');
elseif K<N,
	error('Length of signal vector should be > length of b.');
end
if nargin==5,
    Q=round(Q);
else
    Q=1;
end
% Inverse adaptive filter.
f=zeros(N,1);                           % initialize f
e=Q*e;                                  % scale e by Q
for k=1:K,
	x(k)=e(k)+round(b'*f);              % round filter output, get x
	b=b+mu*e(k)*f;                      % update the weigths
	f=[x(k); f(1:N-1)];                 % update vector f.
end
function [g,b]=lms_filter(b,u,d,f,ind)
% [g,b]=lms_filter(b,u,d,f,ind)
%
% Inputs:
%    b  = initial weight vector with length(b)=N.
%    u  = adaptive gain constant; 0 <= u << 1.
%    d  = desired signal vector.
%    f  = input vector to FIR adaptive filter.
%    ind= indicator (see output "b" below.)
% Output:  
%    g  = adaptive filter output vector; error=d-g.
%    b  = Final weight vector if ind is omitted or zero, or
%       = NxK array of successive weight vectors if ind=1.
%
% This is Widrow's normalized adaptive LMS algorithm
% computed as in (9.51) in the text with power sigma^2
% equal to avg. power in vector f, i.e., mean(f.^2).
% See also: rls_filter
f=f(:);                               	%column vectors f,d,b
d=d(:);
K=min(length(d),length(f));
b=b(:);
N=length(b);

% Check for errors and set ind.
if(u<0 | u>=1),
   error('Adaptive gain u is out of range.');
elseif(K<N),
	error('Lengths of f and d should be > length of b.');
elseif(N==0)
    error('b cannot be empty. It must have at least one element.')
end
if(nargin==5 & ind==1),
    b=[b,zeros(N,K-1)];
else
    ind=0;
end
% Compute mu=2*u/(N*avg. power).
mu=2*u/(N*f'*f/length(f));

% Initialize g=zeros and begin f with N-1 zeros.
g=zeros(K,1);
f=[zeros(N-1,1); f];
% Filter; update g and b at each step.
bb=b(:,1);
for k=1:K,
	fvect=f(k+N-1:-1:k);
	g(k)=bb'*fvect;                     % filter output
	e=d(k)-g(k);                        % error signal
	bb=bb+mu*e*fvect;                   % updated current weight vector
    b(:,(1-ind)+ind*min(k+1,K))=bb;     % updated weight vector history
end
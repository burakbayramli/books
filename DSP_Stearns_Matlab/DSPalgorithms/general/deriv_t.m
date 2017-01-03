function y=deriv_t(x,T);
% y=deriv_t(x,T)
%
%y=trapezoidal differential of x;
%T=time step.
%
%Simple symmetric first-difference differentiation,
%y(k)=(x(k+1)-x(k-1))/(2T).

% Check for errors.
[nr,nc]=size(x);
if nr~=1 & nc~=1,
    error('x must be a vector.');
end
if nargin<4,
    vc=0.5;
end
K=length(x);                    %x is a vector.

% Differentiate.
y=zeros(nr,nc);
y(1)=(x(2)-x(1))/T;             %y(1)
k=2:K-1;
y(k)=(x(k+1)-x(k-1))/(2*T);     %y(2) through y(K-1)
y(K)=(x(K)-x(K-1))/T;           %y(K)
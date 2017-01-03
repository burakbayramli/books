function y=reconst(x,t,T)
% y=reconst(x,t,T)
%
% x is a vector of regularly spaced samples.
% t is a vector of time values in seconds which need not be
%   regularly spaced.
% T is the time step in x. T=1 if not specified.
% y is the Fourier series reconstruction of x, such that y(k)
%   is associated with time t(k) in seconds.
% 
% Example: let x=[x0 x1 x2 x3], t=[0.05,0.25], and T=0.1. Then
%    y(1) is an interpolated point halfway between x0 and x1;
%    y(2) is an interpolated point halfway between x2 and x3.
% 
% See also: resamp

x=row_vec(x);
t=row_vec(t);
N=length(x);
Ny=length(t);
if nargin<3,
    T=1;
end
% Implement equation (3.44) in the text.
X=fft(x);
y=zeros(1,Ny);
% To satisfy the sampling thm., all components must be < N/2.
m=1:fix((N-1)/2);
for n=1:Ny,
   arg=m'*(2*pi/T)*t(n)/N;
   sum1=real(X(m+1))*cos(arg);
   sum2=imag(X(m+1))*sin(arg);
   y(n)=(X(1)+2*(sum1-sum2))/N;
end
function y=integral(x,fs)
% y=integral(x,fs)
% 
% x =input vector or array.
%    If x is an array, each column of x is integrated.
% fs=sampling frequency (samples/s).
% y =integral of x at the sample points.
%    Unlike other methods (trapezoidal, splines, etc.), y is
%    exact, provided x has no frequencies at or above fs/2.

T=1/fs;                             %T =time step (s)
[N,nc]=size(x);
if N==1,                            %if x is a row vector,
    x=x';                           %make x a column vector
    [N,nc]=size(x);                 %N=length of each vector
end
% Check for errors.
if N<4,
   error('vector(x) must have at least 4 elements.');
end
y=zeros(N,nc);                    	%output vector or array
k=(0:N-1)';                        	%sample indices
for col=1:nc,                     	%integrate each col. of x
    x0=x(:,col);                  	%initial vector, x
    r=linspace(x0(1),x0(N),N)';   	%ramp connecting x0(1) and x0(N)
    u=x0-r;                        	%x0 with ramp removed
    v=[u; -u(N-1:-1:2)];          	%extended version of v0
    V=fft(v);                       %only the imag. parts are nonzero
    m=(1:N)';                       %frequency indices
    b=-imag(V(m+1))/(N-1);        	%Fourier sine series coeff.
    c=b*(N-1)*T./(m*pi);            %integral cosine coeff.
    n=(0:N-1);                      %time indices
    Iu=(c'*(1-cos(pi*m*n/(N-1))))'; %integral of u
    Ir=n'.*(r(n+1)+r(1))*T/2;       %integral of ramp
    t=(0:N-1)'*T;                   %time vector
    s=(x0(N)-x0(1))/((N-1)*T);    	%slope dr/dt
    y(:,col)=x0(1)*t+.5*s*t.^2+Iu;
end

    
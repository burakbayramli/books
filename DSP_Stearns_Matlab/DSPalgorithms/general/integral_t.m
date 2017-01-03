function y=integral_t(x,fs)
% y=integral_t(x,fs)
% 
% y =trapezoidal integration of x
%    in a column vector or array of column vectors.
% fs =sampling frequency (samples/s).
%
% Note: if x is an array, each column of x is integrated.

T=1/fs;                             %T =time step (s)
[N,nc]=size(x);
if N==1,                            %if x is a row vector,
    x=x';                           %make x a column vector
    [N,nc]=size(x);                 %N=length of each vector
end
% Check for errors.
if N<2,
   error('vector(x) must have at least 2 elements.');
end
y=zeros(N,nc);                      %output vector or array
for col=1:nc,                       %integrate each col. of x
    for i=1:N-1,
        y(i+1)=y(i)+(x(i)+x(i+1))*T/2;
    end
end
end
    
    
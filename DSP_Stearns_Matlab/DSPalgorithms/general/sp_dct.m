function X=sp_dct(x);
% X=sp_dct(x)
%
% X is the Discrete Cosine Transform (DCT) of x.
% Let N =length(x). Then,
% If x is a vector, X(0:N-1) is computed as follows:
%
% X(m)=c(m)*sum from{n=0}to{N-1}{x(n)*cos((2n+1)*m*pi/(2*N))};
%      c(m)=sqrt(1\N) if m=0 and sqrt(2/N) if 0<m<=N-1.
%
% If x is an array, X becomes an array of column DCT's.
% See also: sp_idct, sp_dst

[N,Nc]=size(x);
if N==1
   x=x';                % change row vector to column
   [N,Nc]=size(x);
end
if N<4
   error('x must have at least 4 elements.');
end
m=[0:N-1]';
Y2=fft([x; zeros(N,Nc)]);
phase=exp(-j*m*pi/(2*N));
c=[1/sqrt(2); ones(N-1,1)]*sqrt(2/N);
X=real([c.*phase*ones(1,Nc)].*Y2(1:N,1:Nc));

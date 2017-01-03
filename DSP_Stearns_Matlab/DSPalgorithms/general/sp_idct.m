function x=sp_idct(X);
% x=sp_idct(X)
%
% x is the N-point inverse Discrete Cosine Transform (DCT) of X,
% where N =# components in transform.
% If X is a vector, x(0:N-1) is computed as follows:
%
% x(m)=c(m)*sum from{n=0}to{N-1}{X(n)cos((2n+1)m pi/2N)};
%      c(m)=sqrt(1\N) if m=0 and sqrt(2/N) if 0<m<=N-1.
%
% If X is an array, x becomes an array of column inverse DCT's.
% See also: sp_dct
[N,Nc]=size(X);
if N==1,
    X=col_vec(X);           % Change row vector to column
    [N,Nc]=size(X);         % N =# elements in column
end
m=[0:N-1]';
phase=exp(j*m*pi/(2*N));
c=[1/sqrt(2); ones(N-1,1)]*sqrt(2/N);
X2=[c.*phase*ones(1,Nc).*X; zeros(N,Nc)];
x2=real(2*N*ifft(X2));
x=x2(1:N,1:Nc);

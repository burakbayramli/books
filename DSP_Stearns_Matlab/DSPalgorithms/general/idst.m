function x=idst(X)
%x=idst(X)
%
% x is the inverse Discrete Fourier Transform (DST) of X computed in the 
% form described in function dst.m and also in Chapter 10, Section 10.6 of
% "Digital Signal Processing with Examples in Matlab."
%
% If X(1:K) is a vector, x(1:K) is the vector such that X=dst(x).
% If X(1:K,:) is an array, x becomes an array of column inverse DST's.

%assure x vectors are in N columns with K>=3 samples each
[K,N]=size(X);
if K==1,                                    %if X is a row vector
    X=X';                                   %change X to s col. vector
    [K,N]=size(X);                          %if N>1, X is an array
end
if K<3,
    error('The DST requires at least 3 samples.');
end

%compute the DST using the Matlab fft function
c=sqrt(2/(K+1));                            %DST coefficient
m=(0:K-1)';                                 %DST index vector
c2j=c*(-1).^(m+1)/(2*j);                    %multipliers in the DST formula
x=zeros(K,N);                               %DST array
for n=1:N,                                  %do for each column
    S=[0; X(K:-1:1,n); 0; -X(:,n)];         %odd-periodic version of X
    s=fft(S);
    IDST=c2j.*s(m+2);                       %index begins at 1 in Matlab
    x(:,n)=real(IDST);
end


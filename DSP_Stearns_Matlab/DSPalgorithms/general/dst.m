function X=dst(x)
%X=dst(x)
%
%X is the ordinary Discrete Sine Transform (DST) of the real vector x.
%The DST version used here is described in Chapter 10 of
%"Digital Signal Processing with Examples in Matlab."
%
% If x(1:K) is a vector, X(1:K) is the vector of DST components.
% If x(1:K,:) is an array, X becomes an array of column DST's.

%assure x vectors are in N columns with K>=3 samples each
[K,N]=size(x);
if K==1,                                    %if x is a row vector
    x=x';                                   %change x to s col. vector
    [K,N]=size(x);                          %if N>1, x is an array
end
if K<3,
    error('The DST requires at least 3 samples.');
end

%compute the DST using the Matlab fft function
c=sqrt(2/(K+1));                            %DST coefficient
m=(0:K-1)';                                 %DST index vector
c2j=c*(-1).^(m+1)/(2*j);                    %multipliers in the DST formula
X=zeros(K,N);                               %DST array
for n=1:N,                                  %do for each column
    s=[0; x(K:-1:1,n); 0; -x(:,n)];         %odd-periodic version of x
    S=fft(s);
    DST=c2j.*S(m+2);                        %index begins at 1 in Matlab
    X(:,n)=real(DST);
end

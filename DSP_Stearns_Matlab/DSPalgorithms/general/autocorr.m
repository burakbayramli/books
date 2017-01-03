function phi=autocorr(x,type,N)
% phi=autocorr(x,type,N)
%
% phi(1:N)=autocorrelation (column) vector of x(1:K)
%          with N elements, each computed as follows:
%   phi(n)=sum from k=1 to K {x(k)*x(k+n-1)}/K,
%          computed for n=0,1,...,N-1.
% 
% type can have two values: 0 or 1:
%      0 implies x is extended with zeros.
%      1 implies x is extended periodically.
%
% N   =length of phi =number of values of phi to compute.
% See also: autocorr_mat, crosscor, autocovar, crosscovar

% Check for errors.
K=length(x);
if(nargin~=3)
%error('Function autocorr must have three arguments.');
elseif(type~=0 & type~=1),
   error('Second argument must be either 0 or 1.');
elseif(K<4)
   error('Signal vector must have at least 4 elements.');
end

% Compute phi up to one period.
x=x(:);                                 % Col. vector
if(type==0),                            % If type=0, pad with zeros
    x=[x; zeros(K,1)];
else
    x=[x; x];                           % If type=1, pad with 2nd period
end
if K>128 & N>12*log2(2*K),              % If N is large, use fft's
    X=fft(x);
    phi=ifft(X.*conj(X))/K;
    if(type==1),
        phi=phi/2;                      % Divide by 2 if fft's and type=1
    end
else                                    % N is small, use sum of products
    phi=zeros(N,1);
    for n=1:min(K,N),
        phi(n)=real(x(1:K)'*x(n:K+n-1)/K);
    end
end

% If N>K, extend phi to length N depending on type.
if(N>K),
    for n=K+1:N,
        if(type==0),
            phi(n)=0;
        else
            phi(n)=phi(rem(n-1,K)+1);
        end
   end
end
phi=phi(1:N);

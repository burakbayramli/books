function phi=crosscorr(x,y,type,N)
% phi=crosscorr(x,y,type,N)
%
% phi(1:N)=cross correlation (row) vector of x and y.
%          with N elements, each computed as follows:
%   phi(n)=(sum from k=1 to K {x(k)*y(k+n-1)})/K;
%          where K =length(x) and n=[1:N].
% 
% type can have two values: 0 or 1:
%      0 implies y is extended with zeros if necessary.
%      1 implies y is extended periodically if necessary.
%
% N   =length of phi =number of values of phi to compute.
% See also: autocorr, crosscovar, autocovar

% Check for errors.
K=length(x);
if(nargin~=4)
   error('Function crosscorr must have four arguments.');
elseif(type~=0 & type~=1),
   error('Third argument (type) must be either 0 or 1.');
elseif(K<4)
   error('Vector x must have at least 4 elements.');
end

% Extend both vectors x and y to length L=2*(N+K).
x=x(:)';                            %row vectors
y=y(:)';
L=2*(N+K);
x1=[x, zeros(1,L-K)];               % extend x with zeros to length L
y=y(1:min(L,length(y)));            % remove any unneeded end of y
if type==0,
    y1=[y, zeros(1,L-length(y))];   % extend y with zeros to length L
else
    i=mod([0:L-1],length(y))+1;
    y1=y(i);                        % extend y periodically to length L
end

% Cross-correlate using transforms - see text p.191.
phi=ifft(conj(fft(x1)).*fft(y1))/K;
phi=real(phi(1:N)');


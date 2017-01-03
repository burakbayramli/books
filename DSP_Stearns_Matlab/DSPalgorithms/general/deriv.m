function [dx,b]=deriv(x,N,T,vc)
% [dx,b]=deriv(x,N,T,vc)
%
%Computes the derivative, dx/dt, of a continuous function, x(t).
%Note: If you are working with a discrete vector, x(k), and wish to take
%      the derivative with respect to k, then set T=1.
%
%Inputs:  x  =input vector or array of vectors in columns
%         N  =length of differentiator - see DSP text.
%             N must be odd and at least 5. If even, N is increased by 1.
%         T  =time step between elements of x (see above).
%         vc =cutoff frequency in Hz-s. If omitted, vc is set to 0.5 Hz-s.
%
%Outputs: dx =derivative of x
%         b  =weight vector of differentiating filter
%
[nr,nc]=size(x);
% Check for errors etc.
if N<5,
    error('N must be at least 5.');
elseif rem(N,2)==0,
    N=N+1;
    fprintf('Warning: N is increased to%4.0f\',N);
elseif nr~=1 & nc~=1,
    error('x must be a vector or an array.');
end
if nargin<4,
    vc=0.5;
end

% Derive the weights. See text, p.128, eq. 5.22.
L=(N-1)/2;
h=zeros(N,1);
k=0:L-1;
costerm=(2*pi*vc/T)*cos((k-L)*2*pi*vc)./((k-L)*pi);
sinterm=sin(2*pi*(k-L)*vc)./((k-L).^2*pi*T);
h(1:L)=costerm-sinterm;
h(N:-1:L+2)=-h(1:L);

% Apply a Kaiser window.
w=window(N,'kaiser',2*sqrt(2)*pi);      %Blackman-like Kaiser window
b=h.*w';                                %filter weights

%filter
dx=filter(b,1,x);                      %filtered signal

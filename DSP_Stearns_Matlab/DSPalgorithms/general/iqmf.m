function x=iqmf(y,N,window)
% x=iqmf(y,N,window)
%
% Single-stage inverse transform - reverses qmf transform.
% 
% Inputs:
%   y(:,2) =low/high frequency vector - output of qmf.
%   N      =number of weights used in execution of qmf.
%   window =window used in qmf.
%
% Output:
%   x = recovered signal (input to qmf) in a column vector.
%       The length of x is twice that of each column of y.
%       (x is left-shifted to remove the qmf delay.
%        The final N-1 samples of x are zeros.)
%
% See also: qmf, wavelet1, wavelet2, iwavelet1, iwavelet2

N=2*floor(N/2)+1;                       % N must be odd
[K,nc]=size(y);
if(nc~=2)
	error('Input to iqmf must have 2 columns.');
end
x=zeros(2*K,1);
b=[1 1]'*fir_weights(N,1,window,.25);   % Lowpass weights
b(2,:)=b(1,:).*(-1).^[1:N];
for i=1:2,
	u=zeros(2*K,1);
	u(1:2:2*K-1)=y(:,i);
	x=x+filter(b(i,:),.5,u);            % recovered vector, x(1:2*K)
end
shft=N-1;                               % x will be shifted left N-1 samples
x=[x(1+shft:2*K); zeros(shft,1)];       % Append zeros so length(x) =2*K
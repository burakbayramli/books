function y=qmf(x,N,window)
% y=qmf(x,N,window)
%
% Single-stage transform using two QMF (Quadrature Mirror
%   Filter) channels with cutoff at 0.25 Hz-s.
% Inputs:
%    x      =input signal vector (an array is converted to a vector).
%	 N      =number of weights in linear-phase FIR filters. Must be odd.
%	 window =1 (boxcar),  2 (tapered),  3(tent)  4 (hanning),
%           5 (hamming), 6(blackman), or 7(kaiser) 
%           (In the last case windo is a vector=[7,beta], 
%           with beta in range [4,9].) 
% Output:
%   y(:,1) =low-freq. down-sampled vector.
%   y(:,2) =high-freq. down-sampled vector.
% See also: iqmf, wavelet1, wavelet2

x=col_vec(x);                           % x must be a vector
N=2*floor(N/2)+1;                       % N must be odd
K=fix(length(x)/2);                     % K =half-length of x
y=zeros(K,2);
b=[1 1]'*fir_weights(N,1,window,.25);   % lowpass weights
b(2,:)=b(1,:).*(-1).^(1:N);             % hipass weights in b(2,:)
for i=1:2,
	u=filter(b(i,:),1,x);
	y(:,i)=col_vec(u(1:2:2*K));
end

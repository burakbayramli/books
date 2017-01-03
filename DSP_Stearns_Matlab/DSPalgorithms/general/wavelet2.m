function [y,p]=wavelet2(x,Nbands,Nwts,window)
% [y,p]=wavelet2(x,Nbands,Nwts,window)
%
% Wavelet transform 2: octave bands using QMF stages.
% 
% Inputs:
%   x(1:K) =input signal (converted to a vector).
%            K should be a multiple of 2^(Nbands-1) or x will be truncated. 
%   Nbands =number of frequency bands in the decomposition.
%	Nwts   =number of weights in FIR filters. Should be odd.
%	window =1 (boxcar),  2 (tapered),  3(tent)  4 (hanning),
%           5 (hamming), 6(blackman), or 7(kaiser) 
%           (In the last case windo is a vector=[7,beta], 
%           with beta in range [4,9].) 
% Output:
%	 y =output vector consisting of low to high bands.
%       Refer to text, fig. 10.26. Suppose length(x)=K=64 and Nbands=4. 
%       Then y(1:8)=xLLL, y(9:16)=xLLH, y(17:32)=xLH, and y(33:64)=xH.
%    p = avg power, E(y^2), in each band.
%
% See also: iwavelet2, wavelet1, iwavelet1, qmf, iqmf
Nstgs=Nbands-1;                         %# decomposition stages
if Nbands<2,
    error('Nbands must be at least 2.');
end
x=col_vec(x);                           %x must be a vector
N=2*floor(Nwts/2)+1;                    %filter size must be odd
M=fix(length(x)/2^Nstgs);               %M =length of shortest output sig.
if M<4 | 2*M-Nwts<4,                    %check if x is large enough
    error('Data vector is too short, or filter is too long.')
end
K=M*2^Nstgs;                            %K =length(input)=length(y)
L=K;                                    %L = stage signal length
y=x(1:K);                               %y is initially x(1:K)
p=zeros(Nbands,1);                      %p will be E(y^2) in each band
for stg=Nstgs:-1:1,                     %seg =sement # in y
    y(1:L)=qmf(y(1:L),Nwts,window);     %y(1:L) =[low,high] bands
    p(stg+1)=mean(y(L/2+1:L).^2);       %avg. power in high band
    L=L/2;                              %y(1:L) becomes lower 1/2 of y(1:L)
end                                     %finally, y(1:L) =xLLLL...L
p(1)=mean(y(1:L).^2);                   %avg. power in lowest band


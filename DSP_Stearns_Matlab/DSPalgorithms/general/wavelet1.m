function y=wavelet1(x,Nstgs,Nwts,window)
% y=wavelet1(x,Nstgs,Nwts,window)
%
% Wavelet transform 1: equal bands using QMF stages.
% 
% Inputs:
%   x(1:K) =input signal (converted to a vector).
%            K should be a multiple of 2^Nstgs, or x will be truncated. 
%   Nstgs  =number of stages in the decomposition.
%           (This function produces 2^Nstgs vectors in output y.)
%	Nwts   =number of weights in FIR filters. Should be odd.
%	window =1 (boxcar),  2 (tapered),  3(tent)  4 (hanning),
%           5 (hamming), 6(blackman), or 7(kaiser) 
%           (In the last case windo is a vector=[7,beta], 
%           with beta in range [4,9].) 
% Output:
%	 y =down-sampled vectors. y(:,1)=low freq., Y(:,2)=next, etc.
%       y is M x Ns, where Ns=2^Nstgs, and M=fix(length(x)/Ns).
%
% Note: in this version, the length of x should be a multiple
% of 2^Nstgs. If not, the remainder at the end of x is not processed.
% See also: iwavelet1, wavelet2, iwavelet2, qmf, iqmf
x=col_vec(x);                           % x must be a vector
N=2*floor(Nwts/2)+1;                    % filter size must be odd
Ns=2^Nstgs;                             % Ns =# output signals
M=fix(length(x)/Ns);                    % M =length of each output sig.
if M<4,                                 % check if x is large enough
    error('Data vector is too short.')
end
K=M*Ns;                                 % K =input signal length
y=qmf(x(1:K),N,window);                 % u =first stage output
stg=2;
while stg <= Nstgs                      % for stages 2,...,Nstgs
    Nf=2^(stg-1);                       % Nf =# filters in this stage
    Nb=2*Nf;                            % Nb =# bands out of this stage
    u=zeros(K/Nb,Nb);                   % stage output array
    for i=1:Nf,
        g=qmf(y(:,i),Nwts,window);      % qmf output (K/2^Nf x 2)
% Note: if i is even, we are processing a decimated (aliased) hipass
% signal, so we switch the columns of the qmf output (g).
        if 2*fix(i/2) == i,
            g=[g(:,2),g(:,1)];          % switch the output bands
        end
        u(:,2*i-1:2*i)=g;               % store bands in cols. of u
    end
    y=u;                                % u becomes y for next stage
    stg=stg+1;
end
    
    

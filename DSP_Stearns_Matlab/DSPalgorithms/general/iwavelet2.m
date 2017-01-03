function x=iwavelet2(y,Nbands,Nwts,win)
% x=iwavelet2(y,Nbands,Nwts,win)
%
% Inverse wavelet transform - recovers wavelet2 input vector.
% 
% Inputs:
%   y      =output vector with octal bands produced by wavelet2. 
%           for the format of y, type "help wavelet2".
%   Nbands  =number of bands in octal-band decomposition.
%   Nwts   =number of weights used in execution of qmf.
%   win    =window used in qmf.
%
% Output:
%   x = recovered signal (input to wavtelet2) in a column vector.

% See also: wavelet2, wavelet1, iwavelet1, qmf, iqmf

Nstgs=Nbands-1;
if Nbands<2,
    error('Nbands must be at least 2.');
end

K=length(y);                            %K =length of y.
L=fix(K/(2^Nstgs));                     %L =length of bands 1 & 2

% Check for errors.
if K~=L*(2^Nstgs),
    error('Input error: length(y) must be a multiple of 2^Nstgs.');
end

% Decode using iqmf at each stage.
for stg=Nstgs:-1:1,                     %for stages Nstgs,...,2,1
    u=[y(1:L),y(L+1:2*L)];              %input to iqmf
    y(1:2*L)=iqmf(u,Nwts,win);          %y(1:2*L)=recovered low band
    L=2*L;                              %L =twice previous L
end
x=y;                                    %x =recovered signal vector

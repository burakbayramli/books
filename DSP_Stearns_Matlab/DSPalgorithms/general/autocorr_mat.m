function Phi=autocorr_mat(x,type,N)
% Phi=autocorr_mat(x,type,N)
%
% Phi(N,N)=NxN Toeplitz autocorrelation matrix of x(1:K)
%          with elements as defined in function autocorr;
%          that is, elements NOT DIVIDED BY length(x).
% 
% type can have two values: 0 or 1:
%      0 implies x is extended with zeros.
%      1 implies x is extended periodically.
%
% N    =size of Phi.
% See also: autocorr, crosscor, autocovar, crosscovar

% Use "autocorr" to get the correlation (column) vector.
phi=autocorr(x,type,N);
% Create the index matrix and then compute Phi.
q=[phi(N:-1:2); phi];
ix1=[0:N-1]'*ones(1,N);
ix2=ones(N,1)*[N:-1:1];
index_mat=ix1+ix2;
Phi=q(index_mat);
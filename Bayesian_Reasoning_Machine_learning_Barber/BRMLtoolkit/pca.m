function [Y,E,L,m,Xtilde]=pca(X,varargin)
%PCA Principal Components Analysis
% [Y,E,L,m,Xtilde]=pca(X,<M>,<usemean>,<return only principal solution>)
% Inputs:
% X : each column of matrix X contains a datapoint
% M : the number of principal components to retain (if missing M=size(X,1))
% usemean : set this to 1 to use a mean for the reconstruction
% return only principal solution : set to 1 to return only the M leading eigenvectors and eigenvalues
% Outputs:
% Y : coefficients
% E : eigenvectors
% L : eigenvalues
% m : mean of the data
% Xtilde : reconstructions using M components
if isempty(varargin)
    M=size(X,1);
else
    M=varargin{1};
end
if nargin>=3; usemean=varargin{2}; else usemean=1; end
N=size(X,2);
m = usemean*mean(X,2);
X0 = X - repmat(m,1,N); % centre the data
[E D]=svd(X0,0);
L = (diag(D).^2)/(N-1);
if M<N
    Y = E(:,1:M)'*X0;
    Xtilde = E(:,1:M)*Y + repmat(m,1,N);
else
    Y = E'*X0;
    Xtilde = E*Y + repmat(m,1,N);
end
if nargin==4; if varargin{3}; E=E(:,1:M); L=L(1:M); end; end
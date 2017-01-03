function W=CanonVar(X,K,varargin)
%CANONVAR  Canonical Variates (no post rotation of variates)
% W=CanonVar(X,K,<shrinkage>)
%
% Inputs:
% X : cell array of data matrices in which each column contains a
% datapoint. X{c} contains the data for class c
% K : dimension of the projection
%
% Output:
% W : the projection matrix
% 
% see demoCanonVarDigits.m

% Note that this is not optimally coded
if isempty(varargin); shr=0; else shr=varargin{1}; end
nclasses = length(X); 
D = size(X{1},1); % dimension of data
B = zeros(size(cov((X{1})')));
for c = 1: nclasses
   n(c) = size(X{c},2);
   m(:,c) = mean(X{c},2);
   B = B + n(c)*cov((X{c})'); 
   t(:,c)=n(c)*m(:,c);
end
B=(1-shr)*B+shr*eye(D);
M=sum(t,2)/sum(n);
XX=[];
for c = 1:nclasses
     A(:,c) =sqrt(n(c))*(m(:,c)-M);   
     XX=[XX X{c}]; % all the data, regardless of class
end
Q=orth(XX); % basis to represent the solution
AA=Q'*A*A'*Q; % modified between scatter
B=Q'*B*Q; % modified within scatter
cB=chol(B); 
invcB=inv(cB);
%[Wt evals]=eigs(invcB'*AA*invcB,K);
[Wt S V]=svds(invcB'*AA*invcB,K);
W=Q*(Wt(:,1:K));
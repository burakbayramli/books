function [bsdata, indices]=block_bootstrap(data,B,w)
% Implements a circular block bootstrap for bootstrapping stationary,
% dependant series
% 
% USAGE:
%     [BSDATA, INDICES]=block_bootstrap(DATA,W,B);
% 
% INPUTS:
%     DATA   - T by 1 vector of data to be bootstrapped
%     B      - Number of bootstraps
%     W      - Block length
% 
% OUTPUTS:
%     BSDATA  - T x B matrix of bootstrapped data
%     INDICES - T by B matrix of locations of the original BSDATA=DATA(indexes);
% 
% COMMENTS:
%     To generate bootstrap sequences for other uses, such as bootstrapping
%     vector processes, simple set DATA to (1:N)' 

% Author: Kevin Sheppard
% kevin.sheppard@economics.ox.ac.uk
% Revision: 2    Date: 12/31/2001


%%%%%%%%%%%%%%%%%%%%%%%%%
% Input Checking
%%%%%%%%%%%%%%%%%%%%%%%%%
if nargin~=3
    error('3 inputs required')
end
% Get the length of the data
[t,k]=size(data);
if k>1
    error('DATA must be a column vector')
end
if t<2 
    error('DATA must have at least 2 observations.')
end
if ~isscalar(w) || w<1 || floor(w)~=w
    error('W must be a positive scalar integer')
end
if ~isscalar(B) || B<1 || floor(B)~=B
    error('B must be a positive scalar integer')
end
%%%%%%%%%%%%%%%%%%%%%%%%%
% Input Checking
%%%%%%%%%%%%%%%%%%%%%%%%%

% Make it easy to do the circular bootstrap
data=[data;data(1:w-1)];
% Compute the number of blocks needed
s=ceil(t/w);
% Generate the starting points
Bs=floor(rand(s,B)*t)+1;
indices=zeros(s*w,B);
index=1;
% Adder is a variable that needs to be added each loop
adder=repmat((0:w-1)',1,B);
for i=1:w:t
    indices(i:(i+w-1),:)=repmat(Bs(index,:),w,1)+adder;
    index=index+1;
end
indices=indices(1:t,:);
bsdata=data(indices);


    









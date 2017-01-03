function pnew=condp(pin,varargin)
%CONDP Make a conditional distribution from an array
% pnew=condp(pin,<distribution indices>)
%
% Input : pin  -- a positive matrix pin
% Output:  matrix pnew such that sum(pnew,1)=ones(1,size(p,2))
%
% The optional specifies which indices form the distribution variables.
% For example:
% r=rand([4 2 3]); p=condp(r,[3 1]);
% p is now an array of the same size as r, but with sum(sum(p,3),1) equal
% to 1 for each of the dimensions of the 2nd index.
m=max(pin(:));
if m>0 
    p = pin./m;
else
    p=pin+eps;% in case all unnormalised probabilities are zero
end
if nargin==1
 pnew=p./repmat(sum(p,1),size(p,1),1);
else
    allvars=1:length(size(pin));
    sizevars=size(pin);
    distvars=varargin{1};
    condvars=setdiff(allvars,distvars);
    newp=permute(pin,[distvars condvars]);
    newp=reshape(newp,prod(sizevars(distvars)),prod(sizevars(condvars)));
    newp=newp./repmat(sum(newp,1),size(newp,1),1);
    pnew=reshape(newp,sizevars([distvars condvars]));
    pnew=ipermute(pnew,[distvars condvars]);
end
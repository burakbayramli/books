function pnew=normp(p)
%NORMP Make a normalised distribution from an array
% pnew=normp(p)
%
% Input : p -- a positive array
% Output:  matrix pnew such that sum(pnew(:))=1
p = p+eps; % in case all unnormalised probabilities are zero
pnew=p./sum(p(:));
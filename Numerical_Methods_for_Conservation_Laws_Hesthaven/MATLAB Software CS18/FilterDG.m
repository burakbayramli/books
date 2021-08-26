function [F] = FilterDG(m,mc,p,V)
% function [F] = FilterDG(m,mc,p,V)
% Purpose : Initialize DG filter matrix of size m.
%           Order of exponential filter is (even) p with cutoff at mc;
filterdiag = ones(m+1,1); alpha = -log(eps);

% Initialize filter function
for i=mc:m
    filterdiag(i+1) = exp(-alpha*((i-mc)/(m-mc))^p);
end;
F = V*diag(filterdiag)*inv(V);
return;
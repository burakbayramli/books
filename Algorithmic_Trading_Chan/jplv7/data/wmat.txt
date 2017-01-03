% documentation for wmat.dat

% 1st order contiguity matrix for
% Anselin's Columbus crime dataset
% stored in sparse matrix format [i, j, s] = find(W);
% so that W = sparse(i,j,s); reconstructs the 49x49 matrix
% NOTE: already row-standardized

load wmat.dat;
W = sparse(wmat(:,1),wmat(:,2),wmat(:,3));


% DM = DistanceMatrixRepmat(dsites,ctrs)
% Forms the distance matrix of two sets of points in R^d,
% i.e., DM(i,j) = || datasite_i - center_j ||_2.
% Input
%   dsites: Mxd matrix representing a set of M data sites in R^d
%              (i.e., each row contains one s-dimensional point)
%   ctrs:   Nxd matrix representing a set of N centers in R^d
%              (one center per row)
% Output
%   DM:     MxN matrix whose i,j position contains the Euclidean
%              distance between the i-th data site and j-th center
  function DM = DistanceMatrixRepmat(dsites,ctrs)
  [M,d] = size(dsites); [N,d] = size(ctrs);
  DM = zeros(M,N);
  % Accumulate sum of squares of coordinate differences
  for l=1:d
     %%% Uses less memory
     DM = DM + (repmat(dsites(:,l),1,N)-repmat(ctrs(:,l)',M,1)).^2;
  end
  DM = sqrt(DM);

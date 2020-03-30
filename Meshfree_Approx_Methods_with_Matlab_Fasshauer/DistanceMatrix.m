% DM = DistanceMatrix(dsites,ctrs)
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
% Algorithm was essentially suggested by Paritosh Mokhasi

  function DM = DistanceMatrix(dsites,ctrs)
  M = size(dsites,1); N = size(ctrs,1);
% Algorithm is based on expanding the terms and computing each term
% explicitly, i.e.  
%         (x1 - x2)^2 = x1.^2 + x2.^2 - 2*x1*x2;
  DM = repmat(sum(dsites.*dsites,2),1,N) - ...
       2*dsites*ctrs' + ...
       repmat((sum(ctrs.*ctrs,2))',M,1);
  DM = sqrt(DM);
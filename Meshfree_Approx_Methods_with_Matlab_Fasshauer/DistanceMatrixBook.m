% DM = DistanceMatrixBook(dsites,ctrs)
% Forms the distance matrix of two sets of points in R^s,
% i.e., DM(i,j) = || datasite_i - center_j ||_2.
% Input
%   dsites: Mxs matrix representing a set of M data sites in R^s
%              (i.e., each row contains one s-dimensional point)
%   ctrs:   Nxs matrix representing a set of N centers in R^s
%              (one center per row)
% Output
%   DM:     MxN matrix whose i,j position contains the Euclidean
%              distance between the i-th data site and j-th center
  function DM = DistanceMatrixBook(dsites,ctrs)
  [M,s] = size(dsites); [N,s] = size(ctrs);
  DM = zeros(M,N);
  % Accumulate sum of squares of coordinate differences
  % The ndgrid command produces two MxN matrices:
  %   dr, consisting of N identical columns (each containing
  %       the d-th coordinate of the M data sites)
  %   cc, consisting of M identical rows (each containing
  %       the d-th coordinate of the N centers)
  for d=1:s
     [dr,cc] = ndgrid(dsites(:,d),ctrs(:,d));
     DM = DM + (dr-cc).^2;
  end
  DM = sqrt(DM);
function [ncan, cands, disall, ipos] = search(Chic, MaxCan, n, a, D, L)
% SEARCH   finds 'MaxCan' integer vectors whose distances to the real
%          vector 'a' are minimal in the metric of Q = transpose(L) D L.
%          Only integer vectors with a distance less than sqrt(Chic)
%          are regarded.
%
%          The search for gridpoints inside the ambiguity search ellipsoid
%          is a sequential conditional adjustment upon the ambiguities.
%          The search starts by conditioning the last ambiguity a_n to
%          an integer, then the one-but-last a_{n-1} etc., until
%             - the squared norm grows too large (out of the ellipsoid)
%             - an integer for a_1 is found: a full integer vector is
%               encountered (a gridpoint inside the ellipsoid)
%          then it goes back to some previous (towards a_n) ambiguity
%          and considers another integer for it.

% Chic     Chi squared
% MaxCan   number of minimum integer vectors required
% n        dimension of matrix
% a        vector of real valued estimates \hat{a} (float
%                        solution)
% D        diagonal matrix
% L        lower triangular matrix: Q^{-1} = L D L^T
%          although L is lower triangular, in this example
%          program L is stored column-wise in a 2-dimensional
%          array, to avoid the necessity of a dedicated
%          storage scheme.
% lef      vector of length  n
% left     vector of length  n+1
% right    vector of length  n+1
% dist     difference between the integer tried and \hat{a}_i,
%          length n
% endd     vector of length n
% dq       vector of length n
% ncan     number of integer vectors found
% cands    | 2-dimensional array to store the candidates
% disall   | according squared norms \hat{a}-\check{a}
% ipos     column number in 'cands' where the candidate
%          belonging to the minimum distance is stored

% Delft Geodetic Computing Centre/LGR, Paul de Jonge
% copyright by Delft University of Technology, Faculty of Geodesy

% Recoded into MATLAB by Kai Borre 12-04-96

disall = zeros(1,MaxCan);
cands = zeros(n,MaxCan);
tmax = 0;
imax = 0;

if MaxCan < 1
   error('ERROR in SEARCH: number of requested candidates < 1')
else
   if n < 2
      error('ERROR in SEARCH: dimension of system < 2')
   end
   
   ende = 'false';
   right = zeros(n+1,1);
   left = zeros(n+1,1);
   right(n+1,1) = Chic;
   for i = 1:n-1
      dq(i) = D(i+1)/D(i);
   end
   dq(n) = 1/D(n);
   
   ncan = 0;
   i = n+1;
   iold = i;
   
   while ende == 'false'
      i = i-1;                             % go a level deeper
      if iold <= i                         % we were here before
         lef(i) = lef(i) + L(i+1,i);       % only one dist is one bigger
      else
         lef(i) = 0;
         for j = i+1:n
            lef(i) = lef(i) + L(j,i)*dist(j,1);
         end
      end
      iold = i;                     % keep track of level
      
      right(i) = (right(i+1)-left(i+1)) * dq(i);
      reach = sqrt(right(i));      
      % delta=a(i)-reach-lef(i) is the left border
      % ceil (delta) is the left most integer in the interval
      % dist(i,1) is the distance of this left most integer to the a_hat      
      delta = a(i) - reach - lef(i);
      dist(i,1) = ceil(delta) - a(i);      
      if dist(i,1) > reach-lef(i)   % there is nothing at this level
                                    % so, ... back track
         [i, dist, left, ende] = backtrac(n, i, endd, dist, lef, left, ende);
      else
         endd(i) = reach - lef(i) - 1;     % set the right 'border'
         left(i) = (dist(i,1)+lef(i))^2;
      end      
      if i == 1
         [dist, endd, ncan, cands, tmax, imax, disall] = ...
            collects(n, MaxCan, D, lef, left, right, ...
            Chic, dist, endd, ncan, disall, cands, tmax, imax);
         [i, dist, left, ende] = backtrac(n, i, endd, dist, lef, left, ende);
      end
   end
   % the candidate vectors are stored and the index for the best
   % candidate is set (ipos)
   dminn = 1.e20;                 % some big number
   for i = 1:min(ncan, MaxCan)     
      if disall(i) < dminn
         dminn = disall(i);
         ipos = i;
      end
      cands(:,i) = cands(:,i) + a;
   end
end
% cands contains integer transformed reduced ambiguities
%%%%%%%%%%%%%%% end search.m  %%%%%%%%%%%%%%%%%%%%%%%%%%%

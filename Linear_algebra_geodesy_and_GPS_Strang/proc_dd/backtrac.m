function [i, dist, left, ende] = backtrac(n, i, endd, dist, lef, left, ende)
% BACKTRAC backtrack in the search tree; used in SEARCH

% n       dimension of matrix
% i       level in the tree
% endd    vector
% dist    difference between the integer tried and \hat{a}_i
% lef     vector
% left    vector
% ende    if 'true ', then search is done

% Delft Geodetic Computing Centre/LGR, Paul de Jonge
% copyright by Delft University of Technology, Faculty of Geodesy

% Recoded into MATLAB by Kai Borre 12-04-96

cand_n = 'false';     %  candidate at level n
c_stop = 'false';     %  criterion to stop the loop

while (c_stop == 'false') & (i < n)
   i = i+1;
   if dist(i) <= endd(i)
      dist(i) = dist(i) + 1;
      left(i) = (dist(i) + lef(i))^2;
      c_stop = 'true ';
      if i == n
         cand_n = 'true ';
      end
   end
end

if (i == n) & (cand_n == 'false')
   ende = 'true ';
end
%%%%%%%%%%%%% end backtrac.m  %%%%%%%%%%%%%%%%%%%%

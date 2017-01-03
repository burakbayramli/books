function [dist, endd, ncan, cands, tmax, imax, disall] = collects(n,...
                               MaxCan, D, lef, left, right,...
                               Chic, dist, endd, ncan,...
                               disall, cands, tmax, imax)
% COLLECTS collects integer vectors and corresponding
%          squared distances

% n         dimension of the system
% MaxCan    number of minimum integer vectors required
% D         diagonal matrix
% lef       vector
% left      vector
% right     vector
% Chic      Chi squared
% dist      difference between the integer tried and \hat{a}_n
% endd      vector
% ncan      number of integer vectors found
% cands     2-dimensional array to store the candidates
% disall    according squared norms \hat{a}-\check{a}
% tmax      the largest distance of the Min (ncan,MaxCan)
%           vectors with minimum distance found until now
% imax      position in disall/cands of the vector with the
%           largest distance of the Min (ncan,MaxCan) vectors
%           with minimum distance found until now

% Delft Geodetic Computing Centre/LGR, Paul de Jonge
% copyright by Delft University of Technology, Faculty of Geodesy

% Recode into MATLAB by Kai Borre 12-04-96

t = Chic - (right(1)-left(1)) * D(1);
endd(1) = endd(1) + 1;

% The following loop should be run through at least once
while  dist(1) <= endd(1)
   ncan = ncan + 1;
   if (ncan <= MaxCan)
      [imax, tmax, cands, disall] = stores(ncan, ncan,...
                                   t, dist, disall, cands, n);
   else
      if (t < tmax)
         [imax, tmax, cands, disall] = stores(MaxCan, imax,...
                                   t, dist, disall, cands, n);
      end
   end
   t = t + (2 * (dist(1)+lef(1)) + 1) * D(1);
   dist(1) = dist(1) + 1;
end
%%%%%%%%%%%%% end collects.m  %%%%%%%%%%%%%%%%%%%

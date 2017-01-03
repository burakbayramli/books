function [imax, tmax, cands, disall] = stores(ican, ipos, t, ...
                                            dist, disall, cands, n)
% STORES   Stores candidates and corresponding distances

% ican     Min (number of vectors found until now, MaxCan)
% ipos     position in disall/cands to put the new found vector
% t        distance of the new found vector
% dist     difference between the integer tried and \hat{a}_n
% disall   distance of the MaxCan integer vectors
% cands    2d-array to store the integer vectors
% n        dimension of the system (number of DD ambiguities)
% imax     position in disall/cands of the vector with the
%          largest distance of the ican vectors with minimum
%          distance found until now
% tmax     the largest distance of the ican vectors with
%          minimum distance found until now

% Delft Geodetic Computing Centre/LGR, Paul de Jonge
% copyright by Delft University of Technology, Faculty of Geodesy

% Rewritten for MATLAB by Kai Borre 12-04-96

cands(1:n,ipos) = dist;
disall(ipos) = t;
tmax = t;
imax = ipos;
for i = 1:ican
   if disall(i) > tmax
      imax = i;
      tmax = disall(i);
   end
end
%%%%%%%%%%%% end stores.m %%%%%%%%%%%%%%%%%%%

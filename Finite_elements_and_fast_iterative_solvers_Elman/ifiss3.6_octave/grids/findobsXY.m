function [II,JJ] = findobsXY(obs,X,Y,bndxy)
%FINDOBSXY finds coords of grid points inside obstacle 
%   [II,JJ] = findobsXY(obs,X,Y,bndxy)
%   input
%          obs        indices in bndxy of nodes determining object boundary
%          X          array of horizontal grid coordinates
%          Y          array of vertical grid coordinates 
%          bndxy      coordinates of nodes defining domain boundary
%          X and Y are the output of the Matlab function meshgrid
%   output
%          II         horizontal indices of mesh points interior to object
%          JJ         vertical indices of mesh points interior to object
%
%   IFISS function: HCE; 24 December 2009.
% Copyright (c) 2009 D.J. Silvester, H.C. Elman, A. Ramage;
% Code written by M. Wu 2009

II = [];
JJ = [];
if size(obs,1)>0
   for i = 1:size(obs,1)
       xl = min(bndxy(obs(i,:),1));
       xr = max(bndxy(obs(i,:),1));
       yb = min(bndxy(obs(i,:),2));
       yt = max(bndxy(obs(i,:),2));
       [ii,jj] = find(X>xl & X<xr & Y<yt & Y>yb);
       II = [II;ii];
       JJ = [JJ;jj];
   end
end

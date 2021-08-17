function [map] = ConstructMap(BCType, BCcode)

% function map = ConstructMap(BCType, BCcode);
% Purpose: Construct boundary map from the BCType table
% By Allan P. Engsig-Karup, 07-12-2006.

Globals2D;

% Determine which faces in which elements which have the specified BCs:
%   fids = face id's, eids = element id's;
[eids,fids] = find(BCType==BCcode);

% initialize length of new map
map = [];

for n = 1 : length(fids) % go over each boundary face of BCcode type
    map = [map (eids(n)-1)*Nfaces*Nfp+[ (fids(n)-1)*Nfp + [1:Nfp] ]];
end
return

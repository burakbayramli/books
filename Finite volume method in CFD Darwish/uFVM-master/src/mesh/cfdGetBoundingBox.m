function [minLimit_x, maxLimit_x, minLimit_y, maxLimit_y, minLimit_z, maxLimit_z] = cfdGetBoundingBox
%--------------------------------------------------------------------------
%
%  written by the CFD Group @ AUB, 2018
%  contact us at: cfd@aub.edu.lb
%==========================================================================
% Case Description:
%     Apply a check to the mesh, giving out some important quantitative and
%     qualitative statistics
%--------------------------------------------------------------------------

global Region;

% Domain cfdBounding box
maxLimit_x = -cfdBIG;
minLimit_x =  cfdBIG;

maxLimit_y = -cfdBIG;
minLimit_y =  cfdBIG;

maxLimit_z = -cfdBIG;
minLimit_z =  cfdBIG;


for iNode=1:Region.mesh.numberOfNodes
    maxLimit_x = max(Region.mesh.nodeCentroids(iNode,1), maxLimit_x);
    maxLimit_y = max(Region.mesh.nodeCentroids(iNode,2), maxLimit_y);
    maxLimit_z = max(Region.mesh.nodeCentroids(iNode,3), maxLimit_z);
    
    minLimit_x = min(Region.mesh.nodeCentroids(iNode,1), minLimit_x);
    minLimit_y = min(Region.mesh.nodeCentroids(iNode,2), minLimit_y);
    minLimit_z = min(Region.mesh.nodeCentroids(iNode,3), minLimit_z);
end
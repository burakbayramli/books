function cfdBound(theFieldName, lowerBound, iComponent)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function cfdBounds a field from bottom
%--------------------------------------------------------------------------

phi = cfdGetDataArray(theFieldName);

if size(phi,2)==3
    if nargin==2
        for i=1:3
            phi(:,i) = max(phi(:,i), lowerBound);
        end
    else
        phi(:,iComponent) = max(phi(:,iComponent), lowerBound);
    end
else
    phi = max(phi, lowerBound);
end

global Region;
Region.fluid.(theFieldName).phi = phi;
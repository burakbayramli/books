function phi_f = cfdInterpolateFromElementsToFaces(theInterpolationScheme, phi, mdot_f)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018 
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function interpolates a field phi to faces
%--------------------------------------------------------------------------

% Get field type
theNumberOfComponents = size(phi, 2);

% Get mesh info
numberOfElements = cfdGetNumberOfElements;
numberOfInteriorFaces = cfdGetNumberOfInteriorFaces;
numberOfFaces = cfdGetNumberOfFaces;
numberOfBFaces = cfdGetNumberOfBFaces;

owners = cfdGetOwnersSubArrayForInteriorFaces;
neighbours = cfdGetNeighboursSubArrayForInteriorFaces;

g_f = cfdGetFaceWeightsSubArrayForInterior;

if (strcmp(theInterpolationScheme, 'vanLeerV'))    % BODGE////////////////
    vol = [theMesh.elements.volume];
    for iComponent=1:theNumberOfComponents
        phi_f(:,iComponent) = (vol(owners)+vol(neighbours)).*phi(owners,iComponent).*phi(neighbours,iComponent)./(vol(neighbours).*phi(owners,iComponent)+vol(owners).*phi(neighbours,iComponent));
    end
elseif (strcmp(theInterpolationScheme, 'linearUpwind'))
    pos = zeros(size(mdot_f));
    pos(mdot_f>0) = 1;
    for iComponent=1:theNumberOfComponents
        phi_f(:,iComponent) = phi(owners,iComponent).*pos + phi(neighbours,iComponent).*(1 - pos);
    end
elseif (strcmp(theInterpolationScheme, 'linear'))
    for iComponent=1:theNumberOfComponents        
        phi_f(:,iComponent) = g_f.*phi(neighbours,iComponent) + (1 - g_f).*phi(owners,iComponent);
    end
else 
    error([theInterpolationScheme, ' interpolation scheme incorrect\n'])
end

%
% phi_f at cfdBoundary faces are simple set equal to the values of phi at cfdBoundary patch values
%
for iComponent=1:theNumberOfComponents
    phi_f(numberOfInteriorFaces+1:numberOfFaces,iComponent) = phi(numberOfElements+1:numberOfElements+numberOfBFaces,iComponent);
end

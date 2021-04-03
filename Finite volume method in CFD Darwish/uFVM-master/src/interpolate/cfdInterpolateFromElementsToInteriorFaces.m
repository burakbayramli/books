function phi_f = cfdInterpolateFromElementsToInteriorFaces(theInterpolationScheme, phi, mdot_f)
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

% Get info
owners_f = cfdGetOwnersSubArrayForInteriorFaces;
neighbours_f = cfdGetNeighboursSubArrayForInteriorFaces;
g_f = cfdGetFaceWeightsSubArrayForInterior;
volumes = cfdGetVolumesForElements;
theNumberOfInteriorFaces = cfdGetNumberOfInteriorFaces;

% Initialize face array
phi_f = zeros(theNumberOfInteriorFaces,theNumberOfComponents);

if (strcmp(theInterpolationScheme, 'vanLeerV'))    % BODGE////////////////
    for iComponent=1:theNumberOfComponents
        phi_f(:,iComponent) = (volumes(owners_f)+volumes(neighbours_f)).*phi(owners_f,iComponent).*phi(neighbours_f,iComponent)./(volumes(neighbours_f).*phi(owners_f,iComponent)+volumes(owners_f).*phi(neighbours_f,iComponent));
    end
elseif (strcmp(theInterpolationScheme, 'linearUpwind'))
    pos = zeros(size(mdot_f));
    pos(mdot_f>0) = 1;
    for iComponent=1:theNumberOfComponents
        phi_f(:,iComponent) = phi(owners_f,iComponent).*pos + phi(neighbours_f,iComponent).*(1 - pos);
    end
elseif (strcmp(theInterpolationScheme, 'linear'))    
    for iComponent=1:theNumberOfComponents        
        phi_f(:,iComponent) = g_f.*phi(neighbours_f,iComponent) + (1 - g_f).*phi(owners_f,iComponent);
    end
else 
    error([theInterpolationScheme, ' interpolation scheme incorrect\n'])
end
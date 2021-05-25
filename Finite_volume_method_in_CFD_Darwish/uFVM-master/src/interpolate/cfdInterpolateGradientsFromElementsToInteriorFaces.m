function grad_f = cfdInterpolateGradientsFromElementsToInteriorFaces(theInterpolationScheme, grad, phi, mdot_f)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018 
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function interpolates gradient to interior faces
%--------------------------------------------------------------------------

% Get field type
if nargin<3
    theNumberOfComponents = size(grad,3);
else
    theNumberOfComponents = size(phi,2);
end

% Get info
theNumberOfInteriorFaces = cfdGetNumberOfInteriorFaces;
owners_f = cfdGetOwnersSubArrayForInteriorFaces;
neighbours_f = cfdGetNeighboursSubArrayForInteriorFaces;
g_f = cfdGetFaceWeightsSubArrayForInterior;
CF = cfdGetFaceCFSubArrayForInterior;

% Initialize gradient array for interior faces
grad_f = zeros(theNumberOfInteriorFaces, 3, theNumberOfComponents);

% Calculate gradient
if strcmp(theInterpolationScheme,'linear') || strcmp(theInterpolationScheme,'Gauss linear')
    for iComponent=1:theNumberOfComponents
        grad_f(:,1,iComponent) = (1-g_f).*grad(neighbours_f,1,iComponent) + g_f.*grad(owners_f,1,iComponent);
        grad_f(:,2,iComponent) = (1-g_f).*grad(neighbours_f,2,iComponent) + g_f.*grad(owners_f,2,iComponent);
        grad_f(:,3,iComponent) = (1-g_f).*grad(neighbours_f,3,iComponent) + g_f.*grad(owners_f,3,iComponent);
    end           
elseif strcmp(theInterpolationScheme,'Gauss linear corrected')
    for iComponent=1:theNumberOfComponents        
        % Linear interpolation
        grad_f(:,1,iComponent) = (1-g_f).*grad(neighbours_f,1,iComponent) + g_f.*grad(owners_f,1,iComponent);
        grad_f(:,2,iComponent) = (1-g_f).*grad(neighbours_f,2,iComponent) + g_f.*grad(owners_f,2,iComponent);
        grad_f(:,3,iComponent) = (1-g_f).*grad(neighbours_f,3,iComponent) + g_f.*grad(owners_f,3,iComponent);                        
        
        % ScfdUrface-normal gradient
        dcfdMag = cfdMag(CF);        
        e_CF = cfdUnit(CF);
        
        local_grad_cfdMag_f = (phi(neighbours_f,iComponent)-phi(owners_f,iComponent))./dcfdMag;               
        local_grad(:,1) = local_grad_cfdMag_f.*e_CF(:,1);
        local_grad(:,2) = local_grad_cfdMag_f.*e_CF(:,2);
        local_grad(:,3) = local_grad_cfdMag_f.*e_CF(:,3);
        
        local_avg_grad_cfdMag = dot(grad_f(:,:,iComponent)',e_CF')';
        local_avg_grad(:,1) = local_avg_grad_cfdMag.*e_CF(:,1);
        local_avg_grad(:,2) = local_avg_grad_cfdMag.*e_CF(:,2);
        local_avg_grad(:,3) = local_avg_grad_cfdMag.*e_CF(:,3);
        
        % Corrected gradient
        grad_f(:,:,iComponent) = grad_f(:,:,iComponent) - local_avg_grad + local_grad;
    end
elseif strcmp(theInterpolationScheme,'Gauss upwind')
    pos = zeros(size(mdot_f));
    pos((mdot_f>0)) = 1;
    %
    grad_f(:,1) = pos.*grad(neighbours_f,1) + (1-pos).*grad(owners_f,1);
    grad_f(:,2) = pos.*grad(neighbours_f,2) + (1-pos).*grad(owners_f,2);
    grad_f(:,3) = pos.*grad(neighbours_f,3) + (1-pos).*grad(owners_f,3);     
else
    error([theInterpolationScheme, ' interpolation scheme incorrect\n']);
end

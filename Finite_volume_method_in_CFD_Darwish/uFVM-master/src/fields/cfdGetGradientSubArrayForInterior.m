function phiGrad = cfdGetGradientSubArrayForInterior(theFieldName, iComponent)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function returns the gradient subarray at interior elements
%--------------------------------------------------------------------------

global Region;

if strcmp(Region.fluid.(theFieldName).type, 'surfaceScalarField')
    phiGrad = Region.fluid.(theFieldName).phiGradient(1:Region.mesh.numberOfInteriorFaces, :);
elseif strcmp(Region.fluid.(theFieldName).type, 'volScalarField')
    phiGrad = Region.fluid.(theFieldName).phiGradient(1:Region.mesh.numberOfElements, :);
elseif strcmp(Region.fluid.(theFieldName).type, 'volVectorField')
    if nargin==1
        phiGrad = Region.fluid.(theFieldName).phiGradient(1:Region.mesh.numberOfElements, :, :);
    else
        phiGrad = Region.fluid.(theFieldName).phiGradient(1:Region.mesh.numberOfElements, :, iComponent);
    end    
end

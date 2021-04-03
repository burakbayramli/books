function cfdUpdateGradientGaussLinear0(theFieldName)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018 
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function computes the gradient for a field at the centroids of 
%   the 'Elements' using a first order gauss interpolation no correction for 
%   non-conjuntionality is applied 'TheFieldName' is the name of a field 
%   defined in the database
%--------------------------------------------------------------------------

theMeshField = cfdGetMeshField(theFieldName);

phiGrad = cfdComputeGradientGaussLinear0(theMeshField.phi);

theMeshField.phiGradient = phiGrad;

cfdSetMeshField(theMeshField);

function cfdCorrectVelocityForInterior(iComponent)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
%    Correct velocity field by adding correction to latest velocity values
%--------------------------------------------------------------------------

dphi = cfdGetDPhi;
theScalarMeshField = cfdGetMeshField('U');

theNumberOfElements = cfdGetNumberOfElements;
theScalarMeshField.phi(1:theNumberOfElements,iComponent) = theScalarMeshField.phi(1:theNumberOfElements,iComponent) + dphi;
cfdSetMeshField(theScalarMeshField);

dphi = zeros(theNumberOfElements,1);
cfdSetDPhi(dphi);
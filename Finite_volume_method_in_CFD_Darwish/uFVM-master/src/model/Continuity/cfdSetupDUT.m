function cfdSetupDUT
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2017
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function creates the DUT field required to assemble
%   pressure correction equation, specifically, in treatment of transient
%   term in the Rhie-Chow interpolation.
%--------------------------------------------------------------------------
cfdSetupMeshField('DUT1');
cfdSetupMeshField('DUT2');
cfdSetupMeshField('DUT3');
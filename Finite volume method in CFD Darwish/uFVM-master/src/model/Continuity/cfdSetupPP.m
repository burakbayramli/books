function cfdSetupPP
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2017
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function creates the DU field required to assemble
%   pressure correction equation at a later stage. It also creates the PP
%   field which is the pressure correction field
%--------------------------------------------------------------------------

% Setup pressure correction field
cfdSetupMeshField('pp'); 


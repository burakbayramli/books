function theConstant = cfdSetupConstant(theName, theValue)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function set up the constant
%--------------------------------------------------------------------------

global Region

if(isfield(Region,'constants')==0)
   Region.constants = {}; 
end
theConstant.name = theName;
theConstant.userName = theName;
theConstant.value = theValue;

ll=length(Region.constants);
Region.constants{ll+1}=theConstant;
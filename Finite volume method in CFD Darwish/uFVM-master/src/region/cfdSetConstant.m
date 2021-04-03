function cfdSetConstant(theConstant)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2017 
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function stores the constant in the data base
%--------------------------------------------------------------------------

global Region;

theIndex = 0;

theNumberOfConstants = length(Region.constants);
for iConstant=1:theNumberOfConstants
    if strcmp(theConstant.name, Region.constants{iConstant}.name)
        theIndex = iConstant;
    end
end

if theIndex==0
    theNumberOfConstants = length(Region.constants);
    Region.constants{theNumberOfConstants+1} = theConstant;
else
    disp('Overriding constant definition');
    Region.constants{theIndex} = theConstant;
end

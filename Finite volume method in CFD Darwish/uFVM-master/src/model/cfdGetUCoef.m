function TM = cfdGetUCoef(iBPatch)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function calculates U coefficient
%--------------------------------------------------------------------------

% Retrieve turbulence state from data base
foamDict = cfdGetFoamDict;
turbulence = foamDict.turbulenceProperties.turbulence;

% Check if laminar or turbulent
if strcmp(turbulence,'off')
    mu_b = cfdGetSubArrayForBoundaryPatch('mu', iBPatch);  
    wallDist_b = cfdGetWallDistSubArrayForBoundaryPatch(iBPatch);
    %
    TM = mu_b./wallDist_b;    
else
    % Get turbulence model
    RASModel = foamDict.turbulenceProperties.RASModel;
    if strcmp(RASModel,'laminar')        
        mu_b = cfdGetSubArrayForBoundaryPatch('mu', iBPatch);  
        wallDist_b = cfdGetWallDistSubArrayForBoundaryPatch(iBPatch);
        %
        TM = mu_b./wallDist_b;  
    else
        error('ERROR turbulence model not defined');
    end
end

end

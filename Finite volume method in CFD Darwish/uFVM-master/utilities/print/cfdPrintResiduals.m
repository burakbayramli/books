function cfdPrintResiduals(theEquationName)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2017
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function prints residuals to console
%--------------------------------------------------------------------------

% Get residuals
theEquation = cfdGetModel(theEquationName);
residuals = theEquation.residuals;

rmsResidual = residuals.rmsResidual;
maxResidual = residuals.maxResidual; 
initResidual = residuals.initResidual;  
finalResidual = residuals.finalResidual; 
    
if strcmp(cfdGetFieldType(theEquationName), 'volVectorField')       
    if length(theEquationName)>8
        theEquationName = theEquationName(1:8);
        RS = '';
        LS = '';
    elseif length(theEquationName)<8
        nSpaces = 8 - length(theEquationName);
        for nSpace=1:nSpaces
            spaces(nSpace)= ' ';
        end
        RS = spaces(1:floor(nSpaces/2));
        LS = spaces((floor(nSpaces/2)+1):end);
    else
        RS = '';
        LS = '';
    end
    
    comp = {'x','y','z'};
    
    for iComponent=1:3
        fprintf(['| ',RS,theEquationName,'-',comp{iComponent},LS, ...
            ' |  %.3E  |  %.3E  |    %.3E    |   %.3E   |\n'], ...
            rmsResidual(iComponent), maxResidual(iComponent), initResidual(iComponent), finalResidual(iComponent));
    end
else
    
    if length(theEquationName)>10
        theEquationName = theEquationName(1:10);
        RS = '';
        LS = '';
    elseif length(theEquationName)<10
        nSpaces = 10 - length(theEquationName);
        for nSpace=1:nSpaces
            spaces(nSpace)= ' ';
        end
        RS = spaces(1:floor(nSpaces/2));
        LS = spaces((floor(nSpaces/2)+1):end);
    else
        RS = '';
        LS = '';
    end
    
    fprintf(['| ',RS,theEquationName,LS,' |  %.3E  |  %.3E  |    %.3E    |   %.3E   |\n'],rmsResidual,maxResidual,initResidual,finalResidual);
end
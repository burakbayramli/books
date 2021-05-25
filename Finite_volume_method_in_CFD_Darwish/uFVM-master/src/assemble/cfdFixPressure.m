function cfdFixPressure
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%     Fix pressure
%--------------------------------------------------------------------------

if cfdNeedPressureLevel    
    pRefCell = cfdGetRefCell;
    
    if pRefCell>-1
        % Get coefficients
        theCoefficients = cfdGetCoefficients;
        
        theElementNbIndices = cfdGetElementNbIndices;
        for iNBElement=1:length(theElementNbIndices{pRefCell})
            theCoefficients.anb{pRefCell}(theElementNbIndices{pRefCell}(iNBElement)) = 0;            
        end        
        theCoefficients.bc(pRefCell) = 0;
        
        % Store
        cfdSetCoefficients(theCoefficients);
    end
    
end
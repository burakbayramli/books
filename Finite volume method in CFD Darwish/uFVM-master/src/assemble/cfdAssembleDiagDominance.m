function cfdAssembleDiagDominance
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   Enforce Diagonal Dominance as this may not be ensured
%--------------------------------------------------------------------------

% Get info and fields
theCoefficients = cfdGetCoefficients;
ac = theCoefficients.ac;
anb = theCoefficients.anb;
cconn = theCoefficients.cconn;

theNumberOfElements = cfdGetNumberOfElements;

for iElement=1:theNumberOfElements
    theNumberOfNeighbours = length(cconn{iElement});
    
    SumAik = 0;
    
    % adding all the off diagonal pressure terms
    for k=1:theNumberOfNeighbours
        if anb{iElement}(k)>0
            anb{iElement}(k) = 0;
        end
        SumAik = SumAik - anb{iElement}(k);        
    end
    
    ac(iElement) = max(ac(iElement),SumAik);          
end

% Store
theCoefficients.ac = ac;
theCoefficients.anb = anb;

cfdSetCoefficients(theCoefficients);



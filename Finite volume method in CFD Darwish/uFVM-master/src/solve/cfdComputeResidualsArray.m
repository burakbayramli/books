function residual = cfdComputeResidualsArray(theCoefficients)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   Compute residuals array: res = b_C - sum(a_F*phi_F)
%--------------------------------------------------------------------------

ac = theCoefficients.ac;
anb = theCoefficients.anb;
bc = theCoefficients.bc;
cconn = theCoefficients.cconn;
phi = theCoefficients.dphi;

numberOfElements = length(ac);
residual = zeros(size(ac));
for iElement=1:numberOfElements    
    residual(iElement) = bc(iElement) - ac(iElement)*phi(iElement);    
    for nNeighbour=1:length(cconn{iElement})
        iNeighbour = cconn{iElement}(nNeighbour);
        residual(iElement) = residual(iElement) - anb{iElement}(nNeighbour)*phi(iNeighbour);
    end    
end
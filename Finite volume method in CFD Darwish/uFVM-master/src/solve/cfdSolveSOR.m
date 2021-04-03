function dphi = cfdSolveSOR(ac,anb,bc,cconn,dphi)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This functions solves linear system Ax = b using Guass-Seidel method   
%--------------------------------------------------------------------------

numberOfElements = length(ac);

for iElement=1:numberOfElements
    local_dphi = bc(iElement);
    for iLocalNeighbour = 1:length(cconn{iElement})
        iNeighbour = cconn{iElement}(iLocalNeighbour);
        local_dphi = local_dphi - anb{iElement}(iLocalNeighbour)*dphi(iNeighbour);
    end
    dphi(iElement) = local_dphi/ac(iElement);
end

for iElement=numberOfElements:-1:1
    local_dphi = bc(iElement);
    for iLocalNeighbour = 1:length(cconn{iElement})
        iNeighbour = cconn{iElement}(iLocalNeighbour);
        local_dphi = local_dphi - anb{iElement}(iLocalNeighbour)*dphi(iNeighbour);
    end
    dphi(iElement) = local_dphi/ac(iElement);
end


function iLevel = cfdAgglomerate(maxCoarseLevels)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   Build algebraic multigrid hierarchy
%--------------------------------------------------------------------------

minNumberOfParents = 5;

iLevel = 1;
while iLevel<=maxCoarseLevels 
    iLevel = iLevel + 1;
    theNumberOfParents = cfdAgglomerateLevel(iLevel);
    cfdAssembleAgglomeratedLHS(iLevel);
    if theNumberOfParents<=minNumberOfParents
        break;
    end
end
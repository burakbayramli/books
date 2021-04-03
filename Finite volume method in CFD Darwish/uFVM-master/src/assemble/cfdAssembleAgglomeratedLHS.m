function cfdAssembleAgglomeratedLHS(iLevel)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   Calculate coarse level's LHS coefficients (ac, anb, etc)
%--------------------------------------------------------------------------

% Get info
theCoefficients = cfdGetCoefficients(iLevel-1);
parents = theCoefficients.parents;
ac = theCoefficients.ac;
anb = theCoefficients.anb;
cconn = theCoefficients.cconn;
csize = theCoefficients.csize;

theParentCoefficients = cfdGetCoefficients(iLevel);
AC = theParentCoefficients.ac;
ANB = theParentCoefficients.anb;
CCONN = theParentCoefficients.cconn;

theNumberOfFineElements = theCoefficients.numberOfElements;
for iElement=1:theNumberOfFineElements
    iParent = parents(iElement);
    AC(iParent) = AC(iParent) + ac(iElement);
    theNumberOfNeighbours = csize(iElement);
    for iNB_local=1:theNumberOfNeighbours
        iNB = cconn{iElement}(iNB_local);
        iNBParent = parents(iNB);
        if iNBParent==iParent
            AC(iParent) = AC(iParent) + anb{iElement}(iNB_local);
        else
            iNBParent_local = find(CCONN{iParent}==iNBParent,1);
            ANB{iParent}(iNBParent_local) = ANB{iParent}(iNBParent_local) + anb{iElement}(iNB_local);
        end
    end
end

% Store
theParentCoefficients.ac = AC;
theParentCoefficients.anb = ANB;
theParentCoefficients.cconn = CCONN;

cfdSetCoefficients(theParentCoefficients,iLevel);

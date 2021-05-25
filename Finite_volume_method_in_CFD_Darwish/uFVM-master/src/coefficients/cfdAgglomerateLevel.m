function theNumberOfParents = cfdAgglomerateLevel(iLevel)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   Agglomerate level to construct coarser algebraic level
%--------------------------------------------------------------------------

% Get info
theCoefficients = cfdGetCoefficients(iLevel-1);
theNumberOfFineElements = length(theCoefficients.ac);

anb = theCoefficients.anb;
cconn = theCoefficients.cconn;
csize = theCoefficients.csize;

parents = zeros(theNumberOfFineElements,1);
maxAnb = zeros(theNumberOfFineElements,1);

for iElement=1:theNumberOfFineElements
    maxAnb(iElement) = max(-anb{iElement});
end

iParent = 1;

% Step 1 Agglomeration
for iSeed=1:theNumberOfFineElements
    if(parents(iSeed)==0)
        parents(iSeed) = iParent;
        children = [iSeed];
        for iNB_local = 1:csize(iSeed)
            iNB = cconn{iSeed}(iNB_local);
            if(parents(iNB) == 0)
                if((-anb{iSeed}(iNB_local)/maxAnb(iSeed))>0.5)
                    parents(iNB) = iParent;
                    children = [children iNB];
                end
            end
        end
        theNumberOfChildren = length(children);
        children2 = [];
        for iChild_local=2:theNumberOfChildren
            iChild = children(iChild_local);
            for iChildNB_local=1:csize(iChild)
                iChildNB = cconn{iChild}(iChildNB_local);
                if(parents(iChildNB)==0)
                    if((-anb{iChild}(iChildNB_local)/maxAnb(iChild))>0.5)
                        parents(iChildNB) = iParent;
                        children2 = [children2 iChildNB];
                    end
                end
            end
        end
        theNumberOfChildren = length(children) + length(children2);
        if(theNumberOfChildren==1)
            parents(iSeed) = 0;
        else
            iParent = iParent + 1;
        end
    end
end

% Last Step Agglomeration
for iOrphan=1:theNumberOfFineElements
    if(parents(iOrphan)==0)
        strength = 0;
        for iNB_local=1:csize(iOrphan)
            iNB = cconn{iOrphan}(iNB_local);
            if(parents(iNB)~=0)
                if(strength<-anb{iOrphan}(iNB_local)/maxAnb(iNB))
                    strength = -anb{iOrphan}(iNB_local)/maxAnb(iNB);
                    parents(iOrphan) = parents(iNB);
                end
            end
        end
    end
    if(parents(iOrphan)==0)
        parents(iOrphan) = iParent;
        iParent = iParent + 1;
        disp('the orphan could not find a parent');
    end
end

theNumberOfParents = iParent - 1;
theCoefficients.parents = parents;
cfdSetCoefficients(theCoefficients, iLevel-1);

% setup connectivity and csize
theParentCConn = cell(theNumberOfParents,1);
for iElement=1:theNumberOfFineElements
    for iNB_local=1:csize(iElement)
        iNB = cconn{iElement}(iNB_local);
        if parents(iElement)~=parents(iNB)
            if isempty(find(theParentCConn{parents(iElement)}==parents(iNB),1))
                theParentCConn{parents(iElement)} = [theParentCConn{parents(iElement)} parents(iNB)];
            end
        end
    end
end

for iCoarseElement=1:length(theParentCConn)
    theParentCSize(iCoarseElement,1) = length(theParentCConn{iCoarseElement});
end

% Setup coefficients for coarser level
theParentCoefficients = cfdSetupCoefficients(theParentCConn,theParentCSize);
cfdSetCoefficients(theParentCoefficients,iLevel);


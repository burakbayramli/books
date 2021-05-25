function cfdCorrectScalarEquation(theEquationName)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
%    Correct scalar equation
%--------------------------------------------------------------------------


% Get info
theNmberOfElements = cfdGetNumberOfElements;

% Get fields
theScalarMeshField = cfdGetMeshField(theEquationName);

% Get correction
dphi = cfdGetDPhi;

% Correct Interior Field
theScalarMeshField.phi(1:theNmberOfElements) = theScalarMeshField.phi(1:theNmberOfElements) + dphi;

% Store
cfdSetMeshField(theScalarMeshField);

% Reset correction
dphi = zeros(theNmberOfElements,1);
cfdSetDPhi(dphi);


% Correct Boundary patches
theNumberOfBPatches = cfdGetNumberOfBPatches;
for iBPatch=1:theNumberOfBPatches
    
    % Get Physical and Equation Boundary Conditions
    theBCInfo = cfdGetBoundaryPatchRef(iBPatch);
    thePhysicalType = theBCInfo.type;
    theBCType = cfdBcForBoundaryPatch(theEquationName, iBPatch);
    %
    % WALL
    %
    if strcmp(thePhysicalType,'wall')
        if strcmp(theBCType,'fixedValue')
            correctFixedValueBC(iBPatch,theEquationName,iComponent);
        elseif strcmp(theBCType,'zeroGradient')
            correctZeroGradientBC(iBPatch,theEquationName,iComponent);
        elseif strcmp(theBCType,'fixedGradient')
            correctFixedGradientBC(iBPatch,theEquationName,iComponent);            
        elseif strcmp(theBCType,'noSlip')
            correctZeroGradientBC(iBPatch,theEquationName,iComponent);
        elseif strcmp(theBCType,'slip')
            correctZeroGradientBC(iBPatch,theEquationName,iComponent);
        else
            error('Wall Condition not implemented')
        end
        %
        % INLET
        %
    elseif strcmp(thePhysicalType,'inlet')
        if strcmp(theBCType,'fixedValue')
            correctFixedValueBC(iBPatch,theEquationName);
        else
            error('Inlet Condition not implemented')
        end
        %
        % OUTLET
        %
    elseif strcmp(thePhysicalType,'outlet')
        if strcmp(theBCType,'fixedValue')
            correctOutletFixedValueBC(iBPatch,theEquationName,iComponent);
        elseif strcmp(theBCType,'outlet')
            correctZeroGradientBC(iBPatch,theEquationName);
        elseif strcmp(theBCType,'zeroGradient')
            correctZeroGradientBC(iBPatch,theEquationName);
        else
            error([theBCType '<<<< Outlet Condition not implemented'])
        end
        %
        % SYMMETRY
        %
    elseif strcmp(thePhysicalType,'symmetry')
        correctZeroGradientBC(iBPatch,theEquationName);
        %
        % EMPTY
        %
    elseif strcmp(thePhysicalType,'empty')
        correctZeroGradientBC(iBPatch,theEquationName);
        %
        % ERROR
        %
    else
        error('%s Condition not implemented', thePhysicalType)
    end
    
end

end



%===================================================
% WALL - Fixed Value
%===================================================
function correctFixedValueBC(iBPatch,theEquationName)

end

%===================================================
% WALL - Zero Gradient
%===================================================
function correctZeroGradientBC(iBPatch,theEquationName)

% Get info
iBElements = cfdGetBoundaryElementsSubArrayForBoundaryPatch(iBPatch);
owners_b = cfdGetOwnersSubArrayForBoundaryPatch(iBPatch);

% Get field and update from owner cells' values
theScalarField = cfdGetMeshField(theEquationName);
theScalarField.phi(iBElements) = theScalarField.phi(owners_b);

% Store
cfdSetMeshField(theScalarField);

end


%===================================================
% WALL - Fixed Gradient
%===================================================
function correctFixedGradientBC(iBPatch,theEquationName)

% Get info
iBElements = cfdGetBoundaryElementsSubArrayForBoundaryPatch(iBPatch);
owners_b = cfdGetOwnersSubArrayForBoundaryPatch(iBPatch);

CF_b = cfdGetFaceCFForBoundaryPatch(iBPatch);

% Get fields
Gamma_b = cfdGetSubArrayForBoundaryPatch('Gamma', iBPatch);

% Fet specified flux
q_b = cfdValueForBoundaryPatch(theEquationName, iBPatch);

% Get field and update
theScalarField = cfdGetMeshField(theEquationName);
theScalarField.phi(iBElements) = theScalarField.phi(owners_b) - cfdMag(CF_b)./Gamma_b.*q_b;

% Store
cfdSetMeshField(theScalarField);

end

function cfdCorrectEnergyEquation
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
%    Correct energy equation
%--------------------------------------------------------------------------


% Get info
theNmberOfElements = cfdGetNumberOfElements;

% Get fields
theTemperatureField = cfdGetMeshField('T');

% Get correction
dphi = cfdGetDPhi;

% Correct Interior Field
theTemperatureField.phi(1:theNmberOfElements) = theTemperatureField.phi(1:theNmberOfElements) + dphi;

% Store
cfdSetMeshField(theTemperatureField);

% Reset correction
dphi = zeros(theNmberOfElements,1);
cfdSetDPhi(dphi);


% Correct Boundary patches
theNumberOfBPatches = cfdGetNumberOfBPatches;
for iBPatch=1:theNumberOfBPatches
    
    % Get Physical and Equation Boundary Conditions
    theBCInfo = cfdGetBoundaryPatchRef(iBPatch);
    thePhysicalType = theBCInfo.type;
    theBCType = cfdBcForBoundaryPatch('T', iBPatch);
    %
    % WALL
    %
    if strcmp(thePhysicalType,'wall')
        if strcmp(theBCType,'fixedValue')
            correctFixedValueBC(iBPatch);
        elseif strcmp(theBCType,'zeroGradient')
            correctZeroGradientBC(iBPatch);
        elseif strcmp(theBCType,'fixedGradient')
            correctFixedGradientBC(iBPatch);            
        elseif strcmp(theBCType,'noSlip')
            correctZeroGradientBC(iBPatch);
        elseif strcmp(theBCType,'slip')
            correctZeroGradientBC(iBPatch);
        else
            error('Wall Condition not implemented')
        end
        %
        % INLET
        %
    elseif strcmp(thePhysicalType,'inlet')
        if strcmp(theBCType,'fixedValue')
            correctFixedValueBC(iBPatch);
        elseif strcmp(theBCType,'zeroGradient')
            correctZeroGradientBC(iBPatch);            
        else
            error('Inlet Condition not implemented')
        end
        %
        % OUTLET
        %
    elseif strcmp(thePhysicalType,'outlet')
        if strcmp(theBCType,'fixedValue')
           correctFixedValueBC(iBPatch);
        elseif strcmp(theBCType,'outlet')
            correctZeroGradientBC(iBPatch);
        elseif strcmp(theBCType,'zeroGradient')
            correctZeroGradientBC(iBPatch);
        else
            error([theBCType '<<<< Outlet Condition not implemented'])
        end
        %
        % SYMMETRY
        %
    elseif strcmp(thePhysicalType,'symmetry')
        correctZeroGradientBC(iBPatch);
        %
        % EMPTY
        %
    elseif strcmp(thePhysicalType,'empty')
        correctZeroGradientBC(iBPatch);
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
function correctFixedValueBC(iBPatch)

end

%===================================================
% WALL - Zero Gradient
%===================================================
function correctZeroGradientBC(iBPatch)

% Get info
iBElements = cfdGetBoundaryElementsSubArrayForBoundaryPatch(iBPatch);
owners_b = cfdGetOwnersSubArrayForBoundaryPatch(iBPatch);

% Get field and update from owner cells' values
theTemperatureField = cfdGetMeshField('T');
theTemperatureField.phi(iBElements) = theTemperatureField.phi(owners_b);

% Store
cfdSetMeshField(theTemperatureField);

end


%===================================================
% WALL - Fixed Gradient
%===================================================
function correctFixedGradientBC(iBPatch)

% Get info
iBElements = cfdGetBoundaryElementsSubArrayForBoundaryPatch(iBPatch);
owners_b = cfdGetOwnersSubArrayForBoundaryPatch(iBPatch);

CF_b = cfdGetFaceCFForBoundaryPatch(iBPatch);

% Get fields
k_b = cfdGetSubArrayForBoundaryPatch('k', iBPatch);

% Fet specified flux
q_b = cfdValueForBoundaryPatch('T', iBPatch);

% Get field and update
theTemperatureField = cfdGetMeshField('T');
theTemperatureField.phi(iBElements) = theTemperatureField.phi(owners_b) - cfdMag(CF_b)./k_b.*q_b;

% Store
cfdSetMeshField(theTemperatureField);

end

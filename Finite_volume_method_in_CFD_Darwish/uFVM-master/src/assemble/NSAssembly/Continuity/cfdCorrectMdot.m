function cfdCorrectMdot
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%
%--------------------------------------------------------------------------

cfdCorrectMdotForInterior;
cfdCorrectMdotForBoundaryPatches;

end

function cfdCorrectMdotForInterior

% Get info
theNumberOfInteriorFaces = cfdGetNumberOfInteriorFaces;
iFaces = 1:theNumberOfInteriorFaces;
owners_f = cfdGetOwnersSubArrayForInteriorFaces;
neighbours_f = cfdGetNeighboursSubArrayForInteriorFaces;

% Get fluxes
theFluxes = cfdGetFluxes;
FluxCf = theFluxes.FluxCf;
FluxFf = theFluxes.FluxFf;

% Get fields
theMdotField = cfdGetMeshField('mdot_f');
mdot_f = theMdotField.phi;

pp = cfdGetSubArrayForInterior('pp');

% Correct
mdot_f(iFaces) = mdot_f(iFaces) + 0.75*(FluxCf(iFaces).*pp(owners_f) + FluxFf(iFaces).*pp(neighbours_f));

% Store mdot_f
theMdotField.phi = mdot_f;
cfdSetMeshField(theMdotField);

end

function cfdCorrectMdotForBoundaryPatches

theNumberOfPatches = cfdGetNumberOfBPatches;
for iBPatch=1:theNumberOfPatches
    
    theBCInfo = cfdGetBoundaryPatchRef(iBPatch);
    thePhysicalType = theBCInfo.type;
    theBCType = cfdBcForBoundaryPatch('p', iBPatch);
    
    if strcmp(thePhysicalType,'wall')
        continue;
    elseif strcmp(thePhysicalType,'inlet')
        if strcmp(theBCType,'inlet') || strcmp(theBCType,'zeroGradient')
            cfdCorrectMdotZeroGradient(iBPatch);
        elseif strcmp(theBCType,'fixedValue')
            cfdCorrectMdotFixedValue(iBPatch);            
        else
            error([theBCType ' boundary condition not implemented']);
        end
    elseif strcmp(thePhysicalType,'outlet')
        if strcmp(theBCType,'fixedValue')
            cfdCorrectMdotFixedValue(iBPatch);
        elseif strcmp(theBCType,'outlet') || strcmp(theBCType,'zeroGradient')
            cfdCorrectMdotZeroGradient(iBPatch);            
        else
            error([theBCType ' boundary condition not implemented']);
        end
    elseif strcmp(thePhysicalType,'symmetry')
        continue;
    elseif strcmp(thePhysicalType,'empty')
        continue;
    else
        error([thePhysicalType ' physical condition not implemented']);
    end
    
end


end


%===================================================
% Zero Gradient
%===================================================
function cfdCorrectMdotZeroGradient(iBPatch)

% Get info
iBFaces = cfdGetBFaceIndicesForBoundaryPatch(iBPatch);
owners_b = cfdGetOwnersSubArrayForBoundaryPatch(iBPatch);

Sb = cfdGetFaceSfSubArrayForBoundaryPatch(iBPatch);

% Get fields
theMdotField = cfdGetMeshField('mdot_f');
mdot_f = theMdotField.phi;

rho_b = cfdGetSubArrayForBoundaryPatch('rho', iBPatch);

theVelocityField = cfdGetMeshField('U');
U_b = theVelocityField.phi(owners_b,:);

% Update and store mdot
mdot_f(iBFaces) = 0.75*(rho_b .* dot(U_b',Sb')') + (1-0.75)*mdot_f(iBFaces);
theMdotField.phi = mdot_f;

cfdSetMeshField(theMdotField);


end


%===================================================
% Fixed Value
%===================================================
function cfdCorrectMdotFixedValue(iBPatch)

% Get info
iBFaces = cfdGetBFaceIndicesForBoundaryPatch(iBPatch);
owners_b = cfdGetOwnersSubArrayForBoundaryPatch(iBPatch);

% Get fluxes
theFluxes = cfdGetFluxes;
FluxCf = theFluxes.FluxCf;

% Get fields
theMdotField = cfdGetMeshField('mdot_f');
mdot_f = theMdotField.phi;

pp = cfdGetSubArrayForInterior('pp');

% Correct
mdot_f(iBFaces) = mdot_f(iBFaces) + 0.75*(FluxCf(iBFaces).*pp(owners_b));

% Store mdot_f
theMdotField.phi = mdot_f;
cfdSetMeshField(theMdotField);

end
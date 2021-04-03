function cfdAssembleMassDivergenceTerm
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%     This function assembles pressure correction equation
%--------------------------------------------------------------------------

% Assemble at interior faces
cfdAssembleMassDivergenceTermInterior;

% Assemble at cfdBoundary patch faces
theNumberOfBPatches = cfdGetNumberOfBPatches;
for iBPatch=1:theNumberOfBPatches
    
    % Find the Physical Type
    theBoundary = cfdGetBoundaryPatchRef(iBPatch);
    thePhysicalType = theBoundary.type;
    theBCType = cfdBcForBoundaryPatch('p', iBPatch);

    if strcmp(thePhysicalType,'wall')
        if strcmp(theBCType,'noSlip')
             cfdAssembleMassDivergenceTermWallNoslipBC(iBPatch);
        elseif strcmp(theBCType,'slip')
             cfdAssembleMassDivergenceTermWallSlipBC(iBPatch);
        elseif strcmp(theBCType,'zeroGradient')
             cfdAssembleMassDivergenceTermWallZeroGradientBC(iBPatch);
        else
            error([theBCType '<<<< not implemented']);
        end
    elseif strcmp(thePhysicalType,'inlet')
        if strcmp(theBCType,'inlet') || strcmp(theBCType,'zeroGradient')
             cfdAssembleMassDivergenceTermInletZeroGradientBC(iBPatch);
        elseif strcmp(theBCType,'fixedValue')
             cfdAssembleMassDivergenceTermInletFixedValueBC(iBPatch);
        else
            error([theBCType '<<<< not implemented']);
        end
    elseif strcmp(thePhysicalType,'outlet')
        if strcmp(theBCType,'outlet') || strcmp(theBCType,'zeroGradient')
             cfdAssembleMassDivergenceTermOutletZeroGradientBC(iBPatch);
        elseif strcmp(theBCType,'fixedValue')
             cfdAssembleMassDivergenceTermOutletFixedValueBC(iBPatch);
        else
            error([theBCType '<<<< not implemented']);
        end
    elseif (strcmp(thePhysicalType,'empty')) || (strcmp(thePhysicalType,'symmetry')) || (strcmp(thePhysicalType,'symmetryPlane'))
        continue;
    else
        error([thePhysicalType '<<<< not implemented']);
    end
    %
end

end


%===================================================
% INTERIOR Assembly
%===================================================
function cfdAssembleMassDivergenceTermInterior

% Get mesh info
theNumberOfInteriorFaces = cfdGetNumberOfInteriorFaces;
iFaces = 1:theNumberOfInteriorFaces;

owners_f = cfdGetOwnersSubArrayForInteriorFaces;
neighbours_f = cfdGetNeighboursSubArrayForInteriorFaces;

Sf = cfdGetFaceSfSubArrayForInterior;
CF = cfdGetFaceCFSubArrayForInterior;

% Calculated info
e = cfdUnit(CF);
cfdMagCF = cfdMag(CF);

% Initialize local fluxes
local_FluxCf(iFaces,1) = 0.0;
local_FluxFf(iFaces,1) = 0.0;
local_FluxVf(iFaces,1) = 0.0;

% Get DU field
DU1_f = cfdInterpolateFromElementsToInteriorFaces('linear', cfdGetSubArrayForInterior('DU1'));
DU2_f = cfdInterpolateFromElementsToInteriorFaces('linear', cfdGetSubArrayForInterior('DU2'));
DU3_f = cfdInterpolateFromElementsToInteriorFaces('linear', cfdGetSubArrayForInterior('DU3'));

% Get first computed mdot_f
mdot_f_prev = cfdGetSubArrayForInterior('mdot_f');

% Get velocity field and interpolate to faces
U_bar_f = cfdInterpolateFromElementsToInteriorFaces('linear', cfdGetSubArrayForInterior('U'));

% Get rho field and assign density at faces as the convected one
rho = cfdGetSubArrayForInterior('rho');
rho_f = cfdInterpolateFromElementsToInteriorFaces('linearUpwind', rho, mdot_f_prev);

% Get pressure field and interpolate to faces
p = cfdGetSubArrayForInterior('p');

% Get linear interpolated and corrected pressure gradients
p_grad_bar_f = cfdInterpolateGradientsFromElementsToInteriorFaces('linear', cfdGetGradientSubArrayForInterior('p'), p);
p_grad_f =  cfdInterpolateGradientsFromElementsToInteriorFaces('Gauss linear corrected', cfdGetGradientSubArrayForInterior('p'), p);

%
% Assemble Coefficients
%
%
% assemble term I
%     rho_f [v]_f.Sf
%
U_bar_f = dot(U_bar_f',Sf')';
local_FluxVf = local_FluxVf + rho_f.*U_bar_f;
%
% Assemble term II and linearize it
%      - rho_f ([DPVOL]_f.P_grad_f).Sf
%
DUSf = [DU1_f.*Sf(:,1),DU2_f.*Sf(:,2),DU3_f.*Sf(:,3)]; magDUSf = cfdMag(DUSf);
eDUSf = cfdUnit(DUSf);

DUEf = [magDUSf.*e(:,1)./dot(e',eDUSf')',magDUSf.*e(:,2)./dot(e',eDUSf')',magDUSf.*e(:,3)./dot(e',eDUSf')'];
geoDiff = cfdMag(DUEf)./cfdMagCF;

DUTf = DUSf - DUEf;

local_FluxCf = local_FluxCf + rho_f.*geoDiff;
local_FluxFf = local_FluxFf - rho_f.*geoDiff;
local_FluxVf = local_FluxVf - rho_f.*dot(p_grad_f(iFaces,:)',DUTf')';

%
%  assemble term III
%    rho_f ([P_grad]_f.([DPVOL]_f.Sf))
%
local_FluxVf = local_FluxVf + rho_f.*dot(p_grad_bar_f(iFaces,:)',DUSf')';
%
% assemble terms IV
%     (1-URF)(U_f -[v]_f.S_f)
%
% urf_U = cfdGetEquationRelaxationFactor('U');
% local_FluxVf = local_FluxVf + (1 - urf_U)*(mdot_f_prev - rho_f.*U_bar_f);

% Assemble total flux
% local_FluxTf = local_FluxCf.*p(owners_f) + local_FluxFf.*p(neighbours_f) + local_FluxVf;

% Update global fluxes
theFluxes = cfdGetFluxes;

theFluxes.FluxCf(iFaces) = local_FluxCf;
theFluxes.FluxFf(iFaces) = local_FluxFf;
theFluxes.FluxVf(iFaces) = local_FluxVf;
% theFluxes.FluxTf(iFaces,1) = local_FluxTf;

cfdSetFluxes(theFluxes);

end

%===================================================
% WALL- slip Conditions
%===================================================
function  cfdAssembleMassDivergenceTermWallSlipBC(iBPatch)

end

%===================================================
% WALL-noslip Condition
%===================================================
function  cfdAssembleMassDivergenceTermWallNoslipBC(iBPatch)

end

%===================================================
% WALL-zeroGradient Condition
%===================================================
function  cfdAssembleMassDivergenceTermWallZeroGradientBC(iBPatch)

end

%===================================================
% INLET - Zero Gradient
%===================================================
function  cfdAssembleMassDivergenceTermInletZeroGradientBC(iBPatch)

% Get info
owners_b = cfdGetOwnersSubArrayForBoundaryPatch(iBPatch);
Sf_b = cfdGetFaceSfSubArrayForBoundaryPatch(iBPatch);
iBFaces = cfdGetBFaceIndicesForBoundaryPatch(iBPatch);

theNumberOfBFaces = cfdGetNumberOfFacesForBoundaryPatch(iBPatch);

% Initialize local fluxes
local_FluxCb(1:theNumberOfBFaces,1) = 0.0;
local_FluxFb(1:theNumberOfBFaces,1) = 0.0;

% Get Fields
U_b = cfdGetSubArrayForBoundaryPatch('U', iBPatch);
rho_b = cfdGetSubArrayForBoundaryPatch('rho', iBPatch);
p_b = cfdGetSubArrayForBoundaryPatch('p', iBPatch);
p = cfdGetSubArrayForInterior('p');

% ---------
%  STEP 1
% ---------
%  Assemble FLUXCb, FLUXFb and FLUXVb coefficients
%
%---------------------------------------------------
% assemble RHIE-CHOW Interpolation Term I
%---------------------------------------------------
%

%
% ONLY assemble term I
%
U_b = dot(Sf_b',U_b')';
local_FluxVb = rho_b.*U_b;  % term I

% Total flux
% local_FluxTb = local_FluxCb.*p(owners_b) + local_FluxFb.*p_b + local_FluxVb;

% Update global fluxes
theFluxes = cfdGetFluxes;

theFluxes.FluxCf(iBFaces) =  local_FluxCb;
theFluxes.FluxFf(iBFaces) =  local_FluxFb;
theFluxes.FluxVf(iBFaces) =  local_FluxVb;
%theFluxes.FluxTf(iBFaces) =  local_FluxTb;

cfdSetFluxes(theFluxes);

end


%===================================================
% OUTLET - Zero Gradient
%===================================================
function  cfdAssembleMassDivergenceTermOutletZeroGradientBC(iBPatch)

% Get info
owners_b = cfdGetOwnersSubArrayForBoundaryPatch(iBPatch);
Sf_b = cfdGetFaceSfSubArrayForBoundaryPatch(iBPatch);
iBFaces = cfdGetBFaceIndicesForBoundaryPatch(iBPatch);

theNumberOfBFaces = cfdGetNumberOfFacesForBoundaryPatch(iBPatch);

% Initialize local fluxes
local_FluxCb(1:theNumberOfBFaces,1) = 0.0;
local_FluxFb(1:theNumberOfBFaces,1) = 0.0;
local_FluxVb(1:theNumberOfBFaces,1) = 0.0;

% Get Fields
U_b = cfdGetSubArrayForBoundaryPatch('U', iBPatch);
rho_b = cfdGetSubArrayForBoundaryPatch('rho', iBPatch);
p_b = cfdGetSubArrayForBoundaryPatch('p', iBPatch);
p = cfdGetSubArrayForInterior('p');

% ---------
%  STEP 1
% ---------
%  Assemble FLUXCb, FLUXFb and FLUXVb coefficients
%
%---------------------------------------------------
% assemble RHIE-CHOW Interpolation Term I
%---------------------------------------------------
%

%
% ONLY assemble term I
%
U_b = dot(Sf_b(:,:)',U_b(:,:)')';
local_FluxVb = local_FluxVb + rho_b.*U_b;  % term I

% Total flux
% local_FluxTb = local_FluxCb.*p(owners_b) + local_FluxFb.*p_b + local_FluxVb;

% Update global fluxes
theFluxes = cfdGetFluxes;

theFluxes.FluxCf(iBFaces) =  local_FluxCb;
theFluxes.FluxFf(iBFaces) =  local_FluxFb;
theFluxes.FluxVf(iBFaces) =  local_FluxVb;
% theFluxes.FluxTf(iBFaces) =  local_FluxTb;

cfdSetFluxes(theFluxes);

end



%===================================================
% OUTLET - Fixed Value
%===================================================

function  cfdAssembleMassDivergenceTermOutletFixedValueBC(iBPatch)

% Get info
owners_b = cfdGetOwnersSubArrayForBoundaryPatch(iBPatch);

theNumberOfBFaces = cfdGetNumberOfFacesForBoundaryPatch(iBPatch);

Sf_b = cfdGetFaceSfSubArrayForBoundaryPatch(iBPatch);
CF_b = cfdGetFaceCFForBoundaryPatch(iBPatch);
e = cfdUnit(CF_b);

iFaceStart = cfdGetStartingFaceIndexForBoundaryPatch(iBPatch);
iFaceEnd = iFaceStart+theNumberOfBFaces-1;
iBFaces = iFaceStart:iFaceEnd;

% Initialize local fluxes
local_FluxVb(1:theNumberOfBFaces,1) = 0.0;

% Get Fields
U_b = cfdGetSubArrayForBoundaryPatch('U', iBPatch);
rho_b = cfdGetSubArrayForBoundaryPatch('rho', iBPatch);
p_b = cfdGetSubArrayForBoundaryPatch('p', iBPatch);
p = cfdGetSubArrayForInterior('p');
p_grad_b = cfdGetGradientSubArrayForBoundaryPatch('p', iBPatch);

%
% ---------
%  STEP 1
% ---------
%  Assemble FLUXCb, FLUXFb and FLUXVb coefficients
%
%---------------------------------------------------------------
% assemble RHIE-CHOW Interpolation Term I, II, III, VIII and IX
%---------------------------------------------------------------
%

%
% Assemble Coefficients
%

% The DU field for cfdBoundary
DU1_b = cfdGetSubArrayForBoundaryPatch('DU1', iBPatch);
DU2_b = cfdGetSubArrayForBoundaryPatch('DU2', iBPatch);
DU3_b = cfdGetSubArrayForBoundaryPatch('DU3', iBPatch);

DUSb = [DU1_b.*Sf_b(:,1),DU2_b.*Sf_b(:,2),DU3_b.*Sf_b(:,3)]; 
magSUDb = cfdMag(DUSb);
eDUSb = cfdUnit(DUSb);

DUEb = [magSUDb./dot(e',eDUSb')'.*e(:,1),magSUDb./dot(e',eDUSb')'.*e(:,2),magSUDb./dot(e',eDUSb')'.*e(:,3)];
geoDiff = cfdMag(DUEb)./cfdMag(CF_b);

%
% Assemble term I
%
U_bar_b = dot(U_b',Sf_b')';
local_FluxVb = local_FluxVb + rho_b.*U_bar_b;  % term I
%
% Assemble term II and linearize it
%
local_FluxCb =  rho_b.*geoDiff;
local_FluxFb = -rho_b.*geoDiff;
%
% Assemble term III
%
local_FluxVb = local_FluxVb + rho_b.*dot(p_grad_b',DUSb')';
% local_FluxTb = local_FluxCb.*p(owners_b) + local_FluxFb.*p_b + local_FluxVb;

% Update global fluxes
theFluxes = cfdGetFluxes;

theFluxes.FluxCf(iBFaces) = local_FluxCb;
theFluxes.FluxFf(iBFaces) = local_FluxFb;
theFluxes.FluxVf(iBFaces) = local_FluxVb;
% theFluxes.FluxTf(iBFaces) =  local_FluxTb;

cfdSetFluxes(theFluxes);

end


%===================================================
% INLET - Fixed Value
%===================================================
function  cfdAssembleMassDivergenceTermInletFixedValueBC(iBPatch)

% Get info
owners_b = cfdGetOwnersSubArrayForBoundaryPatch(iBPatch);
Sf_b = cfdGetFaceSfSubArrayForBoundaryPatch(iBPatch);
iBFaces = cfdGetBFaceIndicesForBoundaryPatch(iBPatch);

theNumberOfBFaces = cfdGetNumberOfFacesForBoundaryPatch(iBPatch);

% Initialize local fluxes
local_FluxCb(1:theNumberOfBFaces,1) = 0.0;
local_FluxFb(1:theNumberOfBFaces,1) = 0.0;
local_FluxVb(1:theNumberOfBFaces,1) = 0.0;

% Get Fields
U_b = cfdGetSubArrayForBoundaryPatch('U', iBPatch);
rho_b = cfdGetSubArrayForBoundaryPatch('rho', iBPatch);
p_b = cfdGetSubArrayForBoundaryPatch('p', iBPatch);
p = cfdGetSubArrayForInterior('p');
p_grad_b = cfdGetGradientSubArrayForBoundaryPatch('p', iBPatch);

%
% ---------
%  STEP 1
% ---------
%  Assemble FLUXCb, FLUXFb and FLUXVb coefficients
%
%---------------------------------------------------------------
% assemble RHIE-CHOW Interpolation Term I, II, III, VIII and IX
%---------------------------------------------------------------
%

%
% Assemble Coefficients
%

% The DU field for cfdBoundary
DU1_b = cfdGetSubArrayForBoundaryPatch('DU1', iBPatch);
DU2_b = cfdGetSubArrayForBoundaryPatch('DU2', iBPatch);
DU3_b = cfdGetSubArrayForBoundaryPatch('DU3', iBPatch);

DUSb = [DU1_b.*Sf_b(:,1),DU2_b.*Sf_b(:,2),DU3_b.*Sf_b(:,3)];
eDUSb = [DUSb(:,1)./cfdMag(DUSb),DUSb(:,2)./cfdMag(DUSb),DUSb(:,3)./cfdMag(DUSb)];

DUEb = [cfdMag(DUSb)./dot(eCF',eDUSb')'.*eCF(:,1),cfdMag(DUSb)./dot(eCF',eDUSb')'.*eCF(:,2),cfdMag(DUSb)./dot(eCF',eDUSb')'.*eCF(:,3)];
geoDiff = cfdMag(DUEb)./cfdMag(CF_b);

%
% Assemble term I
%
U_bar_b = dot(U_b',Sf_b')';
local_FluxVb = local_FluxVb + rho_b.*U_bar_b;  % term I
%
% Assemble term II and linearize it
%
local_FluxCb = local_FluxCb + rho_b.*geoDiff;
local_FluxFb = 0.0;
%
% Assemble term III
%
local_FluxVb = local_FluxVb + rho_b.*dot(p_grad_b',DUSb')';
% local_FluxTb = local_FluxCb.*p(owners_b) + local_FluxFb.*p_b + local_FluxVb;

% Update global fluxes
theFluxes = cfdGetFluxes;

theFluxes.FluxCf(iBFaces) =  local_FluxCb;
theFluxes.FluxFf(iBFaces) =  local_FluxFb;
theFluxes.FluxVf(iBFaces) =  local_FluxVb;
% theFluxes.FluxTf(iBFaces,1) =  local_FluxTb;

cfdSetFluxes(theFluxes);

end

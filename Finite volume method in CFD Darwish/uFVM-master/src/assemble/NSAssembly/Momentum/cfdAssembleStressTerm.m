function cfdAssembleStressTerm(iComponent)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018 
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function assembles Stress term
%--------------------------------------------------------------------------

% Assemble Over Interior Faces
cfdAssembleStressTermInterior(iComponent);

% Assemble Over Boundary Patches
theNumberOfBPatches = cfdGetNumberOfBPatches;
for iBPatch=1:theNumberOfBPatches
    
    % Find the Physical Type
    theBoundary = cfdGetBoundaryPatchRef(iBPatch);
    thePhysicalType = theBoundary.type;
    theBCType = cfdBcForBoundaryPatch('U',iBPatch);

    if strcmp(thePhysicalType,'wall')
        if strcmp(theBCType,'slip')
            continue;
        elseif strcmp(theBCType,'noSlip')
            cfdAssembleStressTermWallNoslipBC(iBPatch, iComponent);
        elseif(strcmp(theBCType,'fixedValue'))
            cfdAssembleStressTermWallFixedValueBC(iBPatch, iComponent);
        else
            error([theBCType ' bc is not implemented']);
        end
    elseif strcmp(thePhysicalType,'inlet')
        if strcmp(theBCType,'fixedValue')
            cfdAssembleStressTermInletFixedValueBC(iBPatch, iComponent);
        elseif strcmp(theBCType,'zeroGradient') || strcmp(theBCType,'inlet')
            cfdAssembleStressTermInletZeroGradientBC(iBPatch, iComponent);
            else
            error([theBCType ' bc is not implemented']);
        end
    elseif strcmp(thePhysicalType,'outlet')
        if strcmp(theBCType,'zeroGradient') || strcmp(theBCType,'outlet')
            cfdAssembleStressTermOutletZeroGradientBC(iBPatch, iComponent);
        elseif strcmp(theBCType,'fixedValue')
            cfdAssembleStressTermOutletFixedValueBC(iBPatch, iComponent);            
        else
            error([theBCType ' bc is not implemented']);
        end
    elseif strcmp(thePhysicalType,'symmetry') || strcmp(thePhysicalType,'symmetryPlane')
        cfdAssembleStressTermSymmetry(iBPatch, iComponent);
    elseif strcmp(thePhysicalType,'empty')
        cfdAssembleStressTermEmptyBC(iBPatch, iComponent);
    else
        error([thePhysicalType ' physical condition is not implemented']);
    end    
end

end




%===================================================
% INTERIOR
%===================================================
function cfdAssembleStressTermInterior(iComponent)

% Get info
theNumberOfInteriorFaces = cfdGetNumberOfInteriorFaces;
Sf = cfdGetFaceSfSubArrayForInterior;
CF = cfdGetFaceCFSubArrayForInterior;
owners_f = cfdGetOwnersSubArrayForInteriorFaces;
neighbours_f = cfdGetNeighboursSubArrayForInteriorFaces;
iFaces = 1:theNumberOfInteriorFaces;

% Calculated info
n = cfdUnit(Sf);
e = cfdUnit(CF);

CF_limited = max(dot(n',CF'), 0.05*cfdMag(CF)')';

magSf = cfdMag(Sf);
magCF = cfdMag(CF_limited);

% Get fields
Ui = cfdGetSubArrayForInterior('U',iComponent);

UGrad = cfdGetGradientSubArrayForInterior('U');
UGrad_f = cfdInterpolateGradientsFromElementsToInteriorFaces('linear',UGrad);
tUGrad_f = cfdTransp(UGrad_f);


% Get viscosity
mu = cfdGetSubArrayForInterior('mu');
mu_f = cfdInterpolateFromElementsToInteriorFaces('linear',mu);

% Minimum correction approach
Ef = [magSf.*e(:,1),magSf.*e(:,2),magSf.*e(:,3)];

% Calculate non-orthogonal complement of Sf
Tf = Sf - Ef;

% Geometric diffusion
geoDiff_f = cfdMag(Ef)./magCF;

% Linear fluxes
local_FluxCf =   mu_f.*geoDiff_f;
local_FluxFf = - mu_f.*geoDiff_f;

% Non-orthogonal part
local_FluxVf = - mu_f.*dot(UGrad_f(:,:,iComponent)',Tf')';

% Add transpose term
local_FluxVf = local_FluxVf + mu_f.*dot(tUGrad_f(:,:,iComponent)',Sf')';

% Add Bulk viscosity term
if cfdIsCompressible
    local_FluxVf = local_FluxVf + 2/3.*mu_f.*cfdTrace(UGrad_f).*Sf(:,iComponent);
end

% Update global fluxes
theFluxes = cfdGetFluxes;

theFluxes.FluxCf(iFaces,1) = local_FluxCf;
theFluxes.FluxFf(iFaces,1) = local_FluxFf;
theFluxes.FluxVf(iFaces,1) = local_FluxVf;
theFluxes.FluxTf(iFaces,1) = local_FluxCf.*Ui(owners_f) + local_FluxFf.*Ui(neighbours_f) + local_FluxVf;

cfdSetFluxes(theFluxes);

end

%===================================================
% WALL - noSlip
%===================================================
function cfdAssembleStressTermWallNoslipBC(iBPatch, iComponent)

% Get info
Sf_b = cfdGetFaceSfSubArrayForBoundaryPatch(iBPatch);
wallDist_b = cfdGetWallDistLimitedSubArrayForBoundaryPatch(iBPatch);
owners_b = cfdGetOwnersSubArrayForBoundaryPatch(iBPatch);

iBFaces = cfdGetBFaceIndicesForBoundaryPatch(iBPatch);

% Calculated arrays
magSb = cfdMag(Sf_b);

% Get required fields
mu_b = cfdGetSubArrayForBoundaryPatch('mu', iBPatch);

U = cfdGetDataArray('U');
U_C = U(owners_b,:);
U_b = cfdGetSubArrayForBoundaryPatch('U', iBPatch);

u_C = U_C(:,1);
v_C = U_C(:,2);
w_C = U_C(:,3);

u_b = U_b(:,1);
v_b = U_b(:,2);
w_b = U_b(:,3);

% Normals and components
n = cfdUnit(Sf_b); 

nx = n(:,1); 
ny = n(:,2); 
nz = n(:,3);

nx2 = dot(nx',nx')';
ny2 = dot(ny',ny')';
nz2 = dot(nz',nz')';

% Local fluxes
if iComponent==1
    local_FluxCb =  mu_b.*magSb./wallDist_b.*(1-nx2);  
    local_FluxFb =  zeros(size(local_FluxCb));
    local_FluxVb = -mu_b.*magSb./wallDist_b.*(u_b.*(1-nx2)+(v_C-v_b).*ny.*nx+(w_C-w_b).*nz.*nx);
elseif iComponent==2
    local_FluxCb =  mu_b.*magSb./wallDist_b.*(1-ny2);    
    local_FluxFb =  zeros(size(local_FluxCb));
    local_FluxVb = -mu_b.*magSb./wallDist_b.*((u_C-u_b).*nx.*ny+v_b.*(1-ny2)+(w_C-w_b).*nz.*ny);
else
    local_FluxCb =  mu_b.*magSb./wallDist_b.*(1-nz2);    
    local_FluxFb =  zeros(size(local_FluxCb));
    local_FluxVb = -mu_b.*magSb./wallDist_b.*((u_C-u_b).*nx.*nz+(v_C-v_b).*ny.*nz+w_b.*(1-nz2));
end

% Update global fluxes
theFluxes = cfdGetFluxes;

theFluxes.FluxCf(iBFaces) = local_FluxCb;
theFluxes.FluxFf(iBFaces) = local_FluxFb;
theFluxes.FluxVf(iBFaces) = local_FluxVb;
theFluxes.FluxTf(iBFaces) = local_FluxCb.*U_C(:,iComponent) + local_FluxFb.*U_b(:,iComponent) + local_FluxVb;

cfdSetFluxes(theFluxes);

end

%===================================================
% INLET - Fixed Value
%===================================================
function cfdAssembleStressTermInletFixedValueBC(iBPatch, iComponent)

% Get info
Sf_b = cfdGetFaceSfSubArrayForBoundaryPatch(iBPatch);
wallDist_b = cfdGetWallDistLimitedSubArrayForBoundaryPatch(iBPatch);
owners_b = cfdGetOwnersSubArrayForBoundaryPatch(iBPatch);

iBFaces = cfdGetBFaceIndicesForBoundaryPatch(iBPatch);

% Get velocity for current component
Ui_C = cfdGetDataArray('U', iComponent);
Ui_b = cfdGetSubArrayForBoundaryPatch('U', iBPatch, iComponent);

% Get viscosity
mu_b = cfdGetSubArrayForBoundaryPatch('mu', iBPatch);

% Geometric diffusion
geoDiff_b = cfdMag(Sf_b)./wallDist_b;

% Linear fluxes
local_FluxCb =  mu_b.*geoDiff_b;
local_FluxFb = -mu_b.*geoDiff_b;

% non-linear fluxes
local_FluxVb = zeros(size(local_FluxFb));

% Get fluxes
theFluxes = cfdGetFluxes;

theFluxes.FluxCf(iBFaces) = local_FluxCb;
theFluxes.FluxFf(iBFaces) = local_FluxFb;
theFluxes.FluxVf(iBFaces) = local_FluxVb;
theFluxes.FluxTf(iBFaces) = local_FluxCb.*Ui_C(owners_b) + local_FluxFb.*Ui_b + local_FluxVb;

cfdSetFluxes(theFluxes);

end


%===================================================
% Inlet - Zero Gradient
%===================================================
function cfdAssembleStressTermInletZeroGradientBC(iBPatch, iComponent)

% Get info
Sf_b = cfdGetFaceSfSubArrayForBoundaryPatch(iBPatch);
wallDist_b = cfdGetWallDistLimitedSubArrayForBoundaryPatch(iBPatch);
owners_b = cfdGetOwnersSubArrayForBoundaryPatch(iBPatch);

iBFaces = cfdGetBFaceIndicesForBoundaryPatch(iBPatch);

% Get velocity for current component
Ui_C = cfdGetDataArray('U', iComponent);
Ui_b = cfdGetSubArrayForBoundaryPatch('U', iBPatch, iComponent);

% Get viscosity
mu_b = cfdGetSubArrayForBoundaryPatch('mu', iBPatch);

% Geometric diffusion
geoDiff_b = cfdMag(Sf_b)./wallDist_b;

% Linear fluxes
local_FluxCb =  mu_b.*geoDiff_b;
local_FluxFb = -mu_b.*geoDiff_b;

% non-linear fluxes
local_FluxVb = zeros(size(local_FluxFb));

% Get fluxes
theFluxes = cfdGetFluxes;

theFluxes.FluxCf(iBFaces) = local_FluxCb;
theFluxes.FluxFf(iBFaces) = local_FluxFb;
theFluxes.FluxVf(iBFaces) = local_FluxVb;
theFluxes.FluxTf(iBFaces) = local_FluxCb.*Ui_C(owners_b) + local_FluxFb.*Ui_b + local_FluxVb;

cfdSetFluxes(theFluxes);

end



%===================================================
% OUTLET - Zero Gradient
%===================================================
function cfdAssembleStressTermOutletZeroGradientBC(iBPatch, iComponent)

% Get info
Sf_b = cfdGetFaceSfSubArrayForBoundaryPatch(iBPatch);
wallDist_b = cfdGetWallDistLimitedSubArrayForBoundaryPatch(iBPatch);
owners_b = cfdGetOwnersSubArrayForBoundaryPatch(iBPatch);

iBFaces = cfdGetBFaceIndicesForBoundaryPatch(iBPatch);

% Get velocity for current component
Ui_C = cfdGetDataArray('U', iComponent);
Ui_b = cfdGetSubArrayForBoundaryPatch('U', iBPatch, iComponent);

% Get viscosity
mu_b = cfdGetSubArrayForBoundaryPatch('mu', iBPatch);

% Geometric diffusion
geoDiff_b = cfdMag(Sf_b)./wallDist_b;

% Linear fluxes
local_FluxCb =  mu_b.*geoDiff_b;
local_FluxFb = -mu_b.*geoDiff_b;

% non-linear fluxes
local_FluxVb = zeros(size(local_FluxFb));

% Get fluxes
theFluxes = cfdGetFluxes;

theFluxes.FluxCf(iBFaces) = local_FluxCb;
theFluxes.FluxFf(iBFaces) = local_FluxFb;
theFluxes.FluxVf(iBFaces) = local_FluxVb;
theFluxes.FluxTf(iBFaces) = local_FluxCb.*Ui_C(owners_b) + local_FluxFb.*Ui_b + local_FluxVb;

cfdSetFluxes(theFluxes);

end


%===================================================
% INLET - Fixed Value
%===================================================
function cfdAssembleStressTermOutletFixedValueBC(iBPatch, iComponent)

% Get info
Sf_b = cfdGetFaceSfSubArrayForBoundaryPatch(iBPatch);
wallDist_b = cfdGetWallDistLimitedSubArrayForBoundaryPatch(iBPatch);
owners_b = cfdGetOwnersSubArrayForBoundaryPatch(iBPatch);

iBFaces = cfdGetBFaceIndicesForBoundaryPatch(iBPatch);

% Get velocity for current component
Ui_C = cfdGetDataArray('U', iComponent);
Ui_b = cfdGetSubArrayForBoundaryPatch('U', iBPatch, iComponent);

% Get viscosity
mu_b = cfdGetSubArrayForBoundaryPatch('mu', iBPatch);

% Geometric diffusion
geoDiff_b = cfdMag(Sf_b)./wallDist_b;

% Linear fluxes
local_FluxCb =  mu_b.*geoDiff_b;
local_FluxFb = -mu_b.*geoDiff_b;

% non-linear fluxes
local_FluxVb = zeros(size(local_FluxFb));

% Get fluxes
theFluxes = cfdGetFluxes;

theFluxes.FluxCf(iBFaces) = local_FluxCb;
theFluxes.FluxFf(iBFaces) = local_FluxFb;
theFluxes.FluxVf(iBFaces) = local_FluxVb;
theFluxes.FluxTf(iBFaces) = local_FluxCb.*Ui_C(owners_b) + local_FluxFb.*Ui_b + local_FluxVb;

cfdSetFluxes(theFluxes);

end


%===================================================
% SYMMETRY
%===================================================
function cfdAssembleStressTermSymmetry(iBPatch, iComponent)

% Get info
Sf_b = cfdGetFaceSfSubArrayForBoundaryPatch(iBPatch);
wallDist_b = cfdGetWallDistLimitedSubArrayForBoundaryPatch(iBPatch);
owners_b = cfdGetOwnersSubArrayForBoundaryPatch(iBPatch);

iBFaces = cfdGetBFaceIndicesForBoundaryPatch(iBPatch);

% Calculated info
magSb = cfdMag(Sf_b);

% Get required fields
mu_b = cfdGetSubArrayForBoundaryPatch('mu', iBPatch);

U = cfdGetDataArray('U');
U_C = U(owners_b,:);
U_b = cfdGetSubArrayForBoundaryPatch('U', iBPatch);

u_C = U_C(:,1);
v_C = U_C(:,2);
w_C = U_C(:,3);

% Normals and components
n = cfdUnit(Sf_b); 

nx = n(:,1); 
ny = n(:,2); 
nz = n(:,3);

nx2 = dot(nx',nx')';
ny2 = dot(ny',ny')';
nz2 = dot(nz',nz')';

% Local fluxes
if iComponent==1
    local_FluxCb = 2*mu_b.*magSb./wallDist_b.*nx2; 
    local_FluxFb = zeros(size(local_FluxCb));
    local_FluxVb = 2*mu_b.*magSb./wallDist_b.*((v_C.*ny+w_C.*nz).*nx);
elseif iComponent==2
    local_FluxCb = 2*mu_b.*magSb./wallDist_b.*ny2;   
    local_FluxFb = zeros(size(local_FluxCb));
    local_FluxVb = 2*mu_b.*magSb./wallDist_b.*((u_C.*nx+w_C.*nz).*ny);
else
    local_FluxCb = 2*mu_b.*magSb./wallDist_b.*nz2;  
    local_FluxFb = zeros(size(local_FluxCb));
    local_FluxVb = 2*mu_b.*magSb./wallDist_b.*((u_C.*nx+v_C.*ny).*nz);
end

% Update global fluxes
theFluxes = cfdGetFluxes;

theFluxes.FluxCf(iBFaces) = local_FluxCb;
theFluxes.FluxFf(iBFaces) = local_FluxFb;
theFluxes.FluxVf(iBFaces) = local_FluxVb;
theFluxes.FluxTf(iBFaces) = local_FluxCb.*U_C(:,iComponent) + local_FluxFb.*U_b(:,iComponent) + local_FluxVb;

cfdSetFluxes(theFluxes);

end


%===================================================
% EMPTY
%===================================================
function cfdAssembleStressTermEmptyBC(iBPatch, iComponent)

end

%===================================================
% Wall- specifiedValue
%===================================================
function  cfdAssembleStressTermWallFixedValueBC(iBPatch, iComponent)

% Get info
Sf_b = cfdGetFaceSfSubArrayForBoundaryPatch(iBPatch);
wallDist_b = cfdGetWallDistSubArrayForBoundaryPatch(iBPatch);
owners_b = cfdGetOwnersSubArrayForBoundaryPatch(iBPatch);

iBFaces = cfdGetBFaceIndicesForBoundaryPatch(iBPatch);

% Calculated info
magSb = cfdMag(Sf_b);

% Get required fields
mu_b = cfdGetSubArrayForBoundaryPatch('mu', iBPatch);

U = cfdGetDataArray('U');
U_C = U(owners_b,:);
U_b = cfdGetSubArrayForBoundaryPatch('U', iBPatch);

u_C = U_C(:,1);
v_C = U_C(:,2);
w_C = U_C(:,3);

u_b = U_b(:,1);
v_b = U_b(:,2);
w_b = U_b(:,3);

% Normals and components
n = cfdUnit(Sf_b); 

nx = n(:,1); 
ny = n(:,2); 
nz = n(:,3);

nx2 = dot(nx',nx')';
ny2 = dot(ny',ny')';
nz2 = dot(nz',nz')';

% Local fluxes
if iComponent==1
    local_FluxCb =  mu_b.*magSb./wallDist_b.*(1-nx2);  
    local_FluxFb =  zeros(size(local_FluxCb));
    local_FluxVb = -mu_b.*magSb./wallDist_b.*(u_b.*(1-nx2)+(v_C-v_b).*ny.*nx+(w_C-w_b).*nz.*nx);
elseif iComponent==2
    local_FluxCb =  mu_b.*magSb./wallDist_b.*(1-ny2);    
    local_FluxFb =  zeros(size(local_FluxCb));
    local_FluxVb = -mu_b.*magSb./wallDist_b.*((u_C-u_b).*nx.*ny+v_b.*(1-ny2)+(w_C-w_b).*nz.*ny);
else
    local_FluxCb =  mu_b.*magSb./wallDist_b.*(1-nz2);    
    local_FluxFb =  zeros(size(local_FluxCb));
    local_FluxVb = -mu_b.*magSb./wallDist_b.*((u_C-u_b).*nx.*nz+(v_C-v_b).*ny.*nz+w_b.*(1-nz2));
end

% Update global fluxes
theFluxes = cfdGetFluxes;

theFluxes.FluxCf(iBFaces) = local_FluxCb;
theFluxes.FluxFf(iBFaces) = local_FluxFb;
theFluxes.FluxVf(iBFaces) = local_FluxVb;
theFluxes.FluxTf(iBFaces) = local_FluxCb.*U_C(:,iComponent) + local_FluxFb.*U_b(:,iComponent) + local_FluxVb;

cfdSetFluxes(theFluxes);

end

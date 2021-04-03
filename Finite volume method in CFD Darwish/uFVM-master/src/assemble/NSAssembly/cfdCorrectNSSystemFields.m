function cfdCorrectNSSystemFields
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   
%--------------------------------------------------------------------------

% Correct Pressure correction field
setupPressureCorrection;

cfdCorrectPressureCorrectionForInterior;
cfdUpdateScalarFieldForAllBoundaryPatches('pp');
cfdUpdateGradient('pp');

% Correct mdot_f
cfdCorrectMdot;

% Correct pressure
cfdCorrectPressureForInterior;
cfdCorrectPressureForBoundaryPatches;

% Correct U
cfdCorrectVelocityForInterior;
cfdCorrectVelocityForBoundaryPatches(1);
cfdCorrectVelocityForBoundaryPatches(2);
cfdCorrectVelocityForBoundaryPatches(3);

cfdUpdateProperty('rho');

% Updates
cfdUpdateScalarFieldForAllBoundaryPatches('p');
cfdUpdateScalarFieldForAllBoundaryPatches('rho');
cfdUpdateVectorFieldForAllBoundaryPatches('U');

% Update scales
cfdUpdateScale('p');
cfdUpdateScale('U');
cfdUpdateScale('rho');

% Update gradients
cfdUpdateGradient('rho');
cfdUpdateGradient('p');
cfdUpdateGradient('U');

end


function setupPressureCorrection

% Get correction
dphi = cfdGetDPhi;

% Check if needs reference pressure
pRefValue = cfdReferencePressure;
if cfdNeedPressureLevel
    pRefCell = cfdGetRefCell;
    pRefValue = pRefValue + dphi(pRefCell);
end

% Update correction
dphi = dphi - pRefValue;
cfdSetDPhi(dphi);

end



function cfdCorrectPressureCorrectionForInterior

% Get info
theNumberOfElements = cfdGetNumberOfElements;
iElements = 1:theNumberOfElements;

% Get field
thePPField = cfdGetMeshField('pp');

% Get correction
dphi = cfdGetDPhi;

% Update pressure correction field in data base
thePPField.phi(iElements) = dphi(iElements);

% Reset correction
dphi = zeros(theNumberOfElements,1);
cfdSetDPhi(dphi);

% Store
cfdSetMeshField(thePPField);

end


function cfdCorrectVelocityForInterior

% Get info
theNumberOfElements = cfdGetNumberOfElements;
iElements = 1:theNumberOfElements;

% Get fields
DU1 = cfdGetSubArrayForInterior('DU1');
DU2 = cfdGetSubArrayForInterior('DU2');
DU3 = cfdGetSubArrayForInterior('DU3');

ppGrad = cfdGetGradientSubArrayForInterior('pp');

theVelocityField = cfdGetMeshField('U');
U = theVelocityField.phi;

% Calculate Dc*gradP'
DUPPGRAD = [DU1.*ppGrad(iElements,1),DU2.*ppGrad(iElements,2),DU3.*ppGrad(iElements,3)];

% Correct velocity
U(iElements,:) = U(iElements,:) - DUPPGRAD(iElements,:);

% Update velocity field in data base
theVelocityField.phi = U;

% Store
cfdSetMeshField(theVelocityField);

end

function cfdCorrectPressureForInterior

% Get info
theNumberOfElements = cfdGetNumberOfElements;
iElements = 1:theNumberOfElements;

% Get pressure field
thePressureField = cfdGetMeshField('p');
p = thePressureField.phi;

% Get limits
theMaximumAcceptedValue = cfdMaximumAcceptedValue('p');
theMinimumAcceptedValue = cfdMinimumAcceptedValue('p');

% Get pressure correction field
thePPField  = cfdGetMeshField('pp');
pp = thePPField.phi;

% Get pressure under-relaxation
urf_p = cfdGetFieldRelaxationFactor('p');

% Update pressure field
if cfdIsCompressible    
    for iElement=iElements
        pcorr = urf_p*pp(iElement);
        p_new = p(iElement) + pcorr;        
        
        if p_new>theMaximumAcceptedValue
            p_new = p(iElement) + 0.75*(theMaximumAcceptedValue-p(iElement));
        elseif p_new<theMinimumAcceptedValue
            p_new = p(iElement) + 0.75*(theMinimumAcceptedValue-p(iElement));
        end
        p(iElement) = p_new;
    end        
else
	% Update pressure field with explicit under-relaxation
    p(iElements) =  p(iElements) + urf_p*pp(iElements);
end

% Update and store
thePressureField.phi = p;
cfdSetMeshField(thePressureField);

end

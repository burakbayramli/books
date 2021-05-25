function cfdAssembleIntoGlobalMatrixElementFluxes
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018 
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function assembles algebraic equation coefficients from the
%   contribution of the element fluxes of the current term of the equation
%--------------------------------------------------------------------------

% Get fluxes
theFluxes = cfdGetFluxes;

% Get coefficients structure
theCoefficients = cfdGetCoefficients;

% Call coefficients
ac = theCoefficients.ac;
ac_old = theCoefficients.ac_old;
bc = theCoefficients.bc;

% Assemble element fluxes
for iElement = 1:cfdGetNumberOfElements
    ac(iElement)     = ac(iElement)     + theFluxes.FluxC(iElement);
    ac_old(iElement) = ac_old(iElement) + theFluxes.FluxC_old(iElement);
    bc(iElement)     = bc(iElement)     - theFluxes.FluxT(iElement);
end

% Store updated coefficients
theCoefficients.ac = ac;
theCoefficients.ac_old = ac_old;
theCoefficients.bc = bc;

cfdSetCoefficients(theCoefficients);

end
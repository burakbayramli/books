function cfdAssembleImplicitRelaxation(theEquationName)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function assembles implicit under-relaxation factor
%--------------------------------------------------------------------------

urf = cfdGetEquationRelaxationFactor(theEquationName);

theCoefficients = cfdGetCoefficients;
theCoefficients.ac = theCoefficients.ac/urf;

cfdSetCoefficients(theCoefficients);
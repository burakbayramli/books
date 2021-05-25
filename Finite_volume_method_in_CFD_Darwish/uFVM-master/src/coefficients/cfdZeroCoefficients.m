function cfdZeroCoefficients
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function zeros the coefficients
%--------------------------------------------------------------------------

% Get info
theNumberOfElements = cfdGetNumberOfElements;

% Get coefficients
theCoefficients = cfdGetCoefficients;

% Reset components
theCoefficients.ac = zeros(theNumberOfElements,1);
theCoefficients.ac_old = zeros(theNumberOfElements,1);
theCoefficients.bc = zeros(theNumberOfElements,1);
theCoefficients.dc = zeros(theNumberOfElements,1);
theCoefficients.rc = zeros(theNumberOfElements,1);
theCoefficients.dphi = zeros(theNumberOfElements,1);
theCoefficients.anb = cell(theNumberOfElements,1);

for iElement=1:theNumberOfElements
    theCoefficients.anb{iElement} = zeros(1,theCoefficients.csize(iElement));
end

theCoefficients.numberOfElements = theNumberOfElements;

% Store
cfdSetCoefficients(theCoefficients);

function theCoefficients = cfdSetupCoefficients(theCConn, theCSize)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function sets up the coefficients
%--------------------------------------------------------------------------

% Default settings
if nargin==0    
    theCConn = cfdGetElementNbIndices;
    theCSize = cfdLabelList(length(theCConn));
    for iElement=1:length(theCConn)
        theCSize(iElement) = length(theCConn{iElement});
    end
end
theNumberOfElements = length(theCConn);

% Define and initialize
ac     = cfdScalarList(theNumberOfElements);
ac_old = cfdScalarList(theNumberOfElements);
bc     = cfdScalarList(theNumberOfElements);

anb = cell(theNumberOfElements,1);
for iElement=1:theNumberOfElements
    anb{iElement} = zeros(1,theCSize(iElement));
end

% dc & rc added to be used in ILU Solver
dc = cfdScalarList(theNumberOfElements);
rc = cfdScalarList(theNumberOfElements);

% Correction
dphi = cfdScalarList(theNumberOfElements);

% Store in structure
theCoefficients.ac = ac;
theCoefficients.ac_old = ac_old;
theCoefficients.bc = bc;
theCoefficients.anb = anb;
theCoefficients.dc = dc;
theCoefficients.rc = rc;
theCoefficients.dphi = dphi;
theCoefficients.cconn = theCConn;
theCoefficients.csize = theCSize;
theCoefficients.numberOfElements = theNumberOfElements;

function cfdAssembleDCoefficients(iComponent)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%
%--------------------------------------------------------------------------

% Get info
theNumberOfElements = cfdGetNumberOfElements;
volumes = cfdGetVolumesForElements;

% Get coefficients
theCoefficients = cfdGetCoefficients;

ac  = theCoefficients.ac;
ac_old  = theCoefficients.ac_old;

theDUField = cfdGetMeshField(['DU' num2str(iComponent)]);
theDUTField = cfdGetMeshField(['DUT' num2str(iComponent)]);

if strcmp(cfdGetAlgorithm,'SIMPLE')    
    theDUField.phi(1:theNumberOfElements) = volumes./ac;
    theDUTField.phi(1:theNumberOfElements) = ac_old./ac;            
elseif(strcmp(theAlgorithm,'PIMPLE'))
    % BODGE
end

% Store in data base
cfdSetMeshField(theDUField);
cfdSetMeshField(theDUTField);

% Update at cfdBoundary patches
cfdUpdateScalarFieldForAllBoundaryPatches(theDUField.name);
cfdUpdateScalarFieldForAllBoundaryPatches(theDUTField.name);
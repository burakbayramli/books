function cfdPostAssembleEquation(theEquationName, iComponent)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   
%--------------------------------------------------------------------------

if nargin==1
    iComponent = 1;
end
%
theCoefficients = cfdGetCoefficients;
%
theMesh = cfdGetMesh;
iElements = [1:theMesh.numberOfElements]';
%
ac  = theCoefficients.ac;
ac_old  = theCoefficients.ac_old;
anb = theCoefficients.anb;
bc  = theCoefficients.bc;

%---------------------------------------------------
% Momentum Equations
%---------------------------------------------------
if strcmp(theEquationName,'U')
    
    theDUField = cfdGetMeshField(['DU' num2str(iComponent)]);
    theDUTField = cfdGetMeshField(['DUT' num2str(iComponent)]);
    
    %---------------------------------------------------------------------------
    %  Computations @ CELLS
    %---------------------------------------------------------------------------
    theAlgorithm = cfdGetAlgorithm;
    %
    % SIMPLE
    %
    if strcmp(theAlgorithm,'SIMPLE')
        volume = [theMesh.elements.volume]';
        
        theDUField.phi(iElements) = volume./ac;
        theDUTField.phi(iElements) = ac_old./ac;
    end
    %
    % PIMPLE
    %
    if(strcmp(theAlgorithm,'PIMPLE'))
        
        
    end
    
    %---------------------------------------------------------------------------
    %  Computations @ BOUNDARY
    %---------------------------------------------------------------------------
    iBElements = [theMesh.numberOfElements+1:theMesh.numberOfElements+theMesh.numberOfBFaces]';
    
    iBFaces= [theMesh.numberOfInteriorFaces+1:theMesh.numberOfFaces]';
    iOwners = [theMesh.faces(iBFaces).iOwner]';
    
    theDUField.phi(iBElements) = theDUField.phi(iOwners);
    theDUTField.phi(iBElements) = theDUTField.phi(iOwners);
    
    %
    cfdSetMeshField(theDUField);
    cfdSetMeshField(theDUTField);          
end

theCoefficients.ac = ac;
theCoefficients.anb = anb;
theCoefficients.bc = bc;

cfdSetCoefficients(theCoefficients);

end

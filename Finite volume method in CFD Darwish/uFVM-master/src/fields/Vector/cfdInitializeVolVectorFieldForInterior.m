function cfdInitializeVolVectorFieldForInterior(theFieldName)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%  Update cfdVector field from input values at interior elements
%--------------------------------------------------------------------------

% Get mesh and Properties
theMesh = cfdGetMesh;
theNumberOfElements = theMesh.numberOfElements;

% Get field
theField = cfdGetModel(theFieldName);
theInitialValue = eval(theField.ic);

theMeshField = cfdGetMeshField(theFieldName);
phi = theMeshField.phi;

% Check if input value is a list
if length(theInitialValue)==theNumberOfElements
    % Special case if number of elements is equal to number of cfdVector
    % components
    if theNumberOfElements==3
        if length(theInitialValue(1))==3
            for iComponent=1:3
                phi(1:theNumberOfElements,iComponent) = theInitialValue(:,iComponent);
            end
        else
            for iComponent=1:3
                phi(1:theNumberOfElements,iComponent) = theInitialValue(iComponent) .* ones(theNumberOfElements,1);
            end
        end
    else
        for iComponent=1:3
            phi(1:theNumberOfElements,iComponent) = theInitialValue(:,iComponent);
        end
    end
else
    for iComponent=1:3
        phi(1:theNumberOfElements,iComponent) = theInitialValue(iComponent) .* ones(theNumberOfElements,1);
    end
end

% Store
theMeshField.phi = phi;
cfdSetMeshField(theMeshField);
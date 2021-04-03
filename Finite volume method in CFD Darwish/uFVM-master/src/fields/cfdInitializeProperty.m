function cfdInitializeProperty(theFieldName)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function initializes the property field
%--------------------------------------------------------------------------

theMesh = cfdGetMesh;
theField = cfdGetModel(theFieldName);

if isfield(theField,'constant')
    theNumberOfElements = theMesh.numberOfElements;
    theNumberOfBElements =  theMesh.numberOfBElements;
    
    theConstant = theField.constant;
    thePropertyMeshField = cfdGetMeshField(theFieldName);
    
    if(strcmp(theField.type,'Scalar'))
        if ischar(theConstant)
            thePropertyMeshField.phi = eval(theConstant) .* ones(theNumberOfElements+theNumberOfBElements,1);
        else
            thePropertyMeshField.phi = theConstant .* ones(theNumberOfElements+theNumberOfBElements,1);
        end
    elseif(strcmp(theField.type,'Vector'))
        for iComponent=1:3
            theComponentFormula = getFormulaForComponent(theConstant,iComponent);
            thePropertyMeshField.phi(:,iComponent) = eval(theComponentFormula) .* ones(theNumberOfElements+theNumberOfBElements,1);
        end
    end
    cfdSetMeshField(thePropertyMeshField)
    %
elseif isfield(theField,'model')
    theFormula = theField.model;    
    theNumberOfElements = theMesh.numberOfElements;
    theNumberOfBElements =  theMesh.numberOfBElements;
    
    thePropertyMeshField = cfdGetMeshField(theFieldName);
    
    theFormulaFields = identifyFields(theFormula);
    for iTermField=1:length(theFormulaFields)
        theFormulaFieldName = theFormulaFields{iTermField};
        theMeshField = cfdGetMeshField(theFormulaFieldName);
        
        % If the required parameter is not a field, then it is a constant
        if isempty(theMeshField)
            theConstantValue = cfdGetConstant(theFormulaFieldName);
            tt = [theFormulaFieldName ' = theConstantValue;'];
            eval(tt);
        else
            tt = [theFormulaFieldName ' = theMeshField.phi;'];
            eval(tt);
        end
    end
    
    % Change arithmetic operators to be consistent for array operations
    if isempty(strfind(theFormula, '.*')) && isempty(strfind(theFormula, './')) && isempty(strfind(theFormula, '.^'))
        theFormula = strrep(theFormula, '*', '.*');
        theFormula = strrep(theFormula, '/', './');
        theFormula = strrep(theFormula, '^', '.^');
    end
    
    if(strcmp(theField.type,'Scalar'))
        thePropertyMeshField.phi = eval(theFormula);
    elseif(strcmp(theField.type,'Vector'))
        for iComponent=1:3
            theComponentFormula = getFormulaForComponent(theFormula,iComponent);
            thePropertyMeshField.phi(:,iComponent) = eval(theComponentFormula) .* ones(theNumberOfElements+theNumberOfBElements,1);
        end
    end
    
    cfdSetMeshField(thePropertyMeshField);
elseif isfield(theField,'recipe')
    thePropertyMeshField = cfdGetMeshField(theFieldName);
    theNumberOfFluids = cfdGetNumberOfFluids;
    
    if strcmp(theField.recipe,'Average')
        phi = zeros(size(thePropertyMeshField.phi));
        for iFluid=1:theNumberOfFluids            
            thePropertyField = cfdGetMeshField(theFieldName);
            phi = phi + thePropertyField.phi;
        end
        thePropertyMeshField.phi = phi/theNumberOfFluids;
        cfdSetMeshField(thePropertyMeshField);
    elseif strcmp(theField.recipe,'Mixture')
        phi = zeros(size(thePropertyMeshField.phi));
        for iFluid=1:theNumberOfFluids           
            thePropertyField = cfdGetMeshField([theFieldName, num2str(iFluid)]);
            theVFField = cfdGetMeshField(['alpha', num2str(iFluid)]);
            vf = theVFField.phi;
            phi = phi + vf.*thePropertyField.phi;
        end
        thePropertyMeshField.phi = phi;
        cfdSetMeshField(thePropertyMeshField);
    end
end



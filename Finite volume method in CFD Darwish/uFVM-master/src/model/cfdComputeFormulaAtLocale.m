function phi = cfdComputeFormulaAtLocale(theFormula, theLocale, theType, iComponent)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018 
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%  computes the formula over the centroids of theLocale
%  which can be 'Elements', 'Faces' or 'Nodes'
%  theType can be a 'Scalar' or 'Vector'
%  for 'Vector the formula should be written as
%  [formula_x; formula_x; formula_z]
%--------------------------------------------------------------------------

if(nargin<3)
    theType='Scalar';
    iComponent = 1;
end
if(nargin<2)
    theLocale = 'Elements';
    iComponent = 1;
end
%
theMesh = cfdGetMesh;
%
if (strcmp(theLocale,'Elements'))
    %
    %---------------------------------------------------
    % compute formula for elements
    %---------------------------------------------------
    %
    theNumberOfElements = theMesh.numberOfElements;
    theNumberOfInteriorFaces = theMesh.numberOfInteriorFaces;
    theNumberOfFaces = theMesh.numberOfFaces;
    
    theNumberOfBElements =  theMesh.numberOfBElements;
    theElementCentroids = [theMesh.elements(:).centroid]';
    theBElementCentroids = [theMesh.faces(theNumberOfInteriorFaces+1:theNumberOfFaces).centroid]';
         
    if ~ischar(theFormula)
        if size(theFormula, 1) == theNumberOfElements
            phi = theFormula;
        end        
        return;
    end
    
    theCentroids = [theElementCentroids; theBElementCentroids];
    
    x = theCentroids(:,1);
    y = theCentroids(:,2);
    z = theCentroids(:,3);
    
    %
    % Loop over the defined fields and check if the formula includes any of
    % them
    %
    theTermFields = identifyFields(theFormula);
    if isempty(theTermFields)
        if(strcmp(theType,'Scalar'))
            phi = eval(theFormula) .* ones(theNumberOfElements+theNumberOfBElements,1);
        elseif(strcmp(theType,'Vector'))
            for iComponent=1:3
                theComponentFormula = cfdGetFormulaForComponent(theFormula,iComponent);
                phi(:,iComponent) = eval(theComponentFormula) .* ones(theNumberOfElements+theNumberOfBElements,1);
            end
        end
    else        
        for iTermField=1:length(theTermFields)
            theFieldName = theTermFields{iTermField};
            theMeshField = cfdGetMeshField(theFieldName);
            
            % If the required parameter is not a field, then it is a constant
            if isempty(theMeshField)
                theConstantValue = cfdGetConstant(theFieldName);
                tt = [theFieldName ' = theConstantValue;'];
                eval(tt);
            else
                if strcmp(theMeshField.type, 'Scalar')
                    tt = [theMeshField.name ' = theMeshField.phi;'];
                else
                    tt = [theMeshField.name ' = theMeshField.phi(:,iComponent);'];
                end
                eval(tt);
            end
        end
        
        % Change arithmetic operators to be consistent for array operations
        theFormula = strrep(theFormula, '*', '.*');
        theFormula = strrep(theFormula, '/', './');
        theFormula = strrep(theFormula, '^', '.^');
        
        if(strcmp(theType,'Scalar'))
            phi = eval(theFormula);
        elseif(strcmp(theType,'Vector'))
            for iComponent=1:3
                phi(:,iComponent) = eval(theFormula);
            end
        end        
    end   
    
elseif(strcmp(theLocale,'Interior Elements'))
    %
    %---------------------------------------------------
    % compute formula for elements
    %---------------------------------------------------
    %
    theNumberOfElements = theMesh.numberOfElements;
    theElementCentroids = [theMesh.elements(:).centroid]';    
    
    if ~ischar(theFormula)
        if size(theFormula, 1) == theNumberOfElements
            phi = theFormula;
        end        
        return;
    end 
        
    theCentroids = theElementCentroids;
    
    x = theCentroids(:,1);
    y = theCentroids(:,2);
    z = theCentroids(:,3);    
    
    %
    % Loop over the defined fields and check if the formula includes any of
    % them
    %
    theTermFields = identifyFields(theFormula);
    if isempty(theTermFields)
        if(strcmp(theType,'Scalar'))
            phi = eval(theFormula) .* ones(theNumberOfElements,1);
        elseif(strcmp(theType,'Vector'))
            for iComponent=1:3
                theComponentFormula = cfdGetFormulaForComponent(theFormula,iComponent);
                phi(:,iComponent) = eval(theComponentFormula) .* ones(theNumberOfElements,1);
            end
        end
    else        
        for iTermField=1:length(theTermFields)
            theFieldName = theTermFields{iTermField};
            theMeshField = cfdGetMeshField(theFieldName);
                        
            % If the required parameter is not a field, then it is a constant
            if isempty(theMeshField)
                theConstantValue = cfdGetConstant(theFieldName);
                tt = [theFieldName ' = theConstantValue;'];
                eval(tt);
            else
                if strcmp(theMeshField.type, 'Scalar')
                    tt = [theMeshField.name ' = theMeshField.phi(1:theNumberOfElements);'];
                else
                    tt = [theMeshField.name ' = theMeshField.phi(1:theNumberOfElements,iComponent);'];
                end
                eval(tt);
            end
        end
        
        % Change arithmetic operators to be consistent for array operations
        theFormula = strrep(theFormula, '*', '.*');
        theFormula = strrep(theFormula, '/', './');
        theFormula = strrep(theFormula, '^', '.^');
        if(strcmp(theType,'Scalar'))
            phi = eval(theFormula);
        elseif(strcmp(theType,'Vector'))
            for iComponent=1:3
                phi(:,iComponent) = eval(theFormula);
            end
        end        
    end 
    
elseif(strcmp(theLocale,'Faces'))
    %---------------------------------------------------
    % compute formula for faces
    %---------------------------------------------------
    %
    %
    theNumberOfFaces = theMesh.numberOfFaces;
    
    theCentroids = [theMesh.faces(1:theNumberOfFaces).centroid]';
    
    x=theCentroids(:,1);
    y=theCentroids(:,2);
    z=theCentroids(:,3);
    
    %
    % Evaluate the Field
    %
    if(strcmp(theType,'Scalar'))
        phi = eval(theFormula) .* ones(theNumberOfFaces,1);
    elseif(strcmp(theType,'Vector'))
        for iComponent=1:3
            theComponentFormula = cfdGetFormulaForComponent(theFormula,iComponent);
            phi(:,iComponent) = eval(theComponentFormula) .* ones(theNumberOfFaces,1);
        end
    end
    
elseif(strcmp(theLocale,'Interior Faces'))
    %---------------------------------------------------
    % compute formula for faces
    %---------------------------------------------------
    %
    %
    theNumberOfInteriorFaces = theMesh.numberOfInteriorFaces;
    
    theCentroids = [theMesh.faces(1:theNumberOfInteriorFaces).centroid]';
    
    x=theCentroids(:,1);
    y=theCentroids(:,2);
    z=theCentroids(:,3);
    
    %
    % Evaluate the Field
    %
    if(strcmp(theType,'Scalar'))
        phi = eval(theFormula) .* ones(theNumberOfInteriorFaces,1);
    elseif(strcmp(theType,'Vector'))
        for iComponent=1:3
            theComponentFormula = cfdGetFormulaForComponent(theFormula,iComponent);
            phi(:,iComponent) = eval(theComponentFormula) .* ones(theNumberOfInteriorFaces,1);
        end
    end
    
elseif(strcmp(theLocale,'Nodes'))
    %---------------------------------------------------
    % compute formula for nodes
    %---------------------------------------------------
    
    numberOfNodes = theMesh.numberOfNodes;
    
    theCentroids = [theMesh.nodes(1:numberOfNodes).centroid]';
    
    x=theCentroids(:,1);
    y=theCentroids(:,2);
    z=theCentroids(:,3);
    
    %
    % Evaluate the Field
    %
    if(strcmp(theType,'Scalar'))
        phi = eval(theFormula) .* ones(numberOfNodes,1);
    elseif(strcmp(theType,'Vector'))
        for iComponent=1:3
            theComponentFormula = cfdGetFormulaForComponent(theFormula,iComponent);
            phi(:,iComponent) = eval(theComponentFormula) .* ones(numberOfNodes,1);
        end
    end
    
elseif(strcmp(theLocale(1:6),'BPatch'))
    %---------------------------------------------------
    % compute formula for faces
    %---------------------------------------------------
    %
    %
    iPatch = eval(theLocale(7:length(theLocale)));
    
    theBoundary = theMesh.cfdBoundaries(iPatch);
    
    numberOfBFaces = theBoundary.numberOfBFaces;
    
    startFace = theBoundary.startFace;
    endFace = startFace+numberOfBFaces-1;
    
    theCentroids = [theMesh.faces(startFace:endFace).centroid]';
    
    x=theCentroids(:,1);
    y=theCentroids(:,2);
    z=theCentroids(:,3);
    
    %
    % Evaluate the Field
    %
    if(strcmp(theType,'Scalar'))
        phi = eval(theFormula) .* ones(numberOfBFaces,1);
    elseif(strcmp(theType,'Vector'))
        for iComponent=1:3
            theComponentFormula = cfdGetFormulaForComponent(theFormula,iComponent);
            phi(:,iComponent) = eval(theComponentFormula) .* ones(numberOfBFaces,1);
        end
    end
end



end



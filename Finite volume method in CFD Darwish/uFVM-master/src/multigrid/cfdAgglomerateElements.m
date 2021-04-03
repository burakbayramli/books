function cfdAgglomerateElements(maxLevels, varargin)
%--------------------------------------------------------------------------
%
%  written by the CFD Group @ AUB, 2018 
%  contact us at: cfd@aub.edu.lb
%==========================================================================
% Case Description:
%     Agglomerate using simple point based volume agglomeration
%--------------------------------------------------------------------------

if isempty(varargin)
    minNumberOfElements = 3; 
    minSize = 1;
    maxSize = 8;
end

% Can
fineMesh = cfdGetMesh;

% Initialize the top parent array
theTopParentsArray = -1*ones(1,fineMesh.numberOfElements); 

% hierarchy is a cell which includes coarse meshes in addition to finest
% mesh
hierarchy.meshes = cell(maxLevels+1,1);

hierarchy.meshes{1} = fineMesh;
hierarchy.cfdFineToCoarseAddressing = {};
hierarchy.cfdCoarseToFineAddressing = {};  
hierarchy.cfdFinestToCoarseAddressing = {};
hierarchy.cfdCoarseToFinestAddressing = {};

hierarchy.faceUpperAddressing = {};


% Coarsen elements for each level until criteria met
for iLevel=2:maxLevels+1
    
    % Get properties of finer mesh
    numberOfElements = fineMesh.numberOfElements;
    numberOfNodes = fineMesh.numberOfNodes;
    nodes = fineMesh.nodes;    
    
    % Initialize the parent array
    theParentsArray = -1*ones(1,fineMesh.numberOfElements); 
    
    % First Pass
    nCoarseElements = 1;
    for iNode=1:numberOfNodes
        node = nodes(iNode);
        
        % Skip if the node is associated with a single element
        if length(node.iElements)==1
           continue; 
        end        
        
        % Skip node if any of its elements is aggloemrated
        neiElementAggloemrated = false;
        for iElement=node.iElements
            if theParentsArray(iElement) > 0
                neiElementAggloemrated = true;
            end
        end
        
        if neiElementAggloemrated
            continue;
        end
        
        % Agglomerate control volumes associated with the node
        allAgglomerated = true;
        for iElement=node.iElements
            if theParentsArray(iElement) == -1
                theParentsArray(iElement) = nCoarseElements;
                allAgglomerated = false;
            end
        end
        
        if ~allAgglomerated        
            nCoarseElements = nCoarseElements + 1;
        end
    end

    % Check for orphans (second pass)
    for iElement=1:numberOfElements
        if theParentsArray(iElement) == -1

            elementNeighbours = fineMesh.elements(iElement).iNeighbours;

            notAgglomerated = true;
            for elementNeiIndex=elementNeighbours
                if theParentsArray(elementNeiIndex)>0
                    theParentsArray(iElement) = theParentsArray(elementNeiIndex);
                    notAgglomerated = false;
                    break;
                end
            end
            
            
            % Create new coarse element
            if notAgglomerated
                theParentsArray(iElement) = nCoarseElements; 
               
                for elementNeiIndex=elementNeighbours
                    theParentsArray(elementNeiIndex) = nCoarseElements;                    
                end 
                
                nCoarseElements = nCoarseElements + 1;
            end          
        end
    end
    
    % Decrement
    nCoarseElements = nCoarseElements - 1;
    
    % Stop if number of coarse elements are less than min criterion
    if nCoarseElements<=minNumberOfElements
        hierarchy.meshes(iLevel) = [];
        break;
    end 
    
    % Create children array
    theChildrenArray = cell(nCoarseElements,1);
    for iCoarseElement=1:nCoarseElements
        theChildrenArray{iCoarseElement} = [];
    end
    
    for iElement=1:numberOfElements
        if theParentsArray(iElement) > 0
            elementChildrenSize = size(theChildrenArray{theParentsArray(iElement)});
            theChildrenArray{theParentsArray(iElement)}(elementChildrenSize+1) = iElement;
        end
    end
    
    % Create top parents array and its inverse
    if iLevel==2
        theTopParentsArray = theParentsArray;
    else
        for iElement=1:length(theTopParentsArray)
            theTopParentsArray(iElement) = theTopParentsArray(hierarchy.cfdFinestToCoarseAddressing{iLevel-1}(iElement));
        end        
    end
    
    theBaseChildrenArray = cell(nCoarseElements,1);
    for iCoarseElement=1:nCoarseElements
        theBaseChildrenArray{iCoarseElement} = [];
    end
    
    for iElement=1:length(theTopParentsArray)
        if theTopParentsArray(iElement) > 0
            elementBaseChildrenSize = size(theBaseChildrenArray{theTopParentsArray(iElement)});
            theBaseChildrenArray{theTopParentsArray(iElement)}(elementBaseChildrenSize+1) = iElement;
        end        
    end
    
    % Create Coarse Mesh and Store in hierarchy
    [hierarchy.meshes{iLevel}, faceUpperAddressing] = cfdCreateCoarsePolyMesh(theTopParentsArray);
   
    hierarchy.cfdFineToCoarseAddressing{iLevel-1} = theParentsArray;
    hierarchy.cfdCoarseToFineAddressing{iLevel} = theChildrenArray;    
    hierarchy.cfdFinestToCoarseAddressing = theTopParentsArray;
    hierarchy.cfdCoarseToFinestAddressing = theBaseChildrenArray;
    
    hierarchy.faceUpperAddressing = faceUpperAddressing;
    
    % Update finer mesh
    fineMesh = hierarchy.meshes{iLevel};    
end

% Store mesh hierarchy
global Region;

Region.MG.hierarchy = hierarchy;
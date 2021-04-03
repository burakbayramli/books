function cfdSetupFluid(theFluidUserName,varargin)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function registers a fluid name and creates a corresponding
%   structure containing the fluid details
%--------------------------------------------------------------------------

global Region;

theNumberOfFluids = length(Region.fluids);

if theNumberOfFluids==0
    theFluidIndex = 1;
else 
    theFluid = cfdGetFluidUsingName(theFluidUserName);
    if isempty(theFluid)
        theFluidIndex = theNumberOfFluids+1;
    else
        error('\n%s\n', 'the fluid is already defined, overriding fluid with new properties')
        theFluidIndex = cfdGetFluidIndex(theFluidUserName);
    end
end

if(theFluidIndex>0) 
    theFluid.userName = theFluidUserName;
    theFluid.name = theFluidUserName;
    theFluid.index = theFluidIndex;
    theFluid.tag = ['_fluid0' num2str(theFluidIndex)];
    theFluid.MW = 56;
    theFluid.thermalExpansionRatio = 0.0001;
    theFluid.mode = 'Incompressible'; % Compressible or Incompressible
    theFluid.type = 'Continuous'; % Disperse or Continuous
    theFluid.diameter = ' '; 
    theFluid.species = {};
    theFluid.viscousModel = 'laminar'; % laminar, k-eps etc

    theNumberOfSubTerms = length(varargin);

    for iSubTerm = 1:2:theNumberOfSubTerms
       theFluid =  setfield(theFluid,varargin{iSubTerm},varargin{iSubTerm+1});
    end

    Region.fluids{theFluid.index} = theFluid;
end

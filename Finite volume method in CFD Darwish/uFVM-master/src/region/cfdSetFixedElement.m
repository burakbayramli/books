function cfdSetFixedElement(varargin)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function fixes the pressure in case of closed walls (cavity)
%--------------------------------------------------------------------------

global Region;

if nargin==0
    algorithm = cfdGetAlgorithm;
    iElement = Region.foamDictionary.fvSolution.(algorithm).pRefCell;
else
    iElement = varargin{1};
end

Region.iFixedElement = iElement;

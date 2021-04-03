function theVector = cfdVector(varargin)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function initializes or copies a cfdVector
%--------------------------------------------------------------------------

if isempty(varargin)
    theVector = zeros(1,3);
elseif length(varargin)==1
    theVector = varargin{1};
elseif length(varargin)==3
    theVector = [varargin{1}, varargin{2}, varargin{3}];  
else
    error('Bad definition of cfdVector');
end

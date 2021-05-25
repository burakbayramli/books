function t = cfdTensor(varargin)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function initializes a cfdTensor
%--------------------------------------------------------------------------

if isempty(varargin)
    t = zeros(3,3);
elseif length(varargin)==3
    t = zeros(3,3);
    t(1,1) = varargin{1};
    t(2,2) = varargin{2};
    t(3,3) = varargin{3};
elseif length(varargin)==9
    t = zeros(3,3);
    
    t(1,1) = varargin{1};
    t(1,2) = varargin{2};
    t(1,3) = varargin{3};    
    
    t(2,1) = varargin{4};
    t(2,2) = varargin{5};
    t(2,3) = varargin{6};    
    
    t(3,1) = varargin{7};
    t(3,2) = varargin{8};
    t(3,3) = varargin{9};        
else
    error('Incorrect number of arguments for defining a cfdTensor\n');
end
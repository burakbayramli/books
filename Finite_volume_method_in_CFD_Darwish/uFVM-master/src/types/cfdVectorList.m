function vectorList = cfdVectorList(varargin)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function initializes a Vector list
%--------------------------------------------------------------------------

if isempty(varargin)
    vectorList = [];
elseif length(varargin)==1
    n = varargin{1};
    %
    vectorList = zeros(n, 3);
elseif length(varargin)==2
    n     = varargin{1};
    value = varargin{2}; 
    %
    vectorList = zeros(n, 3);
    for i=1:n
        vectorList(i,:) = value;
    end
end
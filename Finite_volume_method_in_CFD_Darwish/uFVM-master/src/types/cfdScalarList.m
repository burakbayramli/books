function theScalarList = cfdScalarList(varargin)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function initializes a scalar list
%--------------------------------------------------------------------------

if isempty(varargin)
    theScalarList = [];
elseif length(varargin)==1
    n = varargin{1};
    %
    theScalarList = zeros(n, 1);
    for i=1:n
        theScalarList(i) = 0;
    end
elseif length(varargin)==2
    n    = varargin{1};
    value = varargin{2};    
    %
    theScalarList = zeros(n, 1);
    for i=1:n
        theScalarList(i) = value;
    end
end
function labelList = cfdLabelList(varargin)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function initializes a label list
%--------------------------------------------------------------------------

if isempty(varargin)
    labelList = [];
elseif length(varargin)==1
    n = varargin{1};
    %
    labelList = zeros(n, 1);
    for i=1:n
        labelList(i) = 0;
    end
elseif length(varargin)==2
    n     = varargin{1};
    value = varargin{2}; 
    %
    labelList = zeros(n, 1);
    for i=1:n
        labelList(i) = value;
    end
end
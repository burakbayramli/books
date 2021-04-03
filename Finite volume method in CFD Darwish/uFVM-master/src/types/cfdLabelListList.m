function thecfdLabelListList = cfdLabelListList(varargin)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function initializes a label list
%--------------------------------------------------------------------------

if isempty(varargin)
    thecfdLabelListList = cell(0);
elseif length(varargin)==1
    n = varargin{1};
    %
    thecfdLabelListList = cell(n, 1);
    for i=1:n
        thecfdLabelListList{i} = [];
    end
elseif length(varargin)==2
    n     = varargin{1};
    value = varargin{2}; 
    %
    thecfdLabelListList = cell(n, 1);
    for i=1:n
        thecfdLabelListList{i} = value;
    end
end
function tr = cfdTrace(theTensorList)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function returns the cfdTrace of a cfdTensor list
%--------------------------------------------------------------------------

tr = zeros(size(theTensorList,1),1);
for i=1:size(theTensorList,1)
    tr(i) = theTensorList(i,1,1) + theTensorList(i,2,2) + theTensorList(i,3,3);
end
function theTransposedTensorList = cfdTransp(theTensorList)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function returns the cfdTranspose of a cfdTensor list
%--------------------------------------------------------------------------

if size(theTensorList,3)==1
    theTransposedTensorList = theTensorList';
    return;
end

theTransposedTensorList = zeros(size(theTensorList));
for i=1:size(theTensorList,1)
    theTransposedTensorList(i,1,:) = theTensorList(i,:,1);
    theTransposedTensorList(i,2,:) = theTensorList(i,:,2);
    theTransposedTensorList(i,3,:) = theTensorList(i,:,3);
end
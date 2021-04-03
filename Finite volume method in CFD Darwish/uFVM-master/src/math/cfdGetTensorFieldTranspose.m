function theTensorFieldTransp = cfdGetTensorFieldTranspose(theTensorField)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018 
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function returns the cfdTranspose of a cfdTensor field
%--------------------------------------------------------------------------

theSize = size(theTensorField, 1);
theTensorFieldTransp = zeros(size(theTensorField));

for iElement=1:theSize
    theTensorFieldTransp(iElement,1,:) = theTensorField(iElement,:,1);
    theTensorFieldTransp(iElement,2,:) = theTensorField(iElement,:,2);
    theTensorFieldTransp(iElement,3,:) = theTensorField(iElement,:,3);
end
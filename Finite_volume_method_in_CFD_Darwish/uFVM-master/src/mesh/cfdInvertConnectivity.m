function theInvertedConnectivityArray = cfdInvertConnectivity(theConnectivityArray)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function invertes the topology of the connectivity array
%--------------------------------------------------------------------------

theIvertedSize = 0;
for i=1:size(theConnectivityArray, 1)        
    for j=1:length(theConnectivityArray{i})
        theIvertedSize = max(theIvertedSize, theConnectivityArray{i}(j));
    end
end

theInvertedConnectivityArray = cell(theIvertedSize,1);
for i=1:length(theConnectivityArray)      
    for j=1:length(theConnectivityArray{i})
        theInvertedConnectivityArray{theConnectivityArray{i}(j)}(end+1) = i;
    end
end

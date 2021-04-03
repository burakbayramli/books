function F = cfdConvertCelltoMatrix(f)
%--------------------------------------------------------------------------
%
%  written by the CFD Group @ AUB, 2018 
%  contact us at: cfd@aub.edu.lb
%==========================================================================
% Case Description:
%     This function converts cell to matrix
%--------------------------------------------------------------------------

maxLength = 1;

for i=1:length(f)
    maxLength = max(maxLength,length(f{i}));
end

F = zeros(length(f),maxLength);

for i=1:length(f)
   if length(f{i})<maxLength
       F(i,1:length(f{i})) = f{i};
       F(i,length(f{i})+1:end) = f{i}(end);
   else
       F(i,:) = f{i};
   end       
end
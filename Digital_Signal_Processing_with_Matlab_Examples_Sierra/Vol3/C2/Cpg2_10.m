%Load and visualize HB nnc1374
%(note: earlier MATLAB versions cannot handle the large matrix)
%
S=load('nnc1374.mat'); %file (structure) loading
fieldnames(S) %to see structure field names
getfield(S,'Problem') %to see inside the field 'Problem'
M=getfield(S,'Problem','A'); %get the sparse matrix from 'A'

figure(1)
spy(M,'k');
title('the HB nnc1374 matrix');


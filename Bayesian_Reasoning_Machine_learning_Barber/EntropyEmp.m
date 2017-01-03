function entropy=EntropyEmp(data,nstates)
%ENTROPYEMP Compute the entropy of the empirical distribution
% entropy=EntropyEmp(data,nstates)
% data is a matrix that contains the sample states on each row
% with the number of their states in nstates
px=reshape(normp(count(data,nstates)),nstates); % empirical distribution
x = 1:length(nstates);
emppot.variables=x; emppot.table=px; 
entropy = -table(sumpot(multpots([emppot logpot(emppot)]),[],0));
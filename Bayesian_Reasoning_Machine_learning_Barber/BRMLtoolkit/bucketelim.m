function bucket=bucketelim(pot,ord)
%BUCKETELIM Bucket Elimiation on a set of potentials
% bucket=bucketelim(pot,ord)
%
% Bucket Elimination: eliminate variables in order ord
% bucket(end) contains the marginal of the ord(end) variable
variables=potvariables(pot); % get info about potentials
n=length(variables); % number of variables
% initialise the buckets:
merged=[]; for b=1:n; bucket(b).variables=[];  bucket(b).table=1; end
for b=1:n
    merge = whichpot(pot,ord(b)); % which potentials contain the bucket variables
    tomerge = setdiff(merge,merged); % only merge those potentials that are left
    if ~isempty(tomerge); 
        bucket(b)=multpots([bucket(b) pot(tomerge)]); % merge the potentials
    end
    merged=[merged tomerge]; % add to set of merged potentials
end
% do the bucket elimination:
for b=1:n-1
    mess = sumpot(bucket(b),ord(b)); % sum over the variable associated with the bucket
    inds = find(ismember(ord,mess.variables)); % find possible buckets
    newb = inds(1); % choose first bucket in order
    bucket(newb)=multpots([bucket(newb) mess]);	% multiply the new bucket with the summed bucket
end
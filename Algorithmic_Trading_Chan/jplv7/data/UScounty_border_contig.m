function cbcontig=UScounty_border_contig(fipslist)
% For US counties:
%   Creates a (sparse) spatial contiguity matrix based on whether or not
%   the counties shared a common border. 
%
% Input: fipslist - is the column vector (nx1) of the 5 digit fips number 
%                   for US counties of interest. 
% Output: returns a nxn contiguity matrix whose elements are:
%       1 if county i shares share a border with county j; and
%       0 if counties i and j do not share a border.

% The data in the border-contiguity dataset are arranged as follows:
%   1st column: 5-digit fips code for the county of reference
%   2nd column: the number of counties that are border-contiguous to the reference county (0-13)
%   3rd to 15th column: contains the fips code of the border-contiguous counties

% Daniel C. Monchuk - January 2006


% load text data with border-contiguity relationships
load UScbcontig.txt;
fips=UScbcontig(:,1);
contigfips=UScbcontig(:,3:15);

X=[fips contigfips];
k=size(X,2);
nobs=size(fipslist,1);
fipslistcontig=zeros(nobs,k);

% keep the contiguity relationships of only those counties of interest

for i=1:nobs;
    fipslisti=fipslist(i,1);
    ind(i,1)=find(fipslisti==fips);
    fipslistcontig(i,:)=X(ind(i,1),:);
end

% create a dataset to pick out and match the sorted fips into the proper
%   place in the weight matrix.

W=zeros(nobs); % empty weights matrix for contriguit relationships;

for i=1:nobs;
for j=2:14;
    cbfipsj=fipslistcontig(i,j);  
    if cbfipsj>0; % cbfips >0 if sharing a border, fips number is the element.
        % this finds the corresponding row number that corresponds to
        % the fips number indicated by the contiguity relationship.
        [r,c]=find(fipslistcontig(:,1)==cbfipsj);
        W(i,r)=1;
end;
end;
end;

% store the matrix in sparse format;
cbcontig=sparse(W);

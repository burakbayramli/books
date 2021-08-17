function elimed = elim(full,NO,perm)
%elimed = elim(full,NO,perm)
%
%Removes NO from the vector 'full'. If NO is a vector, it will create a
%matrix where each row is absent an element of NO. Optionally able to
%select the orientation of the vector or matrix where 'perm' is a vector
%that would be typically passed to permute()
temp=repmat(full,length(NO),1);
elimed=flipud(reshape(temp(abs(bsxfun(@minus,full,NO'))>eps(temp)),[],length(full)-1));

if nargin==3
    elimed=permute(elimed,perm);
end

end

function mfunc = minmod(v)

% function mfunc = minmod(v)
% Purpose: Implement the midmod function v is a vector

m = size(v,1); mfunc = zeros(1,size(v,2));
s = sum(sign(v),1)/m;

ids = find(abs(s)==1);
if(~isempty(ids))
  mfunc(ids) = s(ids).*min(abs(v(:,ids)),[],1); 
end
return;

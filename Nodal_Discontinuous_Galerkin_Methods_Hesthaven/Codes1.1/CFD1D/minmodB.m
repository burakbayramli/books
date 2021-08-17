function mfunc = minmodB(v,M,h)

% function mfunc = minmodB(v,M,h)
% Purpose: Implement the TVB modified midmod function. v is a vector

mfunc = v(1,:);
ids = find(abs(mfunc) > M*h.^2);

if(size(ids,2)>0)
  mfunc(ids) = minmod(v(:,ids));
end
return

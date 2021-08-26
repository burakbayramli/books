function psi = minmodTVB(v,M,h)
% function psi = minmodTVB(v,M,h)
% Purpose: Implement the TVB modified midmod function on row vector v
psi = v(:,1); ids = find(abs(psi) > M*h.^2);
if(size(ids,1)>0)
  psi(ids) = minmod(v(ids,:));
end
return
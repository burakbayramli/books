function psi = maxmod(v)
% function psi = maxmod(v)
% Purpose: Implement the maxmod function on vector v
N = size(v,1); m = size(v,2); psi = zeros(N,1);
s = sum(sign(v),2)/m; ids = find(abs(s)==1);
if(~isempty(ids))
  psi(ids) = s(ids).*max(abs(v(ids,:)),[],2); 
end
return;
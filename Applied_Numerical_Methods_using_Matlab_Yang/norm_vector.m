function nv=norm_vector(v,p)
if nargin<2, p=2; end
nv= sum(abs(v).^p)^(1/p);
nv= max(abs(v));
if p>0&p~=inf
 elseif p==inf
end   

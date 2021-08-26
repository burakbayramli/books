function [c] = ReconstructWeights(m,r)
% Purpose: Compute weights c_ir for reconstruction 
%               v_{j+1/2} = \sum_{j=0}^{m-1} c_{ir} v_{i-r+j}
%     with m=order and r=shift (-1<=r<=m-1).
c = zeros(1,m); fh = @(s) (-1)^(s+m)*prod(1:s)*prod(1:(m-s));
for i=0:m-1
    q = linspace(i+1,m,m-i);
    for q=(i+1):m
        if (q~=r+1)
           c(i+1) = c(i+1) + fh(r+1)/fh(q)/(r+1-q); 
        else
           c(i+1) = c(i+1) - (harmonic(m-r-1)-harmonic(r+1));
        end
    end
end                   
return
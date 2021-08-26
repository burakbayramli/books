function [Vr] = GradVandermondeDG(m,r)
% function [Vr] = GradVandermondeDG(m,r)
% Purpose : Initialize the gradient of the Vandermonde matrix 
% of order m at (r)
Vr = zeros(length(r),(m+1));
for i=0:m
   [Vr(:,i+1)] = GradLegendreP(r(:),i);
end
return
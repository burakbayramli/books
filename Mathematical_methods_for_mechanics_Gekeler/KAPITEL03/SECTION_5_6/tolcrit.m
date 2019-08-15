function [PHI1,DIFER] = tolcrit(V,PHI0)
n = size(V,1);
    CENTER = sum(V(:,1:n+1),2)/(n+1);
    %CENTER = (sum(V(:,1:n),2) - V(:,n+1))/n;
DIFER = 0;
for I = 1:n+1
   DIFER = DIFER + norm(V(:,I) - CENTER);
end
DIFER = DIFER/n;
PHI1 = min(PHI0,DIFER);

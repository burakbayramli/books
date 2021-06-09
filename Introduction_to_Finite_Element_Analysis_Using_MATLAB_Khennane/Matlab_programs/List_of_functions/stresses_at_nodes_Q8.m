function[ZX, ZY, ZT, Z1, Z2]=Stresses_at_nodes_Q8(SIGMA)
%
% This function averages the stresses at the nodes 
%
global nnd nel nne  geom  connec  
%
for k = 1:nnd
    sigx = 0. ;sigy = 0.; tau = 0.;
    ne = 0;  
    for iel = 1:nel;
        for jel=1:nne;
            if connec(iel,jel) == k;
                ne=ne+1;
                sigx = sigx+SIGMA(iel,1);
                sigy = sigy + SIGMA(iel,2);
                tau = tau + SIGMA(iel,3);
            end
        end
    end
         ZX(k,1) = sigx/ne;
         ZY(k,1) = sigy/ne;
         ZT(k,1)=tau/ne;
         Z1(k,1)= ((sigx+sigy)/2 + sqrt(((sigx+sigy)/2)^2 +tau^2))/ne;
         Z2(k,1)= ((sigx+sigy)/2 - sqrt(((sigx+sigy)/2)^2 +tau^2))/ne;
end
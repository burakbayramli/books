function[ZX, ZY, Z_THETA, ZT]=Stresses_at_nodes_axi(SIGMA)
%
% This function averages the stresses at the nodes 
%
global nnd nel nne  geom  connec  
%
for k = 1:nnd
    sigx = 0. ;sigy = 0.; sig_theta = 0.; tau = 0.;
    ne = 0;  
    for iel = 1:nel;
        for jel=1:nne;
            if connec(iel,jel) == k;
                ne=ne+1;
                sigx = sigx+SIGMA(iel,1);
                sigy = sigy + SIGMA(iel,2);
                sig_theta = sig_theta + SIGMA(iel,3);
                tau = tau + SIGMA(iel,4);
            end
        end
    end
         ZX(k,1) = sigx/ne;
         ZY(k,1) = sigy/ne;
         ZT(k,1)=tau/ne;
         Z_THETA(k,1) = sig_theta/ne;
end
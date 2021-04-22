function[ZX, ZY, ZT, Z1, Z2]=prepare_contour_data(SIGMA)
%
% This function averages the stresses at the nodes 
% and rearrange the values in the matrix Z for contouring
%
global nnd nel nne  geom  connec XIG YIG NXE NYE 
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
    xc = geom(k,1); yc = geom(k,2);
    for i = 1:2*NXE+1;
      for j=1:2*NYE +1;
        if xc == XIG(i) && yc == YIG(j);
         ZX(j,i) = sigx/ne;
         ZY(j,i) = sigy/ne;
         ZT(j,i)=tau/ne;
         Z1(j,i)= ((sigx+sigy)/2 + sqrt(((sigx+sigy)/2)^2 +tau^2))/ne;
         Z2(j,i)= ((sigx+sigy)/2 - sqrt(((sigx+sigy)/2)^2 +tau^2))/ne;
        end
      end
    end
end
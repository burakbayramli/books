function [dN] = f_dN_2D_v(csi,eta)

% 1st derivatives of shape functions for velocity
dN(1).dN=+[eta*(csi/4-1/4)*(eta-1)+(csi*eta*(eta-1))/4;csi*eta*(csi/4-1/4)+csi*(csi/4-1/4)*(eta-1)];
dN(2).dN=-[-eta*(csi/4+1/4)*(eta-1)-(csi*eta*(eta-1))/4;-csi*eta*(csi/4+1/4)-csi*(csi/4+1/4)*(eta-1)];
dN(3).dN=+[eta*(csi/4+1/4)*(eta+1)+(csi*eta*(eta+1))/4;csi*eta*(csi/4+1/4)+csi*(csi/4+1/4)*(eta+1)];
dN(4).dN=-[-eta*(csi/4-1/4)*(eta+1)-(csi*eta*(eta+1))/4;-csi*eta*(csi/4-1/4)-csi*(csi/4-1/4)*(eta+1)];
dN(5).dN=-[2*csi*eta*(eta/2-1/2);(csi^2-1)*(eta/2-1/2)+(eta*(csi^2-1))/2];
dN(6).dN=+[-(csi/2+1/2)*(eta^2-1)-(csi*(eta^2-1))/2;-2*csi*eta*(csi/2+1/2)];
dN(7).dN=+[-2*csi*eta*(eta/2+1/2);-(csi^2-1)*(eta/2+1/2)-(eta*(csi^2-1))/2];
dN(8).dN=-[(csi/2-1/2)*(eta^2-1)+(csi*(eta^2-1))/2;2*csi*eta*(csi/2-1/2)];
dN(9).dN=+[2*csi*(eta^2-1);2*eta*(csi^2-1)];

end
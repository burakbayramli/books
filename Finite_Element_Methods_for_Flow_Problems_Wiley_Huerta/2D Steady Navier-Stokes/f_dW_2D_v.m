function [dW] = f_dW_2D_v(csi,eta)

% 1st derivatives of test functions for velocity
dW(1).dW=+[eta*(csi/4-1/4)*(eta-1)+(csi*eta*(eta-1))/4;csi*eta*(csi/4-1/4)+csi*(csi/4-1/4)*(eta-1)];
dW(2).dW=-[-eta*(csi/4+1/4)*(eta-1)-(csi*eta*(eta-1))/4;-csi*eta*(csi/4+1/4)-csi*(csi/4+1/4)*(eta-1)];
dW(3).dW=+[eta*(csi/4+1/4)*(eta+1)+(csi*eta*(eta+1))/4;csi*eta*(csi/4+1/4)+csi*(csi/4+1/4)*(eta+1)];
dW(4).dW=-[-eta*(csi/4-1/4)*(eta+1)-(csi*eta*(eta+1))/4;-csi*eta*(csi/4-1/4)-csi*(csi/4-1/4)*(eta+1)];
dW(5).dW=-[2*csi*eta*(eta/2-1/2);(csi^2-1)*(eta/2-1/2)+(eta*(csi^2-1))/2];
dW(6).dW=+[-(csi/2+1/2)*(eta^2-1)-(csi*(eta^2-1))/2;-2*csi*eta*(csi/2+1/2)];
dW(7).dW=+[-2*csi*eta*(eta/2+1/2);-(csi^2-1)*(eta/2+1/2)-(eta*(csi^2-1))/2];
dW(8).dW=-[(csi/2-1/2)*(eta^2-1)+(csi*(eta^2-1))/2;2*csi*eta*(csi/2-1/2)];
dW(9).dW=+[2*csi*(eta^2-1);2*eta*(csi^2-1)];

end
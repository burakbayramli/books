function [N] = f_N_2D_v(csi,eta)

% Shape functions for velocity
N(1).N=+1/4*(1-csi)*(1-eta)*csi*eta;
N(2).N=-1/4*(1+csi)*(1-eta)*csi*eta;
N(3).N=+1/4*(1+csi)*(1+eta)*csi*eta;
N(4).N=-1/4*(1-csi)*(1+eta)*csi*eta;
N(5).N=-1/2*(1-eta)*eta*(1-csi^2);
N(6).N=+1/2*(1+csi)*csi*(1-eta^2);
N(7).N=+1/2*(1+eta)*eta*(1-csi^2);
N(8).N=-1/2*(1-csi)*csi*(1-eta^2);
N(9).N=+1*(1-csi^2)*(1-eta^2);

end
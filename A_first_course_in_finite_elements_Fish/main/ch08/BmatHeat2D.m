% B matrix function
 
function [B, detJ] = BmatHeat2D(eta,psi,C)
        
      % Calculate the Grad(N) matrix
        GN    = 0.25 * [eta-1  1-eta   1+eta   -eta-1;
                        psi-1  -psi-1  1+psi    1-psi];

        J     = GN*C;       % compute Jacobian matrix 
        detJ  = det(J);     % Jacobian
      
        B     = J\GN;       % compute the B matrix
   
